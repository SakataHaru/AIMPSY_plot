# Compare Fig. 2-c gross transport throughput with the carrier production
# balances used by Fig. 2-d. Run from any working directory with:
# Rscript IntermReport/code/analyze_carrier_transport_share.R

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(gdxrrw)
  library(tibble)
  library(tidyr)
})

script_args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_args[grepl("^--file=", script_args)])
code_dir <- normalizePath(dirname(script_path[[1]]), winslash = "/", mustWork = TRUE)
project_dir <- normalizePath(file.path(code_dir, ".."), winslash = "/", mustWork = TRUE)

source(file.path(code_dir, "data_import.R"))
source(file.path(code_dir, "plot_common.R"))
source(file.path(code_dir, "fig2", "fig2_c.R"))
source(file.path(code_dir, "fig2", "fig2_d.R"))

scenario_map <- c(ELE = "500C-ELE", H2 = "500C-H2", SYF = "500C-SYF")
aimt_main_dir <- file.path(
  project_dir, "data", "AIM-Tech", "softlink_IntermReport", "2609111525", "main"
)
psm_run_dir <- file.path(
  project_dir, "data", "AIM-PSY", "global_softlink_IntermReport", "Global", "2609121358"
)
psm_main_dir <- file.path(psm_run_dir, "main")
psm_primary_dir <- file.path(psm_run_dir, "gams_output", "gdx_primary")
region_map_path <- file.path(project_dir, "data", "M_R272.csv")
gams_system_dir <- "C:/GAMS/win64/26.1"

variable_specs <- load_variable_specs(file.path(code_dir, "dataframe.R"))
data <- import_model_data(
  aimt_main_dir = aimt_main_dir,
  psm_main_dir = psm_main_dir,
  psm_primary_dir = psm_primary_dir,
  scenario_map = scenario_map,
  variable_specs = variable_specs,
  gams_system_dir = gams_system_dir
)
primary_2050 <- load_psm_primary(
  psm_primary_dir = psm_primary_dir,
  scenario_map = scenario_map,
  year = 2050,
  time_resol = "6HOUR",
  gams_system_dir = gams_system_dir,
  region_map_path = region_map_path
)

fig2_c <- make_fig2_c(primary_2050)
fig2_d <- suppressWarnings(make_fig2_d(data, primary_2050))
transport <- attr(fig2_c, "fig2_c_data") %>%
  transmute(
    Scenario,
    ScenarioKey = as.character(.data$ScenarioKey),
    Carrier = as.character(.data$Carrier),
    GrossTransportEJ = .data$TransportEJ
  )

transport_scope <- attr(fig2_c, "fig2_c_link_data") %>%
  group_by(.data$Scenario, .data$Carrier, .data$R31Scope) %>%
  summarise(TransportEJ = sum(.data$Value), .groups = "drop") %>%
  tidyr::complete(
    Scenario = unname(scenario_map),
    Carrier = c("Hydrogen", "Ammonia", "Synfuel"),
    R31Scope = c("Across R31 regions", "Within R31 region", "Unmapped"),
    fill = list(TransportEJ = 0)
  ) %>%
  tidyr::pivot_wider(
    names_from = "R31Scope",
    values_from = "TransportEJ",
    values_fill = 0,
    names_prefix = "Transport_"
  ) %>%
  rename_with(~ gsub(" ", "_", .x, fixed = TRUE))

balance <- attr(fig2_d, "fig2_d_balance_audit") %>%
  mutate(ScenarioKey = as.character(.data$ScenarioKey))

# "Final carrier production" is the amount remaining as that carrier after
# modeled carrier-to-carrier conversions. For hydrogen this is specifically:
# electrolysis output - H2 used for NH3 - H2 used for synfuel + cracked H2.
production <- bind_rows(
  balance %>% transmute(
    Scenario, ScenarioKey, Carrier = "Hydrogen",
    GrossProductionEJ = .data$H2Production,
    FinalCarrierProductionEJ = .data$H2AvailableAfterConversion
  ),
  balance %>% transmute(
    Scenario, ScenarioKey, Carrier = "Ammonia",
    GrossProductionEJ = .data$H2ToNH3Output,
    FinalCarrierProductionEJ = .data$H2ToNH3Output - .data$NH3ToH2Input
  ),
  balance %>% transmute(
    Scenario, ScenarioKey, Carrier = "Synfuel",
    GrossProductionEJ = .data$H2ToSynOutput,
    FinalCarrierProductionEJ = .data$H2ToSynOutput
  )
)

safe_pct <- function(numerator, denominator) {
  ifelse(denominator > 1e-9, 100 * numerator / denominator, NA_real_)
}

result <- transport %>%
  left_join(transport_scope, by = c("Scenario", "Carrier")) %>%
  left_join(production, by = c("Scenario", "ScenarioKey", "Carrier")) %>%
  mutate(
    Transport_Across_R31_regions = coalesce(.data$Transport_Across_R31_regions, 0),
    Transport_Within_R31_region = coalesce(.data$Transport_Within_R31_region, 0),
    Transport_Unmapped = coalesce(.data$Transport_Unmapped, 0),
    ProductionBasis = if_else(
      .data$Carrier == "Hydrogen",
      "Gross H2 production before conversion",
      "Carrier production"
    ),
    ProductionForShareEJ = if_else(
      .data$Carrier == "Hydrogen",
      .data$GrossProductionEJ,
      .data$FinalCarrierProductionEJ
    ),
    GrossTransport_pct_of_Production = safe_pct(
      .data$GrossTransportEJ, .data$ProductionForShareEJ
    ),
    GrossTransport_pct_of_FinalProduction = safe_pct(
      .data$GrossTransportEJ, .data$FinalCarrierProductionEJ
    ),
    AcrossR31Transport_pct_of_FinalProduction = safe_pct(
      .data$Transport_Across_R31_regions, .data$FinalCarrierProductionEJ
    ),
    GrossTransport_pct_of_GrossProduction = safe_pct(
      .data$GrossTransportEJ, .data$GrossProductionEJ
    )
  ) %>%
  arrange(factor(.data$ScenarioKey, levels = names(scenario_map)),
          factor(.data$Carrier, levels = c("Hydrogen", "Ammonia", "Synfuel")))

output_path <- file.path(project_dir, "output", "fig2", "carrier_transport_share_2050.csv")
utils::write.csv(result, output_path, row.names = FALSE, na = "")

print(result, n = Inf, width = Inf)
message("Wrote: ", output_path)
