# Calculate the 2050 capacity ratio using the data frames plotted in Fig. 2a/b.
# Run: Rscript IntermReport/code/analyze_transmission_solar_wind_ratio.R

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(gdxrrw)
  library(tibble)
  library(tidyr)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
code_dir <- normalizePath(dirname(sub("^--file=", "", script_arg[[1]])),
                          winslash = "/", mustWork = TRUE)
project_dir <- normalizePath(file.path(code_dir, ".."), winslash = "/", mustWork = TRUE)
source(file.path(code_dir, "data_import.R"))
source(file.path(code_dir, "plot_common.R"))
source(file.path(code_dir, "fig2", "fig2_a.R"))
source(file.path(code_dir, "fig2", "fig2_b.R"))

scenario_map <- c(ELE = "500C-ELE", H2 = "500C-H2", SYF = "500C-SYF")
psm_run_dir <- file.path(project_dir, "data", "AIM-PSY",
                         "global_softlink_IntermReport", "Global", "2609121358")
gams_system_dir <- "C:/GAMS/win64/26.1"
gdxrrw::igdx(gams_system_dir)

data <- list(
  aimt_iamc = tibble(),
  psm_iamc = read_iamc_gdx(file.path(psm_run_dir, "main"), "PSM"),
  variable_specs = list(),
  metadata = list(scenario_map = scenario_map)
)
primary_2050 <- load_psm_primary(
  psm_primary_dir = file.path(psm_run_dir, "gams_output", "gdx_primary"),
  scenario_map = scenario_map,
  year = 2050,
  time_resol = "6HOUR",
  gams_system_dir = gams_system_dir,
  region_map_path = file.path(project_dir, "data", "M_R272.csv")
)

fig2_a <- make_fig2_a(data)
fig2_b <- make_fig2_b(primary_2050)

solar_wind <- fig2_a$data %>%
  filter(.data$Year == 2050, .data$Technology %in% c("Solar PV", "Wind")) %>%
  transmute(ScenarioKey = as.character(.data$ScenarioKey),
            Technology = as.character(.data$Technology), CapacityTW = .data$Value) %>%
  pivot_wider(names_from = "Technology", values_from = "CapacityTW") %>%
  rename(SolarPV_TW = `Solar PV`, Wind_TW = Wind)

transmission <- attr(fig2_b, "fig2_b_data") %>%
  transmute(Scenario = .data$Scenario,
            ScenarioKey = as.character(.data$ScenarioKey),
            Transmission_TW = .data$CapacityTW)

result <- transmission %>%
  inner_join(solar_wind, by = "ScenarioKey") %>%
  mutate(SolarWind_TW = .data$SolarPV_TW + .data$Wind_TW,
         Transmission_per_SolarWind = .data$Transmission_TW / .data$SolarWind_TW,
         Transmission_percent = 100 * .data$Transmission_per_SolarWind) %>%
  select(ScenarioKey, Scenario, Transmission_TW, SolarPV_TW, Wind_TW,
         SolarWind_TW, Transmission_per_SolarWind, Transmission_percent)

if (nrow(result) != length(scenario_map) ||
    any(!is.finite(result$Transmission_per_SolarWind)) ||
    any(result$SolarWind_TW <= 0)) {
  stop("Missing scenario or invalid 2050 solar-plus-wind denominator.")
}

output_path <- file.path(project_dir, "output", "fig2",
                         "transmission_solar_wind_ratio_2050.csv")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
write.csv(result, output_path, row.names = FALSE)
print(result, width = Inf)
message("Saved: ", output_path)
