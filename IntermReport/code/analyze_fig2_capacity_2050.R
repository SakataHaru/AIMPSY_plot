# Extract the four 2050 installed capacities from the Fig. 2a plot data frame.
# Run: Rscript IntermReport/code/analyze_fig2_capacity_2050.R

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

scenario_map <- c(ELE = "500C-ELE", H2 = "500C-H2", SYF = "500C-SYF")
psm_main_dir <- file.path(
  project_dir, "data", "AIM-PSY", "global_softlink_IntermReport",
  "Global", "2609121358", "main"
)
gdxrrw::igdx("C:/GAMS/win64/26.1")
data <- list(
  aimt_iamc = tibble(),
  psm_iamc = read_iamc_gdx(psm_main_dir, "PSM"),
  variable_specs = list(),
  metadata = list(scenario_map = scenario_map)
)

capacity <- make_fig2_a(data)$data %>%
  filter(.data$Year == 2050) %>%
  transmute(
    ScenarioKey = as.character(.data$ScenarioKey),
    Technology = as.character(.data$Technology),
    CapacityTW = .data$Value
  ) %>%
  pivot_wider(names_from = "Technology", values_from = "CapacityTW") %>%
  rename(SolarPV_TW = `Solar PV`, Wind_TW = Wind,
         Battery_TW = Battery, Electrolyzer_TW = Electrolyzer) %>%
  mutate(Scenario = unname(scenario_map[.data$ScenarioKey]),
         .before = SolarPV_TW) %>%
  select(ScenarioKey, Scenario, SolarPV_TW, Wind_TW,
         Battery_TW, Electrolyzer_TW)

if (nrow(capacity) != length(scenario_map) ||
    any(!is.finite(as.matrix(capacity[, -(1:2)])))) {
  stop("Missing scenario or non-finite 2050 capacity in Fig. 2a data.")
}

output_path <- file.path(project_dir, "output", "fig2", "capacity_2050.csv")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
write.csv(capacity, output_path, row.names = FALSE)
print(capacity, width = Inf)
message("Saved: ", output_path)
