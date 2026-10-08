# Scope: two biomass maps only, 500C-H2 / 2050 / R31; updates requested PNGs.
# PSM primary: ../data/AIM-PSY/global_softlink_IntermReport/Global/2609121358/
# gams_output/gdx_primary/500C-H2/2050.gdx; pmax(ME,MK) TWh * .0036 -> EJ.
# AIMT primary/trade: //SY_3600_01/atfs02/Individual/hsakata/AIMEnduse/output/
# softlink_IntermReport/2609111525/gams_output/{gdx_primary/500C-H2/2050.gdx,
# gdx_merged/trade_merged.gdx}; emax PJ / 1000; trade_merged already EJ.
# Additional audit: PSM dmd(N,I,MT) * 6 * 3.6e-6 -> EJ for annual y00 bus;
# AIMT iamc_gdx(Sc,Sr,Sv,Y5) from the local main/merged_output.gdx, EJ/yr.
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(gdxrrw); library(sf)})
main_text <- readLines("main.R", warn = FALSE)
main_text <- sub("BUILD_FIGURES <- TRUE", "BUILD_FIGURES <- FALSE", main_text, fixed = TRUE)
main_text <- sub("script_args <- commandArgs(trailingOnly = FALSE)", "script_args <- character()", main_text, fixed = TRUE)
eval(parse(text = main_text))
invisible(lapply(c("fig3/fig3_bio.R", "fig3/fig3_bio_AIMtech.R"), function(x) parse(file = x)))
igdx(GAMS_SYSTEM_DIR)
gdx_path <- file.path(psm_primary_dir, fig3_model_scenario, "2050.gdx")
all_uels <- rgdx(gdx_path)$uels
link_uels <- grep("^R31-[A-Z0-9]+-(CRN|CRNTRD|BMS|BMSTRD)_R31-[A-Z0-9]+-(CRN|CRNTRD|BMS|BMSTRD)$", all_uels, value = TRUE)
read_trade <- function(symbol) {
  raw <- rgdx(gdx_path, list(name = symbol, uels = list(link_uels, all_uels), compress = FALSE))
  tibble(Scenario = fig3_model_scenario, Link = raw$uels[[1]][raw$val[,1]],
         Value = raw$val[,3] * primary_time_weight_from_timeresol(time_resol) * 3.6e-6) %>%
    group_by(Scenario, Link) %>% summarise(Value = sum(Value), .groups = "drop")
}
raw_limits <- rgdx.param(gdx_path, "pmax", squeeze = FALSE)
limits <- raw_limits %>% transmute(Scenario = fig3_model_scenario,
  Region = as.character(ME), Resource = as.character(MK), Value = as.numeric(pmax)*0.0036) %>%
  filter(grepl("^R_CRN(AGR|FRR|MAN|MSW|1ST|P0[1-8])$", Resource) | Resource == "CRNALL")
primary <- list(psm_vfs_l = tibble(), psm_vff_l = read_trade("vff_l"),
  psm_vfb_l = read_trade("vfb_l"), psm_biomass_limits = limits,
  psm_region_map = tibble(), metadata = list(year = 2050, flow_unit = "EJ/yr"))
data <- list(aimt_iamc = tibble(), psm_iamc = tibble(), variable_specs = list(), metadata = list())
geodata <- load_fig3_geodata(file.path(project_dir, "data"))
aimt <- load_aimt_biomass_data(
  file.path(AIMT_BIOMASS_RUN_DIR,"gams_output","gdx_primary",fig3_model_scenario,"2050.gdx"),
  file.path(AIMT_BIOMASS_RUN_DIR,"gams_output","gdx_merged","trade_merged.gdx"),
  fig3_model_scenario, 2050, GAMS_SYSTEM_DIR)
audit_dir <- file.path(output_dir, "fig3", "biomass_resource_audit_2609121358")
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
for (model in c("PSM", "AIMtech")) {
  p <- if (model == "PSM") make_fig3_bio(data, primary, geodata, fig3_model_scenario,
                                         node_threshold = 0) else
    make_fig3_bio_AIMtech(aimt, geodata, fig3_model_scenario, node_threshold = 0)
  prefix <- if (model == "PSM") "fig3_bio" else "fig3_bio_AIMtech"
  totals <- attr(p, paste0(prefix, "_totals"))
  nodes <- attr(p, paste0(prefix, "_nodes"))
  check <- nodes %>% group_by(Region) %>% summarise(SplitTotal = sum(Value), .groups = "drop") %>%
    left_join(totals, by = "Region")
  stopifnot(nrow(totals) == 31L, all(abs(check$SplitTotal-check$Total) < 1e-10),
            setequal(nodes$Category, c("Energy crops", "Other biomass")))
  print(nodes %>% group_by(Category) %>% summarise(EJ = sum(Value), .groups = "drop"))
  print(totals %>% filter(Region == "JPN"))
  write.csv(nodes, file.path(audit_dir, paste0(model, "_resource_split.csv")), row.names = FALSE)
  # Keep the normal map's regional display threshold.
  p <- if (model == "PSM") make_fig3_bio(data, primary, geodata, fig3_model_scenario) else
    make_fig3_bio_AIMtech(aimt, geodata, fig3_model_scenario)
  invisible(ggplot_build(p))
  save_png_mm(p, file.path(output_dir, "fig3", paste0(prefix, ".png")), 300, 135)
}
# Extract only biomass demand, leaving large hourly flow symbols unread.
raw_dmd <- gdxrrw::rgdx.param(gdx_path, "dmd") %>%
  filter(as.character(I) %in% c("CRN", "BMS"))
stopifnot(all(as.character(raw_dmd$MT) == "y00"))
demand <- raw_dmd %>% transmute(Region = sub("^R31-", "", as.character(N)),
                               Carrier = as.character(I), Value = dmd * 6 * 3.6e-6) %>%
  group_by(Region, Carrier) %>% summarise(EJ = sum(Value), .groups = "drop")
print(demand %>% group_by(Carrier) %>% summarise(EJ = sum(EJ)))
stopifnot(nrow(demand) == 62L, !anyNA(demand$Region))
write.csv(demand, file.path(audit_dir, "PSM_biomass_fixed_demand.csv"), row.names = FALSE)
iamc <- rgdx.param(file.path(aimt_main_dir,"merged_output.gdx"), "iamc_gdx") %>%
  filter(as.character(Sc) == fig3_model_scenario, as.character(Y5) == "2050",
         grepl("^(Prm_Ene_Bio|Fin_Ene_SolidsBio|Fin_Ene_Liq_Bio)", as.character(Sv)),
         as.character(Sr) %in% geodata$regions$Region) %>%
  transmute(Region = as.character(Sr), Variable = as.character(Sv), EJ = iamc_gdx)
print(iamc %>% group_by(Variable) %>% summarise(EJ = sum(EJ)), n = Inf)
write.csv(iamc, file.path(audit_dir, "AIMtech_biomass_iamc.csv"), row.names = FALSE)
cat("Resource split and audit complete.\n")
