# Global softlink regional demand maps ---------------------------------------

library(dplyr)
library(stringr)
library(tidyr)
library(tibble)
library(sf)
library(ggplot2)
library(gdxrrw)
library(rnaturalearth)
library(rnaturalearthdata)

sf_use_s2(FALSE)

for (gams_sys_dir in c("C:/GAMS/38", "C:/GAMS/win64/26.1")) {
  if (dir.exists(gams_sys_dir)) {
    igdx(gams_sys_dir)
    break
  }
}

aimpsy <- "//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output"
scenario_group <- "global_softlink"
region <- "global"
date <- "2604281658"
plot_year <- 2050
scenarios <- c("300C", "700C")
target_vars <- c("Sec_Ene_Hyd", "Sec_Ene_Hyd_Amm")

softlink_dir <- if (basename(normalizePath(getwd(), winslash = "/", mustWork = FALSE)) == "softlink") {
  "."
} else {
  "softlink"
}

main_dir <- file.path(aimpsy, scenario_group, region, date, "main")
gdx_path <- file.path(main_dir, "merged_output.gdx")
out_dir <- file.path(softlink_dir, "output", "global_softlink", "global_12day", "plot_demand_map")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

build_region_map <- function() {
  tribble(
    ~iso3, ~region,
    "JPN", "JPN", "CHN", "CHN", "IND", "IND", "IDN", "IDN",
    "KOR", "KOR", "THA", "THA", "MYS", "MYS", "VNM", "VNM",
    "BRN", "XSE", "KHM", "XSE", "LAO", "XSE", "MMR", "XSE",
    "PHL", "XSE", "SGP", "XSE", "TLS", "XSE",
    "AFG", "XSA", "BGD", "XSA", "BTN", "XSA", "LKA", "XSA",
    "MDV", "XSA", "NPL", "XSA", "PAK", "XSA",
    "MNG", "XEA", "PRK", "XEA", "TWN", "XEA",
    "KAZ", "XCS", "KGZ", "XCS", "TJK", "XCS", "TKM", "XCS",
    "UZB", "XCS",
    "ARE", "XME", "BHR", "XME", "IRN", "XME", "IRQ", "XME",
    "ISR", "XME", "JOR", "XME", "KWT", "XME", "LBN", "XME",
    "OMN", "XME", "QAT", "XME", "SAU", "XME", "SYR", "XME",
    "YEM", "XME",
    "AUS", "AUS", "NZL", "XOC", "PNG", "XOC", "FJI", "XOC",
    "SLB", "XOC", "VUT", "XOC", "WSM", "XOC",
    "CAN", "CAN", "USA", "USA", "MEX", "MEX",
    "BRA", "BRA", "ARG", "XLM", "BOL", "XLM", "CHL", "XLM",
    "COL", "XLM", "CRI", "XLM", "CUB", "XLM", "DOM", "XLM",
    "ECU", "XLM", "GTM", "XLM", "HND", "XLM", "HTI", "XLM",
    "JAM", "XLM", "NIC", "XLM", "PAN", "XLM", "PER", "XLM",
    "PRY", "XLM", "SLV", "XLM", "URY", "XLM", "VEN", "XLM",
    "GUY", "XLM", "SUR", "XLM", "TTO", "XLM", "BHS", "XLM",
    "BRB", "XLM", "BLZ", "XLM", "GUF", "XLM",
    "TUR", "TUR", "RUS", "RUS",
    "AUT", "XE15", "BEL", "XE15", "DNK", "XE15", "FIN", "XE15",
    "FRA", "XE15", "DEU", "XE15", "GRC", "XE15", "IRL", "XE15",
    "ITA", "XE15", "LUX", "XE15", "NLD", "XE15", "PRT", "XE15",
    "ESP", "XE15", "SWE", "XE15", "GBR", "XE15",
    "BGR", "XE3", "CYP", "XE10", "CZE", "XE10", "EST", "XE10",
    "HUN", "XE10", "LVA", "XE10", "LTU", "XE10", "MLT", "XE10",
    "POL", "XE10", "ROU", "XE3", "SVK", "XE10", "SVN", "XE10",
    "CHE", "XEWI", "NOR", "XEWI", "ISL", "XEWI",
    "ALB", "XEEI", "BIH", "XEEI", "HRV", "XEEI", "MNE", "XEEI",
    "MKD", "XEEI", "SRB", "XEEI", "KOS", "XEEI", "UKR", "XEEI", "BLR", "XEEI",
    "MDA", "XEEI", "ARM", "XEEI", "AZE", "XEEI", "GEO", "XEEI",
    "DZA", "XENI", "EGY", "XENI", "LBY", "XENI", "MAR", "XENI",
    "TUN", "XENI", "ESH", "XENI",
    "ZAF", "ZAF",
    "AGO", "XAF", "BEN", "XAF", "BWA", "XAF", "BFA", "XAF",
    "BDI", "XAF", "CMR", "XAF", "CAF", "XAF", "TCD", "XAF",
    "COD", "XAF", "COG", "XAF", "CIV", "XAF", "DJI", "XAF",
    "ERI", "XAF", "ETH", "XAF", "GAB", "XAF", "GHA", "XAF",
    "GIN", "XAF", "GMB", "XAF", "GNB", "XAF", "GNQ", "XAF", "KEN", "XAF",
    "LBR", "XAF", "LSO", "XAF", "MDG", "XAF", "MWI", "XAF",
    "MLI", "XAF", "MRT", "XAF", "MUS", "XAF", "MOZ", "XAF", "NAM", "XAF",
    "NER", "XAF", "NGA", "XAF", "RWA", "XAF", "SEN", "XAF",
    "SLE", "XAF", "SOM", "XAF", "SSD", "XAF", "SDN", "XAF",
    "SWZ", "XAF", "TZA", "XAF", "TGO", "XAF", "UGA", "XAF",
    "ZMB", "XAF", "ZWE", "XAF",
    "GRL", "XNF"
  )
}

build_region_boundaries <- function() {
  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) %>%
    st_make_valid() %>%
    mutate(iso3 = if_else(iso_a3 == "-99", adm0_a3, iso_a3))

  region_map <- build_region_map()

  world %>%
    left_join(region_map, by = "iso3") %>%
    mutate(region = if_else(is.na(region), "XNF", region)) %>%
    st_set_crs(NA) %>%
    group_by(region) %>%
    summarise(geometry = st_union(geometry), .groups = "drop") %>%
    st_make_valid() %>%
    st_set_crs(4326)
}

normalize_region_code <- function(region_code, region_map) {
  r31_regions <- unique(region_map$region)

  tibble(Region_raw = region_code) %>%
    mutate(
      Region_clean = str_remove(Region_raw, "^R31-"),
      iso3 = str_extract(Region_clean, "^[A-Z]{3}")
    ) %>%
    left_join(region_map, by = "iso3") %>%
    transmute(
      Region = case_when(
        Region_clean %in% r31_regions ~ Region_clean,
        !is.na(region) ~ region,
        TRUE ~ Region_clean
      )
    ) %>%
    pull(Region)
}

read_demand_values <- function(gdx_path, region_map) {
  if (!file.exists(gdx_path)) {
    stop("Missing GDX file: ", gdx_path)
  }

  rgdx.param(gdx_path, "iamc_gdx") %>%
    transmute(
      Scenario = as.character(Sc),
      Region = normalize_region_code(as.character(Sr), region_map),
      Variable = as.character(Sv),
      Year = as.numeric(as.character(Y5)),
      Value = suppressWarnings(as.numeric(as.character(iamc_gdx)))
    ) %>%
    filter(
      Scenario %in% scenarios,
      Variable %in% target_vars,
      Year == plot_year,
      !is.na(Value)
    ) %>%
    group_by(Scenario, Region, Variable, Year) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")
}

plot_demand_map <- function(map_data, scenario_name, variable_name) {
  plot_data <- map_data %>%
    filter(Scenario == scenario_name, Variable == variable_name)

  ggplot(plot_data) +
    geom_sf(aes(fill = Value), color = "grey35", linewidth = 0.3) +
    scale_fill_gradient(
      low = "#f7fbff",
      high = "#08306b",
      na.value = "grey92",
      labels = scales::label_number(accuracy = 0.01),
      name = "Demand (EJ/yr)"
    ) +
    coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
    theme_void() +
    theme(
      legend.position = c(0.08, 0.32),
      legend.direction = "vertical",
      legend.background = element_rect(fill = "white", color = "grey85"),
      legend.key.width = unit(8, "mm"),
      legend.key.height = unit(28, "mm"),
      plot.title = element_text(hjust = 0.5, size = 12)
    ) +
    labs(title = paste0(scenario_name, " ", variable_name, " demand by R31 region (", plot_year, ")"))
}

region_map <- build_region_map()
region_boundaries <- build_region_boundaries()
demand_values <- read_demand_values(gdx_path, region_map)

missing_regions <- setdiff(demand_values$Region, region_boundaries$region)
if (length(missing_regions) > 0) {
  warning("Demand regions not found in map: ", paste(sort(missing_regions), collapse = ", "))
}

map_data <- region_boundaries %>%
  left_join(demand_values, by = c("region" = "Region"))

for (scenario_name in scenarios) {
  for (variable_name in target_vars) {
    p <- plot_demand_map(map_data, scenario_name, variable_name)
    print(p)

    ggsave(
      filename = file.path(
        out_dir,
        paste0("global_", scenario_name, "_", variable_name, "_demand_map_", plot_year, ".png")
      ),
      plot = p,
      width = 300,
      height = 130,
      units = "mm",
      dpi = 300,
      bg = "white"
    )
  }
}
