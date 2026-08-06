# PLOT SETTINGS ---------------------------------------------------------------

PSM_SCENARIO <- "global_softlink_Sc"
PSM_REGION <- "Global"
PSM_DATE <- "2608031707"

timeresol <- "6HOUR"
target_scenarios <- c("NPi500")
plot_year <- 2050

# PROJECT DISCOVERY AND PACKAGES ---------------------------------------------

script_args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_args[grepl("^--file=", script_args)])

script_dir <- if (length(script_path) > 0) {
  dirname(normalizePath(script_path[[1]], winslash = "/", mustWork = FALSE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

project_candidates <- unique(c(
  script_dir,
  file.path(script_dir, "master"),
  file.path(script_dir, "AIMPSY_plot", "master"),
  file.path(script_dir, ".."),
  file.path(script_dir, "..", "master"),
  file.path(script_dir, "..", "AIMPSY_plot", "master")
))
project_matches <- project_candidates[
  file.exists(file.path(project_candidates, "code", "dataframe.R"))
]
if (length(project_matches) == 0) {
  stop(
    "Could not locate the AIMPSY_plot/master project containing code/dataframe.R. Checked: ",
    paste(project_candidates, collapse = ", ")
  )
}
project_dir <- normalizePath(project_matches[[1]], winslash = "/", mustWork = TRUE)

old_wd <- getwd()
setwd(project_dir)
on.exit(setwd(old_wd), add = TRUE)

library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(purrr)
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

# DERIVED SETTINGS AND INPUT PATHS --------------------------------------------

trading_project_dir <- project_dir
trading_data_dir <- file.path(trading_project_dir, "data")
psm_output_root <- "//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output"

timeresol <- toupper(trimws(timeresol))
target_scenarios <- trimws(as.character(target_scenarios))
if (length(target_scenarios) == 0 || anyNA(target_scenarios) || any(!nzchar(target_scenarios))) {
  stop("target_scenarios must contain at least one non-blank scenario name.")
}
target_scenarios <- unique(target_scenarios)

primary_time_weight_from_timeresol <- function(timeresol) {
  timeresol <- toupper(trimws(timeresol))
  weights <- c(
    "12DAY" = ceiling(365 / 12 * 1000) / 1000,
    "6HOUR" = 6,
    "4HOUR" = 4,
    "3HOUR" = 3,
    "1HOUR" = 1
  )

  if (!timeresol %in% names(weights)) {
    stop(
      "Unsupported timeresol: ", timeresol,
      ". Supported values: ", paste(names(weights), collapse = ", ")
    )
  }

  unname(weights[[timeresol]])
}

trading_primary_time_weight <- primary_time_weight_from_timeresol(timeresol)
psm_run_dir <- file.path(psm_output_root, PSM_SCENARIO, PSM_REGION, PSM_DATE)
psm_primary_dir <- file.path(psm_run_dir, "gams_output", "gdx_primary")
psm_main_dir <- file.path(psm_run_dir, "main")
merged_gdx_path <- file.path(psm_main_dir, "merged_output.gdx")
trading_base_dir <- file.path(
  trading_project_dir,
  "output",
  paste0("global_", PSM_DATE, "_", timeresol)
)

trading_node_geojson_path <- "C:/Users/Haru Sakata/OneDrive - Kyoto University/My work/AIM-PSY/data/node/nodes_centroid.geojson"
trading_node_csv_path <- file.path(trading_data_dir, "AIM_R31_NH3_trade_nodes.csv")
trading_searoute_dir <- "C:/Users/Haru Sakata/OneDrive - Kyoto University/My work/AIM-PSY/data/searoute"
trading_searoute_path <- file.path(trading_searoute_dir, "searoute.geojson")
trading_searoute_representative_path <- file.path(trading_searoute_dir, "searoute_r31_representative.geojson")

message("Trading input run: ", psm_run_dir)
message("Trading primary GDX directory: ", psm_primary_dir)
message("Trading IAMC GDX: ", merged_gdx_path)
message("Trading scenarios: ", paste(target_scenarios, collapse = ", "))
message("Trading year: ", plot_year, "; primary timeslice multiplier: ", trading_primary_time_weight)
message("Trading output: ", trading_base_dir)

# SHARED DATA CACHE AND READERS -----------------------------------------------

trading_cache <- new.env(parent = emptyenv())

trading_cache_get <- function(key, loader) {
  if (!exists(key, envir = trading_cache, inherits = FALSE)) {
    assign(key, loader(), envir = trading_cache)
  }
  get(key, envir = trading_cache, inherits = FALSE)
}

trading_cache_path <- function(path) {
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

trading_read_gdx_param <- function(gdx_path, symbol) {
  if (!file.exists(gdx_path)) {
    stop("Missing GDX file: ", gdx_path)
  }

  cache_key <- paste("gdx", trading_cache_path(gdx_path), symbol, sep = "::")
  trading_cache_get(cache_key, function() rgdx.param(gdx_path, symbol))
}

trading_build_region_map <- function() {
  tibble::tribble(
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
    "MKD", "XEEI", "SRB", "XEEI", "KOS", "XEEI", "UKR", "XEEI",
    "BLR", "XEEI", "MDA", "XEEI", "ARM", "XEEI", "AZE", "XEEI",
    "GEO", "XEEI",
    "DZA", "XENI", "EGY", "XENI", "LBY", "XENI", "MAR", "XENI",
    "TUN", "XENI", "ESH", "XENI",
    "ZAF", "ZAF",
    "AGO", "XAF", "BEN", "XAF", "BWA", "XAF", "BFA", "XAF",
    "BDI", "XAF", "CMR", "XAF", "CAF", "XAF", "TCD", "XAF",
    "COD", "XAF", "COG", "XAF", "CIV", "XAF", "DJI", "XAF",
    "ERI", "XAF", "ETH", "XAF", "GAB", "XAF", "GHA", "XAF",
    "GIN", "XAF", "GMB", "XAF", "GNB", "XAF", "GNQ", "XAF",
    "KEN", "XAF", "LBR", "XAF", "LSO", "XAF", "MDG", "XAF",
    "MWI", "XAF", "MLI", "XAF", "MRT", "XAF", "MUS", "XAF",
    "MOZ", "XAF", "NAM", "XAF", "NER", "XAF", "NGA", "XAF",
    "RWA", "XAF", "SEN", "XAF", "SLE", "XAF", "SOM", "XAF",
    "SSD", "XAF", "SDN", "XAF", "SWZ", "XAF", "TZA", "XAF",
    "TGO", "XAF", "UGA", "XAF", "ZMB", "XAF", "ZWE", "XAF",
    "GRL", "XNF"
  )
}

trading_load_region_boundaries <- function() {
  trading_cache_get("region_boundaries", function() {
    world <- ne_countries(scale = "medium", returnclass = "sf") %>%
      st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) %>%
      st_make_valid() %>%
      mutate(iso3 = if_else(iso_a3 == "-99", adm0_a3, iso_a3))

    world %>%
      left_join(trading_build_region_map(), by = "iso3") %>%
      mutate(region = if_else(is.na(region), "XNF", region)) %>%
      st_set_crs(NA) %>%
      group_by(region) %>%
      summarise(geometry = st_union(geometry), .groups = "drop") %>%
      st_make_valid() %>%
      st_set_crs(4326)
  })
}

trading_load_link_nodes <- function(primary_path = trading_node_geojson_path, fallback_csv = trading_node_csv_path) {
  cache_key <- paste("link_nodes", trading_cache_path(primary_path), trading_cache_path(fallback_csv), sep = "::")
  trading_cache_get(cache_key, function() {
    if (file.exists(primary_path)) {
      st_read(primary_path, quiet = TRUE) %>%
        st_drop_geometry() %>%
        transmute(
          N_PSY = as.character(N_PSY),
          lon = as.numeric(centroid_x),
          lat = as.numeric(centroid_y)
        )
    } else if (file.exists(fallback_csv)) {
      read.csv(fallback_csv, stringsAsFactors = FALSE) %>%
        transmute(
          N_PSY = str_remove(as.character(N_PSY), "^R31-"),
          lon = as.numeric(lon),
          lat = as.numeric(lat)
        )
    } else {
      stop("Missing node coordinate file: ", primary_path, " or ", fallback_csv)
    }
  })
}

trading_load_trade_nodes <- function(representative_path = trading_searoute_representative_path) {
  cache_key <- paste("trade_nodes", trading_cache_path(representative_path), sep = "::")
  trading_cache_get(cache_key, function() {
    if (!file.exists(representative_path)) {
      stop("Missing sea-route representative node GeoJSON: ", representative_path)
    }

    st_read(representative_path, quiet = TRUE) %>%
      st_drop_geometry() %>%
      transmute(
        N_PSY = str_remove(as.character(R31), "^R31-"),
        lon = as.numeric(Longitude),
        lat = as.numeric(Latitude)
      )
  })
}

trading_load_sea_routes <- function(route_path = trading_searoute_path) {
  cache_key <- paste("sea_routes", trading_cache_path(route_path), sep = "::")
  trading_cache_get(cache_key, function() {
    if (!file.exists(route_path)) {
      stop("Missing sea-route GeoJSON: ", route_path)
    }

    st_read(route_path, quiet = TRUE) %>%
      st_set_crs(4326) %>%
      transmute(
        N0 = str_remove(as.character(R31_from), "^R31-"),
        N1 = str_remove(as.character(R31_to), "^R31-"),
        length_km = as.numeric(length_km),
        route_source = as.character(route_source),
        geometry = geometry
      )
  })
}

trading_region_boundaries <- trading_load_region_boundaries()
trading_link_nodes_tbl <- trading_load_link_nodes()
trading_trade_port_nodes_tbl <- trading_load_trade_nodes()
trading_sea_routes_sf <- trading_load_sea_routes()

# PLOT SPECIFICATIONS AND SHARED SCALES --------------------------------------

# Input contract used below:
# - gdx_primary/<scenario>/<year>.gdx: vff_l and vfb_l are summed by L over
#   their remaining primary-time dimensions, then expanded with the timeresol
#   weight. vfs_l is read as an already annual link capacity and is not weighted.
# - main/merged_output.gdx: iamc_gdx supplies annual regional map values.
# - Electricity link flow stays in GWh/yr; hydrogen and trade flow are converted
#   from GWh/yr to EJ/yr. vfs_l stays in GW.
gwh_to_ej <- 3.6e-6
trade_arrow_threshold_ej <- 10 * trading_primary_time_weight * gwh_to_ej
h2_color_low <- "#3182bd"
h2_color_high <- "#08306b"

link_specs <- tribble(
  ~LinkType, ~Pattern,                  ~NodeSuffix, ~Folder,         ~FileTag, ~TitleLabel,    ~FlowLow,   ~FlowHigh,
  "AC",      ".*-AC_.*-AC$",           "-AC",       "plot_link_ele", "AC",     "AC",          "#f0f9e8", "#0868ac",
  "H2",      ".*-H2TRD_.*-H2TRD$",     "-H2TRD",    "plot_link_h2",  "H2",     "H2 pipeline", h2_color_low, h2_color_high
)

link_value_specs <- tribble(
  ~Symbol, ~Aggregation, ~Direction,
  "vff_l", "sum_time",   "forward",
  "vfb_l", "sum_time",   "backward",
  "vfs_l", "as_is",      "none"
)

link_demand_specs <- tribble(
  ~LinkType, ~DemandVariable, ~DemandLabel,         ~FillLow,   ~FillHigh,
  "AC",      "Sec_Ene_Ele",   "Electricity supply", "#f7fbff", "#08306b",
  "H2",      "Sec_Ene_Hyd",   "Hydrogen demand",    "#f7f7f7", "#252525"
)

trade_flow_specs <- tribble(
  ~Symbol, ~Direction,
  "vff_l", "forward",
  "vfb_l", "backward"
)

trade_specs <- tibble(
  Carrier = c("NH3", "BMS", "SYF"),
  TradeTags = list("NH3TRD", c("BMSTRD", "CRNTRD"), "SYFTRD"),
  Folder = c("plot_trade_nh3", "plot_trade_bio", "plot_trade_syf"),
  TitleLabel = c("NH3 trade", "Biomass trade", "Synfuel trade"),
  DemandVariables = list(
    "Fin_Ene_Hyd_Amm",
    c("Sec_Ene_Ele_Bio_wo_CCS", "Sec_Ene_Ele_Bio_w_CCS"),
    "Fin_Ene_Hyd_Syn"
  ),
  MapLabel = c("Ammonia demand", "Biomass power generation", "Synfuel demand"),
  LegendTitle = c(
    "Ammonia demand (EJ/yr)",
    "Biomass power generation (EJ/yr)",
    "Synfuel demand (EJ/yr)"
  ),
  FillLow = c("#fbf7ff", "#f7fcf5", "#e0f2f1"),
  FillHigh = c("#54278f", "#006d2c", "#00796b"),
  FlowLow = c("#f1b6da", "#b2e2e2", "#b2dfdb"),
  FlowHigh = c("#c51b7d", "#006d77", "#00796b")
)

make_flow_breaks <- function(vmax) {
  if (!is.finite(vmax) || vmax <= 0) return(1)
  if (vmax < 1) {
    breaks <- pretty(c(0, vmax), n = 4)
    return(breaks[breaks > 0 & breaks <= vmax])
  }

  breaks <- 10^seq(0, ceiling(log10(vmax)), by = 1)
  breaks <- breaks[breaks <= vmax]
  if (length(breaks) == 0) breaks <- pretty(c(0, vmax), n = 4)
  breaks
}

fixed_fill_limits <- function(values) {
  vmax <- max(values, na.rm = TRUE)
  if (!is.finite(vmax) || vmax <= 0) return(NULL)
  c(0, vmax)
}

save_trading_plot <- function(plot, output_dir, filename, height_mm = 150) {
  if (is.null(plot)) return(invisible(FALSE))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(
    filename = file.path(output_dir, filename),
    plot = plot,
    width = 300,
    height = height_mm,
    units = "mm",
    dpi = 300,
    bg = "white"
  )
  invisible(TRUE)
}

# LINK FLOW DATA --------------------------------------------------------------

link_value_multiplier <- function(link_spec, value_spec) {
  time_multiplier <- if (value_spec$Symbol %in% c("vff_l", "vfb_l")) {
    trading_primary_time_weight
  } else {
    1
  }
  unit_multiplier <- if (
    link_spec$LinkType == "H2" && value_spec$Symbol %in% c("vff_l", "vfb_l")
  ) gwh_to_ej else 1
  time_multiplier * unit_multiplier
}

read_link_value <- function(scenario_name, link_spec, value_spec) {
  gdx_path <- file.path(psm_primary_dir, scenario_name, paste0(plot_year, ".gdx"))
  value_symbol <- value_spec$Symbol
  raw <- trading_read_gdx_param(gdx_path, value_symbol) %>%
    mutate(across(where(is.factor), as.character)) %>%
    filter(str_detect(L, link_spec$Pattern))

  values <- if (value_spec$Aggregation == "sum_time") {
    raw %>% group_by(L) %>% summarise(Value = sum(.data[[value_symbol]], na.rm = TRUE), .groups = "drop")
  } else {
    raw %>% transmute(L, Value = .data[[value_symbol]])
  }

  values %>%
    mutate(Value = Value * link_value_multiplier(link_spec, value_spec)) %>%
    separate(L, into = c("N0", "N1"), sep = "_", remove = FALSE) %>%
    mutate(
      Scenario = scenario_name,
      Symbol = value_symbol,
      N0 = str_remove_all(N0, fixed(link_spec$NodeSuffix)) %>% str_remove("^R31-"),
      N1 = str_remove_all(N1, fixed(link_spec$NodeSuffix)) %>% str_remove("^R31-"),
      N0_plot = if (value_spec$Direction == "backward") N1 else N0,
      N1_plot = if (value_spec$Direction == "backward") N0 else N1,
      N0 = N0_plot,
      N1 = N1_plot
    ) %>%
    select(-N0_plot, -N1_plot)
}

make_link_edges_sf <- function(link_values) {
  edges <- link_values %>%
    left_join(trading_link_nodes_tbl, by = c("N0" = "N_PSY")) %>%
    rename(x0 = lon, y0 = lat) %>%
    left_join(trading_link_nodes_tbl, by = c("N1" = "N_PSY")) %>%
    rename(x1 = lon, y1 = lat) %>%
    filter(!is.na(x0), !is.na(y0), !is.na(x1), !is.na(y1), Value > 0)

  if (nrow(edges) == 0) return(st_sf(edges, geometry = st_sfc(crs = 4326)))
  geometries <- pmap(
    edges[, c("x0", "y0", "x1", "y1")],
    ~ st_linestring(matrix(c(..1, ..2, ..3, ..4), ncol = 2, byrow = TRUE))
  )
  st_sf(edges, geometry = st_sfc(geometries, crs = 4326))
}

get_link_edges <- function(scenario_name, link_spec, value_specs) {
  map_dfr(seq_len(nrow(value_specs)), function(i) {
    read_link_value(scenario_name, link_spec, value_specs[i, ])
  }) %>%
    make_link_edges_sf()
}

# LINK MAP FUNCTIONS ----------------------------------------------------------

plot_link_combined <- function(edges_sf, scenario_name, link_spec) {
  if (nrow(edges_sf) == 0) {
    warning("No plottable combined links: ", scenario_name, " / ", link_spec$LinkType)
    return(NULL)
  }

  edges_plot <- edges_sf %>% mutate(Value_plot = log10(1 + Value))
  edges_df <- st_drop_geometry(edges_plot)
  breaks <- make_flow_breaks(max(edges_df$Value, na.rm = TRUE))
  is_h2 <- link_spec$LinkType == "H2"
  legend_title <- if (is_h2) {
    "vff_l / vfb_l annual flow (EJ/yr)"
  } else {
    "vff_l / vfb_l annual flow (GWh/yr)"
  }
  labels <- scales::label_number(accuracy = if (is_h2) 0.00001 else 1)(breaks)

  if (is_h2) {
    color_fn <- scales::col_numeric(
      palette = c(h2_color_low, h2_color_high),
      domain = range(edges_df$Value_plot, na.rm = TRUE)
    )
    edges_df <- edges_df %>% mutate(FlowColor = color_fn(Value_plot))
    return(
      ggplot() +
        geom_sf(data = trading_region_boundaries, fill = "grey96", color = "grey45", linewidth = 0.25) +
        geom_segment(
          data = edges_df,
          aes(x = x0, y = y0, xend = x1, yend = y1, linewidth = Value_plot),
          color = edges_df$FlowColor,
          alpha = 0.96,
          lineend = "round"
        ) +
        scale_linewidth_continuous(
          range = c(0.25, 4.4),
          breaks = log10(1 + breaks),
          labels = labels,
          name = "Hydrogen pipeline flow (EJ/yr)"
        ) +
        coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
        theme_void() +
        theme(
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.background = element_blank(),
          legend.key.width = grid::unit(16, "mm"),
          legend.key.height = grid::unit(3, "mm"),
          legend.title = element_text(size = 7),
          legend.text = element_text(size = 6.5),
          plot.title = element_text(hjust = 0.5, size = 12)
        ) +
        guides(linewidth = guide_legend(direction = "horizontal", title.position = "top", title.hjust = 0.5)) +
        labs(title = paste0(scenario_name, " hydrogen pipeline network flows (", plot_year, ")"))
    )
  }

  ggplot() +
    geom_sf(data = trading_region_boundaries, fill = NA, color = "grey45", linewidth = 0.35) +
    geom_segment(
      data = edges_df,
      aes(x = x0, y = y0, xend = x1, yend = y1, linewidth = Value_plot, color = Value_plot),
      alpha = 0.82,
      lineend = "round"
    ) +
    scale_linewidth_continuous(
      range = c(0.08, 1.05), breaks = log10(1 + breaks), labels = labels, name = legend_title
    ) +
    scale_color_gradient(
      low = link_spec$FlowLow, high = link_spec$FlowHigh,
      breaks = log10(1 + breaks), labels = labels, name = legend_title
    ) +
    coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
    theme_void() +
    theme(
      legend.position = "bottom", legend.direction = "horizontal",
      legend.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 12)
    ) +
    guides(linewidth = "none") +
    labs(title = paste0(scenario_name, " inter-regional AC combined flow (", plot_year, ")"))
}

plot_link_capacity <- function(edges_sf, scenario_name, link_spec) {
  if (nrow(edges_sf) == 0) {
    warning("No plottable vfs_l links: ", scenario_name, " / ", link_spec$LinkType)
    return(NULL)
  }
  edges_plot <- edges_sf %>% mutate(Value_plot = log10(1 + Value))
  breaks <- make_flow_breaks(max(edges_plot$Value, na.rm = TRUE))
  ggplot() +
    geom_sf(data = trading_region_boundaries, fill = NA, color = "grey45", linewidth = 0.35) +
    geom_sf(data = edges_plot, aes(linewidth = Value_plot, color = Value_plot), alpha = 0.85) +
    scale_linewidth_continuous(
      range = c(0.10, 2.2), breaks = log10(1 + breaks), labels = breaks, name = "vfs_l link capacity (GW)"
    ) +
    scale_color_gradient(
      low = link_spec$FlowLow, high = link_spec$FlowHigh,
      breaks = log10(1 + breaks), labels = breaks, name = "vfs_l link capacity (GW)"
    ) +
    coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
    theme_void() +
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size = 12)) +
    guides(linewidth = "none") +
    labs(title = paste0(scenario_name, " ", link_spec$TitleLabel, " vfs_l capacity (", plot_year, ")"))
}

read_iamc_map_values <- function(variable_map) {
  trading_read_gdx_param(merged_gdx_path, "iamc_gdx") %>%
    transmute(
      Scenario = as.character(Sc),
      Region = str_remove(as.character(Sr), "^R31-"),
      Variable = as.character(Sv),
      Year = as.numeric(as.character(Y5)),
      Value = suppressWarnings(as.numeric(as.character(iamc_gdx)))
    ) %>%
    inner_join(variable_map, by = "Variable") %>%
    filter(
      Scenario %in% target_scenarios,
      Region %in% trading_region_boundaries$region,
      Year == plot_year,
      !is.na(Value)
    )
}

plot_link_demand_overlay <- function(map_data, edges_sf, scenario_name, link_spec, demand_spec) {
  if (nrow(edges_sf) == 0) return(NULL)
  plot_data <- map_data %>% filter(Scenario == scenario_name, Variable == demand_spec$DemandVariable)
  fill_limits <- fixed_fill_limits(map_data %>% filter(Variable == demand_spec$DemandVariable) %>% pull(Value))
  edges_df <- edges_sf %>% mutate(Value_plot = log10(1 + Value)) %>% st_drop_geometry()
  breaks <- make_flow_breaks(max(edges_df$Value, na.rm = TRUE))
  is_h2 <- link_spec$LinkType == "H2"
  h2_arrow_threshold <- 10 * trading_primary_time_weight * gwh_to_ej
  edges_line_df <- if (is_h2) edges_df %>% filter(Value <= h2_arrow_threshold) else edges_df
  edges_arrow_df <- if (is_h2) edges_df %>% filter(Value > h2_arrow_threshold) else edges_df %>% slice(0)
  labels <- scales::label_number(accuracy = if (is_h2) 0.00001 else 1)(breaks)
  legend_title <- if (is_h2) "Combined flow (EJ/yr)" else "Combined flow (GWh/yr)"

  ggplot() +
    geom_sf(data = plot_data, aes(fill = Value), color = "grey35", linewidth = 0.3) +
    geom_segment(
      data = edges_line_df,
      aes(x = x0, y = y0, xend = x1, yend = y1, linewidth = Value_plot, color = Value_plot),
      alpha = 0.82,
      lineend = "round"
    ) +
    geom_segment(
      data = edges_arrow_df,
      aes(x = x0, y = y0, xend = x1, yend = y1, linewidth = Value_plot, color = Value_plot),
      alpha = 0.82,
      lineend = "round",
      arrow = arrow(length = grid::unit(1.45, "mm"), type = "closed")
    ) +
    scale_fill_gradient(
      low = demand_spec$FillLow, high = demand_spec$FillHigh, limits = fill_limits,
      na.value = "grey92", labels = scales::label_number(accuracy = 0.01), name = "Demand (EJ/yr)"
    ) +
    scale_linewidth_continuous(
      range = c(0.25, 1.45), breaks = log10(1 + breaks), labels = labels, name = legend_title
    ) +
    scale_color_gradient(
      low = link_spec$FlowLow, high = link_spec$FlowHigh,
      breaks = log10(1 + breaks), labels = labels, name = legend_title
    ) +
    coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
    theme_void() +
    theme(
      legend.position = "bottom", legend.direction = "horizontal", legend.box = "horizontal",
      legend.background = element_blank(), legend.key.width = grid::unit(16, "mm"),
      legend.key.height = grid::unit(3, "mm"), legend.title = element_text(size = 7),
      legend.text = element_text(size = 6.5), plot.title = element_text(hjust = 0.5, size = 12)
    ) +
    guides(linewidth = "none") +
    labs(title = paste0(
      scenario_name, " ", demand_spec$DemandLabel, " and ", link_spec$TitleLabel,
      " combined transport flow (", plot_year, ")"
    ))
}

# MARITIME TRADE FLOW DATA ----------------------------------------------------

read_trade_value <- function(scenario_name, trade_spec, flow_spec) {
  gdx_path <- file.path(psm_primary_dir, scenario_name, paste0(plot_year, ".gdx"))
  value_symbol <- flow_spec$Symbol
  trade_tags <- trade_spec$TradeTags[[1]]
  link_pattern <- paste0(
    "^R31-[A-Z0-9]+-", trade_tags, "_R31-[A-Z0-9]+-", trade_tags, "$",
    collapse = "|"
  )
  raw <- trading_read_gdx_param(gdx_path, value_symbol) %>%
    mutate(across(where(is.factor), as.character)) %>%
    filter(str_detect(L, link_pattern))
  link_nodes <- str_match(raw$L, "^R31-([A-Z0-9]+)-[A-Z0-9]+_R31-([A-Z0-9]+)-[A-Z0-9]+$")

  raw %>%
    mutate(N0 = link_nodes[, 2], N1 = link_nodes[, 3]) %>%
    group_by(N0, N1) %>%
    summarise(Value = sum(.data[[value_symbol]], na.rm = TRUE), .groups = "drop") %>%
    mutate(
      Value = Value * trading_primary_time_weight * gwh_to_ej,
      Scenario = scenario_name,
      Symbol = value_symbol,
      N0_plot = if (flow_spec$Direction == "backward") N1 else N0,
      N1_plot = if (flow_spec$Direction == "backward") N0 else N1,
      N0 = N0_plot,
      N1 = N1_plot
    ) %>%
    select(-N0_plot, -N1_plot) %>%
    filter(Value > 0)
}

make_trade_edges_sf <- function(values) {
  missing_nodes <- setdiff(unique(c(values$N0, values$N1)), trading_trade_port_nodes_tbl$N_PSY)
  if (length(missing_nodes) > 0) stop("Missing transport node coordinates: ", paste(missing_nodes, collapse = ", "))

  edges <- values %>%
    inner_join(trading_trade_port_nodes_tbl, by = c("N0" = "N_PSY")) %>% rename(x0 = lon, y0 = lat) %>%
    inner_join(trading_trade_port_nodes_tbl, by = c("N1" = "N_PSY")) %>% rename(x1 = lon, y1 = lat) %>%
    filter(Value > 0) %>%
    mutate(route_key = paste(N0, N1, sep = "__"))
  routes <- trading_sea_routes_sf %>% mutate(route_key = paste(N0, N1, sep = "__"))
  route_index <- match(edges$route_key, routes$route_key)
  if (anyNA(route_index)) stop("Missing sea routes: ", paste(unique(edges$route_key[is.na(route_index)]), collapse = ", "))

  st_sf(edges %>% select(-route_key), geometry = st_geometry(routes)[route_index], crs = st_crs(routes))
}

normalize_longitude <- function(x) ((x + 180) %% 360) - 180

route_coordinates_df <- function(edge_sf, route_id) {
  as_tibble(st_coordinates(st_geometry(edge_sf))) %>%
    mutate(
      route_id = route_id, x = normalize_longitude(X), y = Y,
      part = if ("L1" %in% names(.)) L1 else 1L, coord_order = row_number()
    ) %>%
    group_by(part) %>%
    mutate(dateline_part = cumsum(c(FALSE, abs(diff(x)) > 180)), segment_id = paste(route_id, part, dateline_part, sep = "__")) %>%
    ungroup()
}

prepare_trade_flow <- function(edges_sf) {
  edges_plot <- edges_sf %>% mutate(Value_plot = log10(1 + Value))
  route_lines <- map_dfr(seq_len(nrow(edges_plot)), function(i) {
    edge <- edges_plot[i, ]
    attributes <- st_drop_geometry(edge)
    route_id <- paste(attributes$Scenario, attributes$Symbol, attributes$N0, attributes$N1, i, sep = "__")
    route_coordinates_df(edge, route_id) %>%
      mutate(Value = attributes$Value, Value_plot = attributes$Value_plot)
  }) %>% arrange(Value, route_id, segment_id, coord_order)

  route_nodes <- st_drop_geometry(edges_plot) %>%
    { bind_rows(
      transmute(., N_PSY = N0, lon = x0, lat = y0),
      transmute(., N_PSY = N1, lon = x1, lat = y1)
    ) } %>% distinct(N_PSY, lon, lat)

  route_arrows <- route_lines %>%
    filter(Value > trade_arrow_threshold_ej) %>%
    group_by(route_id) %>%
    filter(segment_id == last(segment_id)) %>%
    slice_tail(n = 2) %>%
    summarise(
      x = first(x), y = first(y), xend = last(x), yend = last(y),
      Value_plot = last(Value_plot), .groups = "drop"
    )

  list(route_lines = route_lines, route_arrows = route_arrows, route_nodes = route_nodes)
}

# MARITIME TRADE MAP FUNCTIONS -----------------------------------------------

plot_trade_flow <- function(edges_sf, scenario_name, trade_spec, map_data = NULL, prepared_flow = NULL) {
  if (nrow(edges_sf) == 0) {
    warning("No plottable trade links: ", scenario_name, " / ", trade_spec$Carrier)
    return(NULL)
  }
  if (is.null(prepared_flow)) prepared_flow <- prepare_trade_flow(edges_sf)
  breaks <- make_flow_breaks(max(edges_sf$Value, na.rm = TRUE))
  p <- ggplot()

  if (is.null(map_data)) {
    p <- p + geom_sf(data = trading_region_boundaries, fill = NA, color = "grey45", linewidth = 0.35)
  } else {
    plot_data <- map_data %>% filter(Scenario == scenario_name, Carrier == trade_spec$Carrier)
    fill_limits <- fixed_fill_limits(map_data %>% filter(Carrier == trade_spec$Carrier) %>% pull(Value))
    p <- p +
      geom_sf(data = plot_data, aes(fill = Value), color = "grey35", linewidth = 0.3) +
      scale_fill_gradient(
        low = trade_spec$FillLow, high = trade_spec$FillHigh, limits = fill_limits,
        na.value = "grey92", labels = scales::label_number(accuracy = 0.01), name = trade_spec$LegendTitle
      )
  }

  p +
    geom_path(
      data = prepared_flow$route_lines,
      aes(x = x, y = y, group = segment_id),
      color = "white", linewidth = 0.25, alpha = 0.55, lineend = "round", show.legend = FALSE
    ) +
    geom_path(
      data = prepared_flow$route_lines,
      aes(x = x, y = y, group = segment_id, linewidth = Value_plot, color = Value_plot),
      alpha = 0.86, lineend = "round"
    ) +
    geom_segment(
      data = prepared_flow$route_arrows,
      aes(x = x, y = y, xend = xend, yend = yend),
      color = "white", linewidth = 1.0, alpha = 0.7, lineend = "round",
      arrow = arrow(length = grid::unit(2.4, "mm"), type = "closed"),
      show.legend = FALSE
    ) +
    geom_segment(
      data = prepared_flow$route_arrows,
      aes(x = x, y = y, xend = xend, yend = yend, linewidth = Value_plot, color = Value_plot),
      alpha = 0.86, lineend = "round",
      arrow = arrow(length = grid::unit(2.15, "mm"), type = "closed")
    ) +
    geom_point(
      data = prepared_flow$route_nodes, aes(x = lon, y = lat), shape = 21, size = 2.8,
      stroke = 0.55, color = "grey20", fill = "white", alpha = 0.95, inherit.aes = FALSE
    ) +
    scale_linewidth_continuous(
      range = c(0.05, 2.4), breaks = log10(1 + breaks),
      labels = scales::label_number(accuracy = 0.00001)(breaks),
      name = "vff_l / vfb_l annual flow (EJ/yr)"
    ) +
    scale_color_gradient(
      low = trade_spec$FlowLow, high = trade_spec$FlowHigh, breaks = log10(1 + breaks),
      labels = scales::label_number(accuracy = 0.00001)(breaks),
      name = "vff_l / vfb_l annual flow (EJ/yr)"
    ) +
    coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
    theme_void() +
    theme(
      legend.position = "bottom", legend.direction = "horizontal", legend.box = "horizontal",
      legend.background = element_blank(), legend.key.width = grid::unit(16, "mm"),
      legend.key.height = grid::unit(3, "mm"), legend.title = element_text(size = 7),
      legend.text = element_text(size = 6.5), plot.title = element_text(hjust = 0.5, size = 12)
    ) +
    guides(linewidth = "none") +
    labs(title = paste0(
      scenario_name, " ", trade_spec$TitleLabel,
      if (is.null(map_data)) "" else paste0(" and ", trade_spec$MapLabel),
      " combined sea trade flow (", plot_year, ")"
    ))
}

# READ DEMAND MAP DATA --------------------------------------------------------

has_iamc_map_data <- file.exists(merged_gdx_path)
if (has_iamc_map_data) {
  link_variable_map <- link_demand_specs %>% transmute(Variable = DemandVariable)
  link_map_values <- read_iamc_map_values(link_variable_map) %>%
    group_by(Scenario, Region, Variable, Year) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")
  link_map_data <- expand_grid(
    Scenario = target_scenarios,
    Variable = link_demand_specs$DemandVariable,
    region = trading_region_boundaries$region
  ) %>%
    left_join(link_map_values, by = c("Scenario", "Variable", "region" = "Region")) %>%
    left_join(trading_region_boundaries %>% select(region, geometry), by = "region") %>%
    st_as_sf(crs = st_crs(trading_region_boundaries))

  trade_variable_map <- trade_specs %>%
    select(Carrier, DemandVariables) %>%
    unnest(DemandVariables) %>%
    rename(Variable = DemandVariables)
  trade_map_values <- read_iamc_map_values(trade_variable_map) %>%
    group_by(Scenario, Region, Carrier, Year) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")
  trade_map_data <- expand_grid(
    Scenario = target_scenarios,
    Carrier = trade_specs$Carrier,
    region = trading_region_boundaries$region
  ) %>%
    left_join(trade_map_values, by = c("Scenario", "Carrier", "region" = "Region")) %>%
    left_join(trading_region_boundaries %>% select(region, geometry), by = "region") %>%
    st_as_sf(crs = st_crs(trading_region_boundaries))
} else {
  warning("Demand overlays will be skipped because merged_output.gdx is missing: ", merged_gdx_path)
}

# GENERATE AND SAVE LINK PLOTS ------------------------------------------------

link_flow_specs <- link_value_specs %>% filter(Symbol %in% c("vff_l", "vfb_l"))
link_capacity_spec <- link_value_specs %>% filter(Symbol == "vfs_l")

for (i in seq_len(nrow(link_specs))) {
  link_spec <- link_specs[i, ]
  demand_spec <- link_demand_specs %>% filter(LinkType == link_spec$LinkType)

  for (scenario_name in target_scenarios) {
    output_dir <- file.path(trading_base_dir, scenario_name, link_spec$Folder)
    combined_edges <- get_link_edges(scenario_name, link_spec, link_flow_specs)
    capacity_edges <- get_link_edges(scenario_name, link_spec, link_capacity_spec)

    save_trading_plot(
      plot_link_combined(combined_edges, scenario_name, link_spec),
      output_dir,
      paste0("global_", scenario_name, "_vff_l_vfb_l_combined_", link_spec$FileTag, "_", plot_year, ".png")
    )
    save_trading_plot(
      plot_link_capacity(capacity_edges, scenario_name, link_spec),
      output_dir,
      paste0("global_", scenario_name, "_vfs_l_", link_spec$FileTag, "_", plot_year, ".png"),
      height_mm = 130
    )

    if (has_iamc_map_data) {
      save_trading_plot(
        plot_link_demand_overlay(link_map_data, combined_edges, scenario_name, link_spec, demand_spec),
        output_dir,
        paste0(
          "global_", scenario_name, "_", demand_spec$DemandVariable,
          "_vff_l_vfb_l_combined_", link_spec$FileTag, "_demand_link_overlay_", plot_year, ".png"
        )
      )
    }
  }
}

# GENERATE AND SAVE TRADE PLOTS -----------------------------------------------

for (i in seq_len(nrow(trade_specs))) {
  trade_spec <- trade_specs[i, ]

  for (scenario_name in target_scenarios) {
    output_dir <- file.path(trading_base_dir, scenario_name, trade_spec$Folder)
    trade_edges <- map_dfr(seq_len(nrow(trade_flow_specs)), function(j) {
      read_trade_value(scenario_name, trade_spec, trade_flow_specs[j, ])
    }) %>%
      make_trade_edges_sf()
    prepared_flow <- prepare_trade_flow(trade_edges)

    save_trading_plot(
      plot_trade_flow(trade_edges, scenario_name, trade_spec, prepared_flow = prepared_flow),
      output_dir,
      paste0("global_", scenario_name, "_vff_l_vfb_l_combined_", trade_spec$Carrier, "_trade_", plot_year, ".png")
    )

    if (has_iamc_map_data) {
      save_trading_plot(
        plot_trade_flow(
          trade_edges, scenario_name, trade_spec,
          map_data = trade_map_data, prepared_flow = prepared_flow
        ),
        output_dir,
        paste0(
          "global_", scenario_name, "_vff_l_vfb_l_combined_", trade_spec$Carrier,
          "_demand_trade_overlay_", plot_year, ".png"
        )
      )
    }
  }
}

# GENERATE AND SAVE COMBINED H2, NH3, AND SYNFUEL NETWORK --------------------

network_carrier_levels <- c("Hydrogen", "Synfuel", "Ammonia")
network_carrier_colors <- c(
  "Hydrogen" = h2_color_high,
  "Synfuel" = "#00796b",
  "Ammonia" = "#c51b7d"
)

read_network_trade_edges <- function(scenario_name, carrier_code) {
  trade_spec <- trade_specs %>% filter(Carrier == carrier_code)
  if (nrow(trade_spec) != 1) {
    stop("Expected one trade specification for carrier: ", carrier_code)
  }

  map_dfr(seq_len(nrow(trade_flow_specs)), function(i) {
    read_trade_value(scenario_name, trade_spec, trade_flow_specs[i, ])
  }) %>%
    make_trade_edges_sf()
}

prepare_network_trade_layer <- function(edges_sf, carrier_name) {
  carrier_factor <- function(x) factor(x, levels = network_carrier_levels)

  if (nrow(edges_sf) == 0) {
    return(list(
      route_lines = tibble(
        x = numeric(), y = numeric(), segment_id = character(),
        Value_plot = numeric(), Carrier = carrier_factor(character())
      ),
      route_arrows = tibble(
        x = numeric(), y = numeric(), xend = numeric(), yend = numeric(),
        Value_plot = numeric(), Carrier = carrier_factor(character())
      ),
      route_nodes = tibble(N_PSY = character(), lon = numeric(), lat = numeric())
    ))
  }

  prepared <- prepare_trade_flow(edges_sf)
  list(
    route_lines = prepared$route_lines %>%
      mutate(Carrier = carrier_factor(carrier_name)),
    route_arrows = prepared$route_arrows %>%
      mutate(Carrier = carrier_factor(carrier_name)),
    route_nodes = prepared$route_nodes
  )
}

add_network_trade_layer <- function(trade_layer) {
  list(
    geom_path(
      data = trade_layer$route_lines,
      aes(x = x, y = y, group = segment_id),
      color = "white",
      linewidth = 0.55,
      alpha = 0.75,
      lineend = "round",
      show.legend = FALSE
    ),
    geom_path(
      data = trade_layer$route_lines,
      aes(
        x = x, y = y, group = segment_id,
        linewidth = Value_plot, color = Carrier
      ),
      alpha = 0.9,
      lineend = "round"
    ),
    geom_segment(
      data = trade_layer$route_arrows,
      aes(x = x, y = y, xend = xend, yend = yend),
      color = "white",
      linewidth = 1.0,
      alpha = 0.75,
      lineend = "round",
      arrow = arrow(length = grid::unit(2.4, "mm"), type = "closed"),
      show.legend = FALSE
    ),
    geom_segment(
      data = trade_layer$route_arrows,
      aes(
        x = x, y = y, xend = xend, yend = yend,
        linewidth = Value_plot, color = Carrier
      ),
      alpha = 0.9,
      lineend = "round",
      arrow = arrow(length = grid::unit(2.15, "mm"), type = "closed"),
      show.legend = FALSE
    )
  )
}

plot_combined_carrier_network <- function(
  h2_edges,
  synfuel_edges,
  ammonia_edges,
  scenario_name
) {
  network_values <- c(h2_edges$Value, synfuel_edges$Value, ammonia_edges$Value)
  network_values <- network_values[is.finite(network_values) & network_values > 0]
  if (length(network_values) == 0) {
    warning("No H2, NH3, or synfuel flows available for scenario: ", scenario_name)
    return(NULL)
  }

  network_breaks <- make_flow_breaks(max(network_values))
  flow_labels <- scales::label_number(accuracy = 0.00001)(network_breaks)
  h2_plot_data <- h2_edges %>%
    mutate(
      Value_plot = log10(1 + Value),
      Carrier = factor("Hydrogen", levels = network_carrier_levels)
    ) %>%
    st_drop_geometry() %>%
    arrange(Value)
  synfuel_layer <- prepare_network_trade_layer(synfuel_edges, "Synfuel")
  ammonia_layer <- prepare_network_trade_layer(ammonia_edges, "Ammonia")
  trade_nodes <- bind_rows(
    synfuel_layer$route_nodes,
    ammonia_layer$route_nodes
  ) %>%
    distinct(N_PSY, lon, lat)

  ggplot() +
    geom_sf(
      data = trading_region_boundaries,
      fill = "grey96",
      color = "grey45",
      linewidth = 0.25
    ) +
    geom_segment(
      data = h2_plot_data,
      aes(
        x = x0, y = y0, xend = x1, yend = y1,
        linewidth = Value_plot, color = Carrier
      ),
      alpha = 0.92,
      lineend = "round"
    ) +
    add_network_trade_layer(synfuel_layer) +
    add_network_trade_layer(ammonia_layer) +
    geom_point(
      data = trade_nodes,
      aes(x = lon, y = lat),
      shape = 21,
      size = 2.5,
      stroke = 0.5,
      color = "grey20",
      fill = "white",
      alpha = 0.95,
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    scale_linewidth_continuous(
      range = c(0.20, 4.0),
      breaks = log10(1 + network_breaks),
      labels = flow_labels,
      name = "vff_l / vfb_l annual flow (EJ/yr)"
    ) +
    scale_color_manual(
      values = network_carrier_colors,
      breaks = network_carrier_levels,
      drop = FALSE,
      name = "Energy carrier"
    ) +
    coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      legend.background = element_blank(),
      legend.key.width = grid::unit(12, "mm"),
      legend.key.height = grid::unit(3, "mm"),
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 6.5),
      plot.title = element_text(hjust = 0.5, size = 12)
    ) +
    guides(
      color = guide_legend(
        order = 1,
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        override.aes = list(linewidth = 2.5, alpha = 1)
      ),
      linewidth = guide_legend(
        order = 2,
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    labs(
      title = paste0(
        scenario_name,
        " hydrogen, synfuel, and ammonia combined network flows (",
        plot_year,
        ")"
      )
    )
}

h2_network_spec <- link_specs %>% filter(LinkType == "H2")
for (scenario_name in target_scenarios) {
  h2_network_edges <- get_link_edges(
    scenario_name,
    h2_network_spec,
    link_flow_specs
  )
  synfuel_network_edges <- read_network_trade_edges(scenario_name, "SYF")
  ammonia_network_edges <- read_network_trade_edges(scenario_name, "NH3")

  save_trading_plot(
    plot_combined_carrier_network(
      h2_edges = h2_network_edges,
      synfuel_edges = synfuel_network_edges,
      ammonia_edges = ammonia_network_edges,
      scenario_name = scenario_name
    ),
    file.path(trading_base_dir, scenario_name, "plot_network"),
    paste0(
      tolower(PSM_REGION),
      "_",
      scenario_name,
      "_vff_l_vfb_l_combined_H2_SYF_NH3_network_",
      plot_year,
      ".png"
    )
  )
}
