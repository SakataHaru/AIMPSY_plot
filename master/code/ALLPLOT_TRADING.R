script_args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_args[grepl("^--file=", script_args)])

script_dir <- if (length(script_path) > 0) {
  dirname(normalizePath(script_path[[1]], winslash = "/", mustWork = FALSE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

project_dir <- if (file.exists(file.path(script_dir, "code", "dataframe.R"))) {
  script_dir
} else {
  normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)
}

old_wd <- getwd()
setwd(project_dir)
on.exit(setwd(old_wd), add = TRUE)

library(dplyr)
library(stringr)
library(sf)
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

trading_project_dir <- project_dir
trading_data_dir <- file.path(trading_project_dir, "data")
trading_base_dir <- file.path(trading_project_dir, "output", "global_260617")
trading_aimpsy_dir <- "//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output"
trading_node_geojson_path <- "C:/Users/Haru Sakata/OneDrive - Kyoto University/My work/AIM-PSY/data/node/nodes_centroid.geojson"
trading_node_csv_path <- file.path(trading_data_dir, "AIM_R31_NH3_trade_nodes.csv")
trading_searoute_dir <- "C:/Users/Haru Sakata/OneDrive - Kyoto University/My work/AIM-PSY/data/searoute"
trading_searoute_path <- file.path(trading_searoute_dir, "searoute.geojson")
trading_searoute_representative_path <- file.path(trading_searoute_dir, "searoute_r31_representative.geojson")

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

trading_load_script_region_boundaries <- function(script_path, cache_key) {
  trading_cache_get(cache_key, function() {
    lines <- readLines(script_path, warn = FALSE)
    start <- grep("^build_region_boundaries <- function", lines)[1]
    end <- grep("^region_boundaries <-", lines)[1] - 1
    if (is.na(start) || is.na(end) || end <= start) {
      stop("Could not locate region boundary function in: ", script_path)
    }

    region_env <- new.env(parent = globalenv())
    eval(parse(text = paste(lines[start:end], collapse = "\n")), envir = region_env)
    region_env$build_region_boundaries()
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
trading_link_region_boundaries <- trading_load_script_region_boundaries(
  file.path(trading_project_dir, "code", "plot_link.R"),
  "link_region_boundaries"
)
trading_link_nodes_tbl <- trading_load_link_nodes()
trading_trade_port_nodes_tbl <- trading_load_trade_nodes()
trading_sea_routes_sf <- trading_load_sea_routes()

plot_scripts <- file.path(
  project_dir,
  "code",
  c(
    "plot_link.R",
    "plot_trade.R",
    "plot_trade_diff.R",
    "plot_network.R"
  )
)

for (plot_script in plot_scripts) {
  message("Sourcing: ", plot_script)
  source(plot_script, local = FALSE)
}
