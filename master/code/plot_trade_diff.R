# Global softlink maritime trade difference maps -----------------------------

library(dplyr)
library(stringr)
library(tidyr)
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

aimpsy <- "//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output"
scenario_group <- "global_softlink_Sc"
region <- "Global"
date <- "2606151735"
plot_year <- 2050
base_scenario <- "NPi500"
scenarios <- c("NPi500-ZF2050")

script_args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_args[grepl("^--file=", script_args)])
if (length(script_path) == 0) {
  candidate_script_paths <- c(
    file.path("master", "code", "plot_trade_diff.R"),
    file.path("code", "plot_trade_diff.R"),
    "plot_trade_diff.R"
  )
  script_path <- candidate_script_paths[file.exists(candidate_script_paths)]
}
if (length(script_path) == 0) {
  stop("Could not locate plot_trade_diff.R. Run this script from C:/AIMPSY_plot or C:/AIMPSY_plot/master.")
}
script_path <- normalizePath(script_path[1])
project_dir <- normalizePath(file.path(dirname(script_path), ".."))
base_dir <- file.path(project_dir, "output", "global_260617")
scenario_analysis_dir <- file.path(base_dir, "scenario_anarysis")

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

  world %>%
    left_join(build_region_map(), by = "iso3") %>%
    mutate(region = if_else(is.na(region), "XNF", region)) %>%
    st_set_crs(NA) %>%
    group_by(region) %>%
    summarise(geometry = st_union(geometry), .groups = "drop") %>%
    st_make_valid() %>%
    st_set_crs(4326)
}

region_boundaries <- build_region_boundaries()

prm_dir <- file.path(aimpsy, scenario_group, region, date, "gams_output", "gdx_primary")
searoute_dir <- "C:/Users/Haru Sakata/OneDrive - Kyoto University/My work/AIM-PSY/data/searoute"
searoute_path <- file.path(searoute_dir, "searoute.geojson")
searoute_representative_path <- file.path(searoute_dir, "searoute_r31_representative.geojson")
node_path <- "C:/Users/Haru Sakata/OneDrive - Kyoto University/My work/AIM-PSY/data/node/nodes_centroid.geojson"
node_csv <- file.path("data", "AIM_R31_NH3_trade_nodes.csv")
gwh_to_ej <- 3.6e-6
time_slice_hours <- 6
annual_flow_multiplier <- time_slice_hours
arrow_threshold <- 10 * annual_flow_multiplier * gwh_to_ej

flow_specs <- tribble(
  ~Symbol, ~Direction,
  "vff_l", "forward",
  "vfb_l", "backward"
)

overlay_specs <- tribble(
  ~Carrier, ~Folder,           ~TitleLabel,      ~LinkPattern,
  "NH3",    "plot_trade_nh3",  "NH3 trade",      "^R31-[A-Z0-9]+-NH3_R31-[A-Z0-9]+-NH3$",
  "BMS",    "plot_trade_bio",  "Biomass trade",  "^R31-[A-Z0-9]+-BMS_R31-[A-Z0-9]+-BMS$"
)

transmission_capacity_specs <- tribble(
  ~LinkType, ~Folder,          ~TitleLabel,       ~LinkPattern,       ~NodeSuffix,
  "AC",      "plot_link_ele",  "AC transmission", ".*-AC_.*-AC$",     "-AC"
)

read_transport_value <- function(scenario_name, overlay_spec, flow_spec) {
  flow_gdx_path <- file.path(prm_dir, scenario_name, paste0(plot_year, ".gdx"))
  if (!file.exists(flow_gdx_path)) {
    stop("Missing GDX file: ", flow_gdx_path)
  }

  value_symbol <- flow_spec$Symbol
  direction <- flow_spec$Direction

  df_raw <- rgdx.param(flow_gdx_path, value_symbol) %>%
    mutate(across(where(is.factor), as.character)) %>%
    filter(str_detect(L, overlay_spec$LinkPattern))

  df_link <- df_raw %>%
    group_by(L) %>%
    summarise(Value = sum(.data[[value_symbol]], na.rm = TRUE), .groups = "drop") %>%
    mutate(Value = Value * annual_flow_multiplier * gwh_to_ej) %>%
    filter(Value > 0)

  if (nrow(df_link) == 0) {
    return(tibble(L = character(), Value = numeric(), Scenario = character(), Symbol = character(), N0 = character(), N1 = character()))
  }

  link_nodes <- str_match(
    df_link$L,
    paste0("^R31-([A-Z0-9]+)-", overlay_spec$Carrier, "_R31-([A-Z0-9]+)-", overlay_spec$Carrier, "$")
  )

  df_link %>%
    mutate(
      N0 = link_nodes[, 2],
      N1 = link_nodes[, 3],
      Scenario = scenario_name,
      Symbol = value_symbol,
      N0_plot = if (direction == "backward") N1 else N0,
      N1_plot = if (direction == "backward") N0 else N1,
      N0 = N0_plot,
      N1 = N1_plot
    ) %>%
    select(-N0_plot, -N1_plot)
}

read_combined_transport_values <- function(scenario_name, overlay_spec) {
  map_dfr(seq_len(nrow(flow_specs)), function(j) {
    read_transport_value(scenario_name, overlay_spec, flow_specs[j, ])
  }) %>%
    group_by(N0, N1) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    mutate(Symbol = "vff_l_vfb_l") %>%
    select(Symbol, N0, N1, Value)
}

read_transport_capacity_value <- function(scenario_name, overlay_spec) {
  capacity_gdx_path <- file.path(prm_dir, scenario_name, paste0(plot_year, ".gdx"))
  if (!file.exists(capacity_gdx_path)) {
    stop("Missing GDX file: ", capacity_gdx_path)
  }

  df_link <- rgdx.param(capacity_gdx_path, "vfs_l") %>%
    mutate(across(where(is.factor), as.character)) %>%
    filter(str_detect(L, overlay_spec$LinkPattern)) %>%
    transmute(L, Value = vfs_l) %>%
    filter(!is.na(Value), Value > 0)

  if (nrow(df_link) == 0) {
    return(tibble(Symbol = character(), N0 = character(), N1 = character(), Value = numeric()))
  }

  link_nodes <- str_match(
    df_link$L,
    paste0("^R31-([A-Z0-9]+)-", overlay_spec$Carrier, "_R31-([A-Z0-9]+)-", overlay_spec$Carrier, "$")
  )

  df_link %>%
    mutate(
      Symbol = "vfs_l",
      N0 = link_nodes[, 2],
      N1 = link_nodes[, 3]
    ) %>%
    select(Symbol, N0, N1, Value)
}

build_transport_diff_values <- function(scenario_name, base_scenario_name, overlay_spec) {
  scenario_values <- read_combined_transport_values(scenario_name, overlay_spec) %>%
    rename(ScenarioValue = Value)
  base_values <- read_combined_transport_values(base_scenario_name, overlay_spec) %>%
    rename(BaseValue = Value)

  full_join(scenario_values, base_values, by = c("Symbol", "N0", "N1")) %>%
    mutate(
      ScenarioValue = coalesce(ScenarioValue, 0),
      BaseValue = coalesce(BaseValue, 0),
      Value = ScenarioValue - BaseValue,
      AbsValue = abs(Value),
      Scenario = scenario_name
    ) %>%
    filter(AbsValue > 0) %>%
    select(Scenario, Symbol, N0, N1, Value, AbsValue, ScenarioValue, BaseValue)
}

build_transport_capacity_diff_values <- function(scenario_name, base_scenario_name, overlay_spec) {
  scenario_values <- read_transport_capacity_value(scenario_name, overlay_spec) %>%
    rename(ScenarioValue = Value)
  base_values <- read_transport_capacity_value(base_scenario_name, overlay_spec) %>%
    rename(BaseValue = Value)

  full_join(scenario_values, base_values, by = c("Symbol", "N0", "N1")) %>%
    mutate(
      ScenarioValue = coalesce(ScenarioValue, 0),
      BaseValue = coalesce(BaseValue, 0),
      Value = ScenarioValue - BaseValue,
      AbsValue = abs(Value),
      Scenario = scenario_name
    ) %>%
    filter(AbsValue > 0) %>%
    select(Scenario, Symbol, N0, N1, Value, AbsValue, ScenarioValue, BaseValue)
}

normalize_link_node <- function(node_name, node_suffix) {
  node_name %>%
    str_remove_all(fixed(node_suffix)) %>%
    str_remove("^R31-")
}

load_link_nodes <- function(primary_path, fallback_csv) {
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
}

read_link_capacity_value <- function(scenario_name, capacity_spec) {
  capacity_gdx_path <- file.path(prm_dir, scenario_name, paste0(plot_year, ".gdx"))
  if (!file.exists(capacity_gdx_path)) {
    stop("Missing GDX file: ", capacity_gdx_path)
  }

  rgdx.param(capacity_gdx_path, "vfs_l") %>%
    mutate(across(where(is.factor), as.character)) %>%
    filter(str_detect(L, capacity_spec$LinkPattern)) %>%
    transmute(L, Value = vfs_l) %>%
    filter(!is.na(Value), Value > 0) %>%
    separate(L, sep = "_", into = c("N0", "N1"), remove = FALSE) %>%
    mutate(
      Symbol = "vfs_l",
      N0 = normalize_link_node(N0, capacity_spec$NodeSuffix),
      N1 = normalize_link_node(N1, capacity_spec$NodeSuffix)
    ) %>%
    select(Symbol, N0, N1, Value)
}

build_link_capacity_diff_values <- function(scenario_name, base_scenario_name, capacity_spec) {
  scenario_values <- read_link_capacity_value(scenario_name, capacity_spec) %>%
    rename(ScenarioValue = Value)
  base_values <- read_link_capacity_value(base_scenario_name, capacity_spec) %>%
    rename(BaseValue = Value)

  full_join(scenario_values, base_values, by = c("Symbol", "N0", "N1")) %>%
    mutate(
      ScenarioValue = coalesce(ScenarioValue, 0),
      BaseValue = coalesce(BaseValue, 0),
      Value = ScenarioValue - BaseValue,
      AbsValue = abs(Value),
      Scenario = scenario_name
    ) %>%
    filter(AbsValue > 0) %>%
    select(Scenario, Symbol, N0, N1, Value, AbsValue, ScenarioValue, BaseValue)
}

load_trade_nodes <- function(representative_path) {
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
}

load_sea_routes <- function(route_path) {
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
}

make_transport_diff_edges_sf <- function(df_link, nodes_tbl, sea_routes_sf) {
  if (nrow(df_link) == 0) {
    return(st_sf(df_link, geometry = st_sfc(crs = 4326)))
  }

  missing_nodes <- setdiff(unique(c(df_link$N0, df_link$N1)), nodes_tbl$N_PSY)
  if (length(missing_nodes) > 0) {
    stop("Missing transport node coordinates: ", paste(missing_nodes, collapse = ", "))
  }

  edges <- df_link %>%
    inner_join(nodes_tbl, by = c("N0" = "N_PSY")) %>%
    rename(x0 = lon, y0 = lat) %>%
    inner_join(nodes_tbl, by = c("N1" = "N_PSY")) %>%
    rename(x1 = lon, y1 = lat) %>%
    filter(!is.na(x0), !is.na(y0), !is.na(x1), !is.na(y1)) %>%
    filter(AbsValue > 0)

  if (nrow(edges) == 0) {
    return(st_sf(edges, geometry = st_sfc(crs = 4326)))
  }

  routes_tbl <- sea_routes_sf %>%
    mutate(route_key = paste(N0, N1, sep = "__"))
  edges <- edges %>%
    mutate(route_key = paste(N0, N1, sep = "__"))

  route_index <- match(edges$route_key, routes_tbl$route_key)
  missing_routes <- edges$route_key[is.na(route_index)]
  if (length(missing_routes) > 0) {
    stop("Missing sea routes: ", paste(unique(missing_routes), collapse = ", "))
  }

  st_sf(
    edges %>% select(-route_key),
    geometry = st_geometry(routes_tbl)[route_index],
    crs = st_crs(sea_routes_sf)
  )
}

make_link_capacity_diff_edges_sf <- function(df_link, nodes_tbl) {
  edges <- df_link %>%
    left_join(nodes_tbl, by = c("N0" = "N_PSY")) %>%
    rename(x0 = lon, y0 = lat) %>%
    left_join(nodes_tbl, by = c("N1" = "N_PSY")) %>%
    rename(x1 = lon, y1 = lat) %>%
    filter(!is.na(x0), !is.na(y0), !is.na(x1), !is.na(y1), AbsValue > 0)

  if (nrow(edges) == 0) {
    return(st_sf(edges, geometry = st_sfc(crs = 4326)))
  }

  lines_sfc <- pmap(
    edges[, c("x0", "y0", "x1", "y1")],
    ~ st_linestring(matrix(c(..1, ..2, ..3, ..4), ncol = 2, byrow = TRUE))
  )

  st_sf(edges, geometry = st_sfc(lines_sfc, crs = 4326))
}

normalize_longitude <- function(x) {
  ((x + 180) %% 360) - 180
}

route_coordinates_df <- function(edge_sf, route_id) {
  coords <- st_coordinates(st_geometry(edge_sf))
  coords_tbl <- as_tibble(coords) %>%
    mutate(
      route_id = route_id,
      x = normalize_longitude(X),
      y = Y,
      part = if ("L1" %in% names(.)) L1 else 1L,
      coord_order = row_number()
    )

  coords_tbl %>%
    group_by(part) %>%
    mutate(
      dateline_part = cumsum(c(FALSE, abs(diff(x)) > 180)),
      segment_id = paste(route_id, part, dateline_part, sep = "__")
    ) %>%
    ungroup()
}

make_sea_route_df <- function(edges_sf) {
  if (nrow(edges_sf) == 0) return(tibble())

  map_dfr(seq_len(nrow(edges_sf)), function(i) {
    edge_row <- edges_sf[i, ]
    edge_attr <- st_drop_geometry(edge_row)
    route_id <- paste(edge_attr$Scenario, edge_attr$Symbol, edge_attr$N0, edge_attr$N1, i, sep = "__")

    route_coordinates_df(edge_row, route_id) %>%
      transmute(
        route_id,
        segment_id,
        coord_order,
        x,
        y,
        Value = edge_attr$Value,
        Value_plot = edge_attr$Value_plot,
        N0 = edge_attr$N0,
        N1 = edge_attr$N1,
        Scenario = edge_attr$Scenario,
        Symbol = edge_attr$Symbol
      )
  }) %>%
    arrange(Value, route_id, segment_id, coord_order)
}

make_route_arrow_df <- function(edges_sf) {
  if (nrow(edges_sf) == 0) return(tibble())

  terminal_arrow_segment <- function(route_segment) {
    route_segment <- route_segment %>% arrange(coord_order)
    n_points <- nrow(route_segment)
    if (n_points < 2) return(NULL)

    reversed_segment <- route_segment %>%
      arrange(desc(coord_order)) %>%
      mutate(
        step_length = c(0, sqrt(diff(x)^2 + diff(y)^2)),
        distance_from_end = cumsum(step_length)
      )

    total_length <- max(reversed_segment$distance_from_end, na.rm = TRUE)
    if (!is.finite(total_length) || total_length == 0) return(NULL)

    target_length <- min(max(total_length * 0.35, 1.2), 6)
    if (total_length <= target_length) {
      p0 <- reversed_segment[n_points, ]
    } else {
      after_idx <- which(reversed_segment$distance_from_end >= target_length)[1]
      before_idx <- after_idx - 1
      before_point <- reversed_segment[before_idx, ]
      after_point <- reversed_segment[after_idx, ]
      segment_length <- after_point$distance_from_end - before_point$distance_from_end
      segment_share <- if (segment_length == 0) 0 else (target_length - before_point$distance_from_end) / segment_length
      p0 <- tibble(
        x = before_point$x + segment_share * (after_point$x - before_point$x),
        y = before_point$y + segment_share * (after_point$y - before_point$y)
      )
    }

    p1 <- reversed_segment[1, ]
    tibble(x = p0$x, y = p0$y, xend = p1$x, yend = p1$y)
  }

  map_dfr(seq_len(nrow(edges_sf)), function(i) {
    edge_row <- edges_sf[i, ]
    edge_attr <- st_drop_geometry(edge_row)
    route_id <- paste(edge_attr$Scenario, edge_attr$Symbol, edge_attr$N0, edge_attr$N1, i, sep = "__")

    route_df <- route_coordinates_df(edge_row, route_id) %>%
      arrange(coord_order)
    if (nrow(route_df) < 2) return(tibble())

    candidate_segments <- route_df %>%
      group_by(segment_id) %>%
      filter(n() >= 2) %>%
      ungroup()
    if (nrow(candidate_segments) < 2) return(tibble())

    last_segment <- candidate_segments %>%
      filter(segment_id == last(segment_id)) %>%
      arrange(coord_order)
    arrow_segment <- terminal_arrow_segment(last_segment)
    if (is.null(arrow_segment)) return(tibble())

    arrow_segment %>%
      mutate(
        Value = edge_attr$Value,
        Value_plot = edge_attr$Value_plot
      )
  }) %>%
    arrange(abs(Value))
}

add_trade_diff_arrows <- function(p, arrow_df) {
  if (nrow(arrow_df) == 0) return(p)

  arrow_df <- arrow_df %>%
    mutate(AbsValue = abs(Value))
  arrow_breaks <- quantile(arrow_df$AbsValue, probs = c(0.5, 0.85), na.rm = TRUE, names = FALSE)
  arrow_df <- arrow_df %>%
    mutate(
      ArrowGroup = case_when(
        AbsValue >= arrow_breaks[[2]] ~ "large",
        AbsValue >= arrow_breaks[[1]] ~ "medium",
        TRUE ~ "small"
      ),
      ArrowGroup = factor(ArrowGroup, levels = c("small", "medium", "large"))
    )

  arrow_styles <- tribble(
    ~ArrowGroup, ~HaloWidth, ~ArrowLength,
    "small",       0.35,       1.1,
    "medium",      0.65,       1.9,
    "large",       1.00,       3.0
  )

  for (i in seq_len(nrow(arrow_styles))) {
    arrow_group <- arrow_styles$ArrowGroup[i]
    arrow_data <- arrow_df %>% filter(ArrowGroup == arrow_group)
    if (nrow(arrow_data) == 0) next

    p <- p +
      geom_segment(
        data = arrow_data,
        aes(x = x, y = y, xend = xend, yend = yend),
        color = "white",
        linewidth = arrow_styles$HaloWidth[i],
        alpha = 0.7,
        lineend = "round",
        arrow = arrow(length = unit(arrow_styles$ArrowLength[i] + 0.25, "mm"), type = "closed"),
        show.legend = FALSE
      ) +
      geom_segment(
        data = arrow_data,
        aes(x = x, y = y, xend = xend, yend = yend, linewidth = Value_plot, color = Value),
        alpha = 0.86,
        lineend = "round",
        arrow = arrow(length = unit(arrow_styles$ArrowLength[i], "mm"), type = "closed")
      )
  }

  p
}

plot_trade_diff_flow <- function(
  edges_sf,
  scenario_name,
  base_scenario_name,
  overlay_spec,
  value_label = "annual flow (EJ/yr)",
  title_metric = "sea trade flow",
  show_arrows = TRUE,
  arrow_value_threshold = arrow_threshold,
  label_accuracy = 0.00001
) {
  if (nrow(edges_sf) == 0) {
    warning("No plottable ", title_metric, " differences for scenario: ", scenario_name, " / ", overlay_spec$Carrier)
    return(NULL)
  }

  vmax <- max(abs(edges_sf$Value), na.rm = TRUE)
  if (!is.finite(vmax) || vmax <= 0) return(NULL)
  brks <- pretty(c(0, vmax), n = 4)
  brks <- brks[brks > 0 & brks <= vmax]
  if (length(brks) == 0) brks <- vmax

  edges_plot_sf <- edges_sf %>%
    mutate(Value_plot = log10(1 + AbsValue))
  edges_plot_df <- edges_plot_sf %>%
    st_drop_geometry()
  edges_arrow_sf <- edges_plot_sf %>%
    filter(show_arrows, AbsValue > arrow_value_threshold)
  route_nodes_df <- bind_rows(
    edges_plot_df %>% transmute(N_PSY = N0, lon = x0, lat = y0),
    edges_plot_df %>% transmute(N_PSY = N1, lon = x1, lat = y1)
  ) %>%
    distinct(N_PSY, lon, lat)
  sea_route_line_df <- make_sea_route_df(edges_plot_sf)
  sea_route_arrow_df <- make_route_arrow_df(edges_arrow_sf)

  p <- ggplot() +
    geom_sf(data = region_boundaries, fill = "grey96", color = "grey45", linewidth = 0.35) +
    geom_path(
      data = sea_route_line_df,
      aes(x = x, y = y, group = segment_id),
      color = "white",
      linewidth = 0.25,
      alpha = 0.55,
      lineend = "round",
      show.legend = FALSE
    ) +
    geom_path(
      data = sea_route_line_df,
      aes(x = x, y = y, group = segment_id, linewidth = Value_plot, color = Value),
      alpha = 0.88,
      lineend = "round"
    )

  if (show_arrows) {
    p <- add_trade_diff_arrows(p, sea_route_arrow_df)
  }

  p <- p +
    geom_point(
      data = route_nodes_df,
      aes(x = lon, y = lat),
      shape = 21,
      size = 2.8,
      stroke = 0.55,
      color = "grey20",
      fill = "white",
      alpha = 0.95,
      inherit.aes = FALSE
    )

  p +
    scale_linewidth_continuous(
      range = c(0.05, 2.4),
      breaks = log10(1 + brks),
      labels = scales::label_number(accuracy = label_accuracy)(brks),
      name = paste0("|", scenario_name, " - ", base_scenario_name, "| ", value_label)
    ) +
    scale_color_gradient2(
      low = "#2166ac",
      mid = "#f7f7f7",
      high = "#b2182b",
      midpoint = 0,
      limits = c(-vmax, vmax),
      labels = scales::label_number(accuracy = label_accuracy),
      name = paste0(scenario_name, " - ", base_scenario_name, " ", value_label)
    ) +
    coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.box.spacing = unit(4, "mm"),
      legend.margin = margin(t = 0, r = 1, b = 0, l = 1, unit = "mm"),
      legend.background = element_blank(),
      legend.key.width = unit(16, "mm"),
      legend.key.height = unit(3, "mm"),
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 6.5),
      plot.title = element_text(hjust = 0.5, size = 12)
    ) +
    guides(
      color = guide_colorbar(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        barwidth = unit(60, "mm"),
        barheight = unit(3, "mm")
      ),
      linewidth = "none"
    ) +
    labs(
      title = paste0(
        scenario_name,
        " - ",
        base_scenario_name,
        " ",
        overlay_spec$TitleLabel,
        " ",
        title_metric,
        " difference (",
        plot_year,
        ")"
      )
    )
}

plot_link_capacity_diff_flow <- function(edges_sf, scenario_name, base_scenario_name, capacity_spec) {
  if (nrow(edges_sf) == 0) {
    warning("No plottable vfs_l capacity differences for scenario: ", scenario_name, " / ", capacity_spec$LinkType)
    return(NULL)
  }

  vmax <- max(abs(edges_sf$Value), na.rm = TRUE)
  if (!is.finite(vmax) || vmax <= 0) return(NULL)
  brks <- pretty(c(0, vmax), n = 4)
  brks <- brks[brks > 0 & brks <= vmax]
  if (length(brks) == 0) brks <- vmax

  edges_plot <- edges_sf %>%
    mutate(Value_plot = log10(1 + AbsValue))

  ggplot() +
    geom_sf(data = region_boundaries, fill = "grey96", color = "grey45", linewidth = 0.35) +
    geom_sf(
      data = edges_plot,
      aes(linewidth = Value_plot, color = Value),
      alpha = 0.88
    ) +
    scale_linewidth_continuous(
      range = c(0.10, 2.4),
      breaks = log10(1 + brks),
      labels = scales::label_number(accuracy = 0.01)(brks),
      name = paste0("|", scenario_name, " - ", base_scenario_name, "| vfs_l capacity (GW)")
    ) +
    scale_color_gradient2(
      low = "#2166ac",
      mid = "#f7f7f7",
      high = "#b2182b",
      midpoint = 0,
      limits = c(-vmax, vmax),
      labels = scales::label_number(accuracy = 0.01),
      name = paste0(scenario_name, " - ", base_scenario_name, " vfs_l capacity (GW)")
    ) +
    coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.box.spacing = unit(4, "mm"),
      legend.margin = margin(t = 0, r = 1, b = 0, l = 1, unit = "mm"),
      legend.background = element_blank(),
      legend.key.width = unit(16, "mm"),
      legend.key.height = unit(3, "mm"),
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 6.5),
      plot.title = element_text(hjust = 0.5, size = 12)
    ) +
    guides(
      color = guide_colorbar(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        barwidth = unit(60, "mm"),
        barheight = unit(3, "mm")
      ),
      linewidth = "none"
    ) +
    labs(
      title = paste0(
        scenario_name,
        " - ",
        base_scenario_name,
        " ",
        capacity_spec$TitleLabel,
        " vfs_l capacity difference (",
        plot_year,
        ")"
      )
    )
}

trade_port_nodes_tbl <- load_trade_nodes(searoute_representative_path)
link_nodes_tbl <- load_link_nodes(node_path, node_csv)
sea_routes_sf <- load_sea_routes(searoute_path)

for (i in seq_len(nrow(overlay_specs))) {
  overlay_spec <- overlay_specs[i, ]

  for (scenario_name in scenarios) {
    diff_dir <- file.path(scenario_analysis_dir, overlay_spec$Folder, paste0("difference_from_", base_scenario))
    dir.create(diff_dir, recursive = TRUE, showWarnings = FALSE)

    edges_diff_sf <- build_transport_diff_values(
      scenario_name = scenario_name,
      base_scenario_name = base_scenario,
      overlay_spec = overlay_spec
    ) %>%
      make_transport_diff_edges_sf(trade_port_nodes_tbl, sea_routes_sf)

    p_diff <- plot_trade_diff_flow(
      edges_sf = edges_diff_sf,
      scenario_name = scenario_name,
      base_scenario_name = base_scenario,
      overlay_spec = overlay_spec
    )

    if (!is.null(p_diff)) {
      ggsave(
        filename = file.path(
          diff_dir,
          paste0(
            "global_",
            scenario_name,
            "_minus_",
            base_scenario,
            "_vff_l_vfb_l_combined_",
            overlay_spec$Carrier,
            "_trade_difference_",
            plot_year,
            ".png"
          )
        ),
        plot = p_diff,
        width = 300,
        height = 150,
        units = "mm",
        dpi = 300,
        bg = "white"
      )
    }

    edges_capacity_diff_sf <- build_transport_capacity_diff_values(
      scenario_name = scenario_name,
      base_scenario_name = base_scenario,
      overlay_spec = overlay_spec
    ) %>%
      make_transport_diff_edges_sf(trade_port_nodes_tbl, sea_routes_sf)

    p_capacity_diff <- plot_trade_diff_flow(
      edges_sf = edges_capacity_diff_sf,
      scenario_name = scenario_name,
      base_scenario_name = base_scenario,
      overlay_spec = overlay_spec,
      value_label = "vfs_l capacity (GW)",
      title_metric = "vfs_l transport capacity",
      show_arrows = FALSE,
      label_accuracy = 0.01
    )

    if (!is.null(p_capacity_diff)) {
      ggsave(
        filename = file.path(
          diff_dir,
          paste0(
            "global_",
            scenario_name,
            "_minus_",
            base_scenario,
            "_vfs_l_",
            overlay_spec$Carrier,
            "_capacity_difference_",
            plot_year,
            ".png"
          )
        ),
        plot = p_capacity_diff,
        width = 300,
        height = 150,
        units = "mm",
        dpi = 300,
        bg = "white"
      )
    }
  }
}

for (i in seq_len(nrow(transmission_capacity_specs))) {
  capacity_spec <- transmission_capacity_specs[i, ]

  for (scenario_name in scenarios) {
    diff_dir <- file.path(scenario_analysis_dir, capacity_spec$Folder, paste0("difference_from_", base_scenario))
    dir.create(diff_dir, recursive = TRUE, showWarnings = FALSE)

    edges_capacity_diff_sf <- build_link_capacity_diff_values(
      scenario_name = scenario_name,
      base_scenario_name = base_scenario,
      capacity_spec = capacity_spec
    ) %>%
      make_link_capacity_diff_edges_sf(link_nodes_tbl)

    p_capacity_diff <- plot_link_capacity_diff_flow(
      edges_sf = edges_capacity_diff_sf,
      scenario_name = scenario_name,
      base_scenario_name = base_scenario,
      capacity_spec = capacity_spec
    )

    if (!is.null(p_capacity_diff)) {
      ggsave(
        filename = file.path(
          diff_dir,
          paste0(
            "global_",
            scenario_name,
            "_minus_",
            base_scenario,
            "_vfs_l_",
            capacity_spec$LinkType,
            "_transmission_capacity_difference_",
            plot_year,
            ".png"
          )
        ),
        plot = p_capacity_diff,
        width = 300,
        height = 150,
        units = "mm",
        dpi = 300,
        bg = "white"
      )
    }
  }
}
