# Integrated hydrogen and ammonia network plots ------------------------------

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

read_gdx_param <- if (exists("trading_read_gdx_param", inherits = TRUE)) {
  trading_read_gdx_param
} else {
  function(gdx_path, symbol) {
    if (!file.exists(gdx_path)) {
      stop("Missing GDX file: ", gdx_path)
    }
    rgdx.param(gdx_path, symbol)
  }
}

for (gams_sys_dir in c("C:/GAMS/38", "C:/GAMS/win64/26.1")) {
  if (dir.exists(gams_sys_dir)) {
    igdx(gams_sys_dir)
    break
  }
}

aimpsy <- if (exists("trading_aimpsy_dir", inherits = TRUE)) trading_aimpsy_dir else "//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output"
scenario_group <- "global_softlink_Sc"
region <- "Global"
date <- "2606171718"
plot_year <- 2050
scenarios <- c("NPi500-LimCCS_bio")

ddir <- file.path(aimpsy, scenario_group, region, date)
prm_dir <- file.path(ddir, "gams_output", "gdx_primary")
main_dir <- file.path(ddir, "main")
merged_gdx_path <- file.path(main_dir, "merged_output.gdx")
base_dir <- if (exists("trading_base_dir", inherits = TRUE)) trading_base_dir else file.path("output", "global_260617")

node_path <- if (exists("trading_node_geojson_path", inherits = TRUE)) {
  trading_node_geojson_path
} else {
  "C:/Users/Haru Sakata/OneDrive - Kyoto University/My work/AIM-PSY/data/node/nodes_centroid.geojson"
}
searoute_dir <- if (exists("trading_searoute_dir", inherits = TRUE)) {
  trading_searoute_dir
} else {
  "C:/Users/Haru Sakata/OneDrive - Kyoto University/My work/AIM-PSY/data/searoute"
}
searoute_path <- file.path(searoute_dir, "searoute.geojson")
searoute_representative_path <- file.path(searoute_dir, "searoute_r31_representative.geojson")

arrow_threshold <- 10
h2_color_low <- "#3182bd"
h2_color <- "#08306b"
nh3_color <- "#b2186b"
syf_color <- "#018571"

script_dir <- {
  file_arg <- commandArgs(trailingOnly = FALSE) %>%
    str_subset("^--file=") %>%
    str_remove("^--file=")

  if (length(file_arg) > 0) {
    dirname(normalizePath(file_arg[[1]], winslash = "/", mustWork = FALSE))
  } else {
    normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  }
}

source_trade_region_functions <- function(path = file.path(script_dir, "plot_trade.R")) {
  if (!file.exists(path)) {
    fallback_paths <- c(
      file.path(script_dir, "code", "plot_trade.R"),
      file.path(getwd(), "code", "plot_trade.R"),
      file.path("master", "code", "plot_trade.R")
    )
    existing_fallback <- fallback_paths[file.exists(fallback_paths)][1]
    if (!is.na(existing_fallback)) {
      path <- existing_fallback
    } else {
      stop(
        "Could not find plot_trade.R at: ",
        paste(c(path, fallback_paths), collapse = " or ")
      )
    }
  }

  lines <- readLines(path, warn = FALSE)
  start <- grep("^build_region_map <- function", lines)[1]
  end <- grep("^region_boundaries <-", lines)[1] - 1
  if (is.na(start) || is.na(end) || end <= start) {
    stop("Could not locate region boundary functions in: ", path)
  }

  eval(parse(text = paste(lines[start:end], collapse = "\n")), envir = .GlobalEnv)
}

if (!exists("trading_region_boundaries", inherits = TRUE)) {
  source_trade_region_functions()
}
region_boundaries <- if (exists("trading_region_boundaries", inherits = TRUE)) {
  trading_region_boundaries
} else {
  build_region_boundaries()
}
gwh_to_ej <- 3.6e-6
time_slice_hours <- 6
annual_flow_multiplier <- time_slice_hours

network_flow_specs <- tribble(
  ~Symbol, ~Direction,
  "vff_l", "forward",
  "vfb_l", "backward"
)

read_h2_link_value <- function(scenario_name, flow_spec) {
  gdx_path <- file.path(prm_dir, scenario_name, paste0(plot_year, ".gdx"))
  if (!file.exists(gdx_path)) {
    stop("Missing GDX file: ", gdx_path)
  }

  value_symbol <- flow_spec$Symbol
  direction <- flow_spec$Direction

  read_gdx_param(gdx_path, value_symbol) %>%
    mutate(across(where(is.factor), as.character)) %>%
    filter(str_detect(L, ".*-H2_.*-H2$")) %>%
    group_by(L) %>%
    summarise(Value = sum(.data[[value_symbol]], na.rm = TRUE), .groups = "drop") %>%
    mutate(Value = Value * annual_flow_multiplier * gwh_to_ej) %>%
    filter(Value > 0) %>%
    separate(L, sep = "_", into = c("N0", "N1"), remove = FALSE) %>%
    mutate(
      Scenario = scenario_name,
      Symbol = value_symbol,
      N0 = N0 %>% str_remove_all(fixed("-H2")) %>% str_remove("^R31-"),
      N1 = N1 %>% str_remove_all(fixed("-H2")) %>% str_remove("^R31-"),
      N0_plot = if (direction == "backward") N1 else N0,
      N1_plot = if (direction == "backward") N0 else N1,
      N0 = N0_plot,
      N1 = N1_plot
    ) %>%
    select(-N0_plot, -N1_plot)
}

read_nh3_trade_value <- function(scenario_name, flow_spec) {
  gdx_path <- file.path(prm_dir, scenario_name, paste0(plot_year, ".gdx"))
  if (!file.exists(gdx_path)) {
    stop("Missing GDX file: ", gdx_path)
  }

  value_symbol <- flow_spec$Symbol
  direction <- flow_spec$Direction

  df_link <- read_gdx_param(gdx_path, value_symbol) %>%
    mutate(across(where(is.factor), as.character)) %>%
    filter(str_detect(L, "^R31-[A-Z0-9]+-NH3_R31-[A-Z0-9]+-NH3$")) %>%
    group_by(L) %>%
    summarise(Value = sum(.data[[value_symbol]], na.rm = TRUE), .groups = "drop") %>%
    mutate(Value = Value * annual_flow_multiplier * gwh_to_ej) %>%
    filter(Value > 0)

  if (nrow(df_link) == 0) {
    return(tibble(L = character(), Value = numeric(), Scenario = character(), Symbol = character(), N0 = character(), N1 = character()))
  }

  link_nodes <- str_match(df_link$L, "^R31-([A-Z0-9]+)-NH3_R31-([A-Z0-9]+)-NH3$")

  df_link %>%
    mutate(
      Scenario = scenario_name,
      Symbol = value_symbol,
      N0 = link_nodes[, 2],
      N1 = link_nodes[, 3],
      N0_plot = if (direction == "backward") N1 else N0,
      N1_plot = if (direction == "backward") N0 else N1,
      N0 = N0_plot,
      N1 = N1_plot
    ) %>%
    select(-N0_plot, -N1_plot)
}

read_syf_trade_value <- function(scenario_name, flow_spec) {
  gdx_path <- file.path(prm_dir, scenario_name, paste0(plot_year, ".gdx"))
  if (!file.exists(gdx_path)) {
    stop("Missing GDX file: ", gdx_path)
  }

  value_symbol <- flow_spec$Symbol
  direction <- flow_spec$Direction

  df_link <- read_gdx_param(gdx_path, value_symbol) %>%
    mutate(across(where(is.factor), as.character)) %>%
    filter(str_detect(L, "^R31-[A-Z0-9]+-SYF_R31-[A-Z0-9]+-SYF$")) %>%
    group_by(L) %>%
    summarise(Value = sum(.data[[value_symbol]], na.rm = TRUE), .groups = "drop") %>%
    mutate(Value = Value * annual_flow_multiplier * gwh_to_ej) %>%
    filter(Value > 0)

  if (nrow(df_link) == 0) {
    return(tibble(L = character(), Value = numeric(), Scenario = character(), Symbol = character(), N0 = character(), N1 = character()))
  }

  link_nodes <- str_match(df_link$L, "^R31-([A-Z0-9]+)-SYF_R31-([A-Z0-9]+)-SYF$")

  df_link %>%
    mutate(
      Scenario = scenario_name,
      Symbol = value_symbol,
      N0 = link_nodes[, 2],
      N1 = link_nodes[, 3],
      N0_plot = if (direction == "backward") N1 else N0,
      N1_plot = if (direction == "backward") N0 else N1,
      N0 = N0_plot,
      N1 = N1_plot
    ) %>%
    select(-N0_plot, -N1_plot)
}

load_h2_nodes <- function(path) {
  if (!file.exists(path)) {
    stop("Missing H2 node GeoJSON: ", path)
  }

  st_read(path, quiet = TRUE) %>%
    st_drop_geometry() %>%
    transmute(
      N_PSY = str_remove(as.character(N_PSY), "^R31-"),
      lon = as.numeric(centroid_x),
      lat = as.numeric(centroid_y)
    )
}

load_trade_nodes <- function(path) {
  if (!file.exists(path)) {
    stop("Missing sea-route representative node GeoJSON: ", path)
  }

  st_read(path, quiet = TRUE) %>%
    st_drop_geometry() %>%
    transmute(
      N_PSY = str_remove(as.character(R31), "^R31-"),
      lon = as.numeric(Longitude),
      lat = as.numeric(Latitude)
    )
}

load_sea_routes <- function(path) {
  if (!file.exists(path)) {
    stop("Missing sea-route GeoJSON: ", path)
  }

  st_read(path, quiet = TRUE) %>%
    st_set_crs(4326) %>%
    transmute(
      N0 = str_remove(as.character(R31_from), "^R31-"),
      N1 = str_remove(as.character(R31_to), "^R31-"),
      geometry = geometry
    )
}

make_h2_edges_sf <- function(df_link, nodes_tbl) {
  edges <- df_link %>%
    left_join(nodes_tbl, by = c("N0" = "N_PSY")) %>%
    rename(x0 = lon, y0 = lat) %>%
    left_join(nodes_tbl, by = c("N1" = "N_PSY")) %>%
    rename(x1 = lon, y1 = lat) %>%
    filter(!is.na(x0), !is.na(y0), !is.na(x1), !is.na(y1), Value > 0)

  if (nrow(edges) == 0) {
    return(st_sf(edges, geometry = st_sfc(crs = 4326)))
  }

  lines_sfc <- pmap(
    edges[, c("x0", "y0", "x1", "y1")],
    ~ st_linestring(matrix(c(..1, ..2, ..3, ..4), ncol = 2, byrow = TRUE))
  )

  st_sf(edges, geometry = st_sfc(lines_sfc, crs = 4326))
}

make_nh3_edges_sf <- function(df_link, nodes_tbl, sea_routes_sf) {
  edges <- df_link %>%
    left_join(nodes_tbl, by = c("N0" = "N_PSY")) %>%
    rename(x0 = lon, y0 = lat) %>%
    left_join(nodes_tbl, by = c("N1" = "N_PSY")) %>%
    rename(x1 = lon, y1 = lat) %>%
    filter(!is.na(x0), !is.na(y0), !is.na(x1), !is.na(y1), Value > 0) %>%
    mutate(route_key = paste(N0, N1, sep = "__"))

  if (nrow(edges) == 0) {
    return(st_sf(edges, geometry = st_sfc(crs = 4326)))
  }

  routes_tbl <- sea_routes_sf %>%
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

read_h2_demand_values <- function(gdx_path) {
  if (!file.exists(gdx_path)) {
    stop("Missing GDX file: ", gdx_path)
  }

  read_gdx_param(gdx_path, "iamc_gdx") %>%
    transmute(
      Scenario = as.character(Sc),
      Region = str_remove(as.character(Sr), "^R31-"),
      Variable = as.character(Sv),
      Year = as.numeric(as.character(Y5)),
      Value = suppressWarnings(as.numeric(as.character(iamc_gdx)))
    ) %>%
    filter(
      Scenario %in% scenarios,
      Region %in% region_boundaries$region,
      Variable == "Sec_Ene_Hyd",
      Year == plot_year,
      !is.na(Value)
    ) %>%
    group_by(Scenario, Region, Year) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")
}

build_h2_demand_map_data <- function(demand_values) {
  map_index <- tidyr::expand_grid(
    Scenario = scenarios,
    region = region_boundaries$region
  ) %>%
    left_join(demand_values, by = c("Scenario", "region" = "Region"))

  map_index %>%
    left_join(region_boundaries %>% select(region, geometry), by = "region") %>%
    st_as_sf(crs = st_crs(region_boundaries))
}

fixed_fill_limits <- function(values) {
  vmax <- max(values, na.rm = TRUE)
  if (!is.finite(vmax) || vmax <= 0) return(NULL)
  c(0, vmax)
}

log_value_breaks <- function(values) {
  vmax <- max(values, na.rm = TRUE)
  if (!is.finite(vmax) || vmax <= 0) return(1)

  if (vmax < 1) {
    brks <- pretty(c(0, vmax), n = 4)
    return(brks[brks > 0 & brks <= vmax])
  }

  brks <- 10^seq(0, ceiling(log10(vmax)), by = 1)
  brks <- brks[brks <= vmax]
  if (length(brks) == 0) brks <- pretty(c(0, vmax), n = 4)
  brks
}

make_common_flow_scale <- function(values) {
  vmax <- max(c(values, 1), na.rm = TRUE)
  if (!is.finite(vmax) || vmax <= 0) vmax <- 1

  list(
    limits = c(0, log10(1 + vmax)),
    breaks = log_value_breaks(c(values, 1))
  )
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
  if (nrow(edges_sf) == 0) {
    return(tibble(
      route_id = character(),
      segment_id = character(),
      coord_order = integer(),
      x = numeric(),
      y = numeric(),
      Value = numeric(),
      Value_plot = numeric()
    ))
  }

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
        Value_plot = edge_attr$Value_plot
      )
  }) %>%
    arrange(Value, route_id, segment_id, coord_order)
}

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

make_nh3_arrow_df <- function(edges_sf) {
  empty_arrow_df <- tibble(
    x = numeric(),
    y = numeric(),
    xend = numeric(),
    yend = numeric(),
    Value = numeric(),
    Value_plot = numeric()
  )

  if (nrow(edges_sf) == 0) return(empty_arrow_df)

  arrow_df <- map_dfr(seq_len(nrow(edges_sf)), function(i) {
    edge_row <- edges_sf[i, ]
    edge_attr <- st_drop_geometry(edge_row)
    route_id <- paste(edge_attr$Scenario, edge_attr$Symbol, edge_attr$N0, edge_attr$N1, i, sep = "__")
    route_df <- route_coordinates_df(edge_row, route_id)

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
      mutate(Value = edge_attr$Value, Value_plot = edge_attr$Value_plot)
  }) %>%
    arrange(Value)

  if (nrow(arrow_df) == 0) empty_arrow_df else arrow_df
}

plot_network_overlay <- function(
  h2_edges_sf,
  sea_edges_sf,
  h2_demand_map,
  scenario_name,
  sea_label,
  sea_color,
  sea_linetype = "solid",
  flow_scale
) {
  if (nrow(h2_edges_sf) == 0 && nrow(sea_edges_sf) == 0) {
    warning("No plottable network links for scenario: ", scenario_name, " / ", sea_label)
    return(NULL)
  }

  h2_plot_df <- h2_edges_sf %>%
    mutate(Value_plot = log10(1 + Value)) %>%
    st_drop_geometry()
  if (nrow(h2_plot_df) > 0) {
    h2_color_scale <- scales::col_numeric(
      palette = c(h2_color_low, h2_color),
      domain = range(h2_plot_df$Value_plot, na.rm = TRUE)
    )
    h2_plot_df <- h2_plot_df %>%
      mutate(FlowColor = h2_color_scale(Value_plot))
  } else {
    h2_plot_df <- h2_plot_df %>%
      mutate(FlowColor = h2_color)
  }

  arrow_threshold_ej <- arrow_threshold * annual_flow_multiplier * gwh_to_ej

  sea_plot_sf <- sea_edges_sf %>%
    mutate(Value_plot = log10(1 + Value))
  sea_route_df <- make_sea_route_df(sea_plot_sf)
  sea_arrow_df <- make_nh3_arrow_df(sea_plot_sf %>% filter(Value > arrow_threshold_ej))

  trade_nodes_df <- sea_plot_sf %>%
    st_drop_geometry() %>%
    transmute(N0, N1, x0, y0, x1, y1) %>%
    { bind_rows(
      transmute(., N_PSY = N0, lon = x0, lat = y0),
      transmute(., N_PSY = N1, lon = x1, lat = y1)
    ) } %>%
    distinct(N_PSY, lon, lat)

  plot_map_data <- h2_demand_map %>%
    filter(Scenario == scenario_name)
  carrier_legend_df <- tibble(
    Carrier = factor(c("Hydrogen pipeline", sea_label),
                     levels = c("Hydrogen pipeline", sea_label)),
    x = 0,
    y = 0
  )

  ggplot() +
    geom_sf(data = plot_map_data, fill = "grey96", color = "grey45", linewidth = 0.25) +
    geom_segment(
      data = h2_plot_df,
      aes(x = x0, y = y0, xend = x1, yend = y1, linewidth = Value_plot),
      color = h2_plot_df$FlowColor,
      alpha = 0.96,
      lineend = "round"
    ) +
    geom_path(
      data = sea_route_df,
      aes(x = x, y = y, group = segment_id),
      color = "white",
      linewidth = if (sea_linetype == "solid") 0.65 else 0.95,
      alpha = if (sea_linetype == "solid") 0.7 else 0.86,
      linetype = sea_linetype,
      lineend = "round",
      show.legend = FALSE
    ) +
    geom_path(
      data = sea_route_df,
      aes(x = x, y = y, group = segment_id, linewidth = Value_plot),
      color = sea_color,
      alpha = 0.98,
      linetype = sea_linetype,
      lineend = "round"
    ) +
    geom_segment(
      data = sea_arrow_df,
      aes(x = x, y = y, xend = xend, yend = yend),
      color = "white",
      linewidth = if (sea_linetype == "solid") 1.35 else 1.55,
      alpha = if (sea_linetype == "solid") 0.78 else 0.86,
      linetype = sea_linetype,
      lineend = "round",
      arrow = arrow(length = unit(2.15, "mm"), type = "closed"),
      show.legend = FALSE
    ) +
    geom_segment(
      data = sea_arrow_df,
      aes(x = x, y = y, xend = xend, yend = yend, linewidth = Value_plot),
      color = sea_color,
      alpha = 0.98,
      linetype = sea_linetype,
      lineend = "round",
      arrow = arrow(length = unit(1.95, "mm"), type = "closed")
    ) +
    geom_point(
      data = trade_nodes_df,
      aes(x = lon, y = lat),
      shape = 21,
      size = 2.8,
      stroke = 0.55,
      color = "grey20",
      fill = "white",
      alpha = 0.95,
      inherit.aes = FALSE
    ) +
    geom_point(
      data = carrier_legend_df,
      aes(x = x, y = y, color = Carrier),
      alpha = 0,
      inherit.aes = FALSE,
      show.legend = TRUE
    ) +
    scale_linewidth_continuous(
      range = c(0.25, 4.4),
      limits = flow_scale$limits,
      breaks = log10(1 + flow_scale$breaks),
      labels = scales::label_number(accuracy = 0.00001)(flow_scale$breaks),
      name = "Network flow (EJ/yr)"
    ) +
    scale_color_manual(
      values = c(
        "Hydrogen pipeline" = h2_color,
        setNames(sea_color, sea_label)
      ),
      name = "Carrier"
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
      linewidth = guide_legend(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5
      ),
      color = guide_legend(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        override.aes = list(alpha = 1, size = 3)
      )
    ) +
    labs(
      title = paste0(
        scenario_name,
        " hydrogen and ",
        tolower(sea_label),
        " network flows (",
        plot_year,
        ")"
      )
    )
}

plot_h2_network_single <- function(h2_edges_sf, h2_demand_map, scenario_name, flow_scale) {
  if (nrow(h2_edges_sf) == 0) {
    warning("No plottable hydrogen network links for scenario: ", scenario_name)
    return(NULL)
  }

  h2_plot_df <- h2_edges_sf %>%
    mutate(Value_plot = log10(1 + Value)) %>%
    st_drop_geometry()

  h2_color_scale <- scales::col_numeric(
    palette = c(h2_color_low, h2_color),
    domain = range(h2_plot_df$Value_plot, na.rm = TRUE)
  )
  h2_plot_df <- h2_plot_df %>%
    mutate(FlowColor = h2_color_scale(Value_plot))

  plot_map_data <- h2_demand_map %>%
    filter(Scenario == scenario_name)

  ggplot() +
    geom_sf(data = plot_map_data, fill = "grey96", color = "grey45", linewidth = 0.25) +
    geom_segment(
      data = h2_plot_df,
      aes(x = x0, y = y0, xend = x1, yend = y1, linewidth = Value_plot),
      color = h2_plot_df$FlowColor,
      alpha = 0.96,
      lineend = "round"
    ) +
    scale_linewidth_continuous(
      range = c(0.25, 4.4),
      limits = flow_scale$limits,
      breaks = log10(1 + flow_scale$breaks),
      labels = scales::label_number(accuracy = 0.00001)(flow_scale$breaks),
      name = "Hydrogen pipeline flow (EJ/yr)"
    ) +
    coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.background = element_blank(),
      legend.key.width = unit(16, "mm"),
      legend.key.height = unit(3, "mm"),
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 6.5),
      plot.title = element_text(hjust = 0.5, size = 12)
    ) +
    guides(
      linewidth = guide_legend(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    labs(title = paste0(scenario_name, " hydrogen pipeline network flows (", plot_year, ")"))
}

plot_sea_network_single <- function(
  sea_edges_sf,
  h2_demand_map,
  scenario_name,
  sea_label,
  sea_color,
  sea_linetype = "solid",
  flow_scale
) {
  if (nrow(sea_edges_sf) == 0) {
    warning("No plottable sea network links for scenario: ", scenario_name, " / ", sea_label)
    return(NULL)
  }

  arrow_threshold_ej <- arrow_threshold * annual_flow_multiplier * gwh_to_ej
  sea_plot_sf <- sea_edges_sf %>%
    mutate(Value_plot = log10(1 + Value))
  sea_route_df <- make_sea_route_df(sea_plot_sf)
  sea_arrow_df <- make_nh3_arrow_df(sea_plot_sf %>% filter(Value > arrow_threshold_ej))
  trade_nodes_df <- sea_plot_sf %>%
    st_drop_geometry() %>%
    transmute(N0, N1, x0, y0, x1, y1) %>%
    { bind_rows(
      transmute(., N_PSY = N0, lon = x0, lat = y0),
      transmute(., N_PSY = N1, lon = x1, lat = y1)
    ) } %>%
    distinct(N_PSY, lon, lat)

  plot_map_data <- h2_demand_map %>%
    filter(Scenario == scenario_name)
  carrier_legend_df <- tibble(
    Carrier = factor(sea_label, levels = sea_label),
    x = 0,
    y = 0
  )

  ggplot() +
    geom_sf(data = plot_map_data, fill = "grey96", color = "grey45", linewidth = 0.25) +
    geom_path(
      data = sea_route_df,
      aes(x = x, y = y, group = segment_id),
      color = "white",
      linewidth = if (sea_linetype == "solid") 0.65 else 0.95,
      alpha = if (sea_linetype == "solid") 0.7 else 0.86,
      linetype = sea_linetype,
      lineend = "round",
      show.legend = FALSE
    ) +
    geom_path(
      data = sea_route_df,
      aes(x = x, y = y, group = segment_id, linewidth = Value_plot),
      color = sea_color,
      alpha = 0.98,
      linetype = sea_linetype,
      lineend = "round"
    ) +
    geom_segment(
      data = sea_arrow_df,
      aes(x = x, y = y, xend = xend, yend = yend),
      color = "white",
      linewidth = if (sea_linetype == "solid") 1.35 else 1.55,
      alpha = if (sea_linetype == "solid") 0.78 else 0.86,
      linetype = sea_linetype,
      lineend = "round",
      arrow = arrow(length = unit(2.15, "mm"), type = "closed"),
      show.legend = FALSE
    ) +
    geom_segment(
      data = sea_arrow_df,
      aes(x = x, y = y, xend = xend, yend = yend, linewidth = Value_plot),
      color = sea_color,
      alpha = 0.98,
      linetype = sea_linetype,
      lineend = "round",
      arrow = arrow(length = unit(1.95, "mm"), type = "closed")
    ) +
    geom_point(
      data = trade_nodes_df,
      aes(x = lon, y = lat),
      shape = 21,
      size = 2.8,
      stroke = 0.55,
      color = "grey20",
      fill = "white",
      alpha = 0.95,
      inherit.aes = FALSE
    ) +
    geom_point(
      data = carrier_legend_df,
      aes(x = x, y = y, color = Carrier),
      alpha = 0,
      inherit.aes = FALSE,
      show.legend = TRUE
    ) +
    scale_linewidth_continuous(
      range = c(0.25, 4.4),
      limits = flow_scale$limits,
      breaks = log10(1 + flow_scale$breaks),
      labels = scales::label_number(accuracy = 0.00001)(flow_scale$breaks),
      name = "Network flow (EJ/yr)"
    ) +
    scale_color_manual(values = setNames(sea_color, sea_label), name = "Carrier") +
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
      linewidth = guide_legend(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5
      ),
      color = guide_legend(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        override.aes = list(alpha = 1, size = 3)
      )
    ) +
    labs(title = paste0(scenario_name, " ", tolower(sea_label), " network flows (", plot_year, ")"))
}

save_network_plot <- function(plot, network_dir, filename) {
  if (is.null(plot)) return(invisible(FALSE))

  ggsave(
    filename = file.path(network_dir, filename),
    plot = plot,
    width = 300,
    height = 150,
    units = "mm",
    dpi = 300,
    bg = "white"
  )

  invisible(TRUE)
}

h2_nodes_tbl <- if (exists("trading_link_nodes_tbl", inherits = TRUE)) {
  trading_link_nodes_tbl
} else {
  load_h2_nodes(node_path)
}
nh3_nodes_tbl <- if (exists("trading_trade_port_nodes_tbl", inherits = TRUE)) {
  trading_trade_port_nodes_tbl
} else {
  load_trade_nodes(searoute_representative_path)
}
sea_routes_sf <- if (exists("trading_sea_routes_sf", inherits = TRUE)) {
  trading_sea_routes_sf
} else {
  load_sea_routes(searoute_path)
}
h2_demand_map <- tidyr::expand_grid(
  Scenario = scenarios,
  region = region_boundaries$region
) %>%
  left_join(region_boundaries %>% select(region, geometry), by = "region") %>%
  st_as_sf(crs = st_crs(region_boundaries))

for (scenario_name in scenarios) {
  network_dir <- file.path(base_dir, scenario_name, "plot_network")
  dir.create(network_dir, recursive = TRUE, showWarnings = FALSE)

  h2_edges_sf <- map_dfr(seq_len(nrow(network_flow_specs)), function(i) {
    read_h2_link_value(scenario_name, network_flow_specs[i, ])
  }) %>%
    make_h2_edges_sf(h2_nodes_tbl)

  nh3_edges_sf <- map_dfr(seq_len(nrow(network_flow_specs)), function(i) {
    read_nh3_trade_value(scenario_name, network_flow_specs[i, ])
  }) %>%
    make_nh3_edges_sf(nh3_nodes_tbl, sea_routes_sf)

  syf_edges_sf <- map_dfr(seq_len(nrow(network_flow_specs)), function(i) {
    read_syf_trade_value(scenario_name, network_flow_specs[i, ])
  }) %>%
    make_nh3_edges_sf(nh3_nodes_tbl, sea_routes_sf)

  flow_scale <- make_common_flow_scale(c(h2_edges_sf$Value, nh3_edges_sf$Value, syf_edges_sf$Value))

  save_network_plot(
    plot_h2_network_single(
      h2_edges_sf = h2_edges_sf,
      h2_demand_map = h2_demand_map,
      scenario_name = scenario_name,
      flow_scale = flow_scale
    ),
    network_dir,
    paste0("global_", scenario_name, "_h2_network_", plot_year, ".png")
  )

  save_network_plot(
    plot_sea_network_single(
      sea_edges_sf = nh3_edges_sf,
      h2_demand_map = h2_demand_map,
      scenario_name = scenario_name,
      sea_label = "Ammonia sea trade",
      sea_color = nh3_color,
      sea_linetype = "solid",
      flow_scale = flow_scale
    ),
    network_dir,
    paste0("global_", scenario_name, "_nh3_network_", plot_year, ".png")
  )

  save_network_plot(
    plot_sea_network_single(
      sea_edges_sf = syf_edges_sf,
      h2_demand_map = h2_demand_map,
      scenario_name = scenario_name,
      sea_label = "Synfuel sea trade",
      sea_color = syf_color,
      sea_linetype = "48",
      flow_scale = flow_scale
    ),
    network_dir,
    paste0("global_", scenario_name, "_syf_network_", plot_year, ".png")
  )

  p_nh3 <- plot_network_overlay(
    h2_edges_sf = h2_edges_sf,
    sea_edges_sf = nh3_edges_sf,
    h2_demand_map = h2_demand_map,
    scenario_name = scenario_name,
    sea_label = "Ammonia sea trade",
    sea_color = nh3_color,
    sea_linetype = "solid",
    flow_scale = flow_scale
  )
  save_network_plot(
    p_nh3,
    network_dir,
    paste0("global_", scenario_name, "_h2_nh3_network_overlay_", plot_year, ".png")
  )

  p_syf <- plot_network_overlay(
    h2_edges_sf = h2_edges_sf,
    sea_edges_sf = syf_edges_sf,
    h2_demand_map = h2_demand_map,
    scenario_name = scenario_name,
    sea_label = "Synfuel sea trade",
    sea_color = syf_color,
    sea_linetype = "48",
    flow_scale = flow_scale
  )
  save_network_plot(
    p_syf,
    network_dir,
    paste0("global_", scenario_name, "_h2_syf_network_overlay_", plot_year, ".png")
  )
}
