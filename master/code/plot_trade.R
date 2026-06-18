# Global softlink maritime trade maps ----------------------------------------

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
scenarios <- c("NPi500", "NPi500-ZF2050")

base_dir <- file.path("output", "global_260617")

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

region_boundaries <- build_region_boundaries()

# Maritime trade links --------------------------------------------------------

prm_dir <- file.path(aimpsy, scenario_group, region, date, "gams_output", "gdx_primary")
main_dir <- file.path(aimpsy, scenario_group, region, date, "main")
merged_gdx_path <- file.path(main_dir, "merged_output.gdx")
searoute_dir <- "C:/Users/Haru Sakata/OneDrive - Kyoto University/My work/AIM-PSY/data/searoute"
searoute_path <- file.path(searoute_dir, "searoute.geojson")
gwh_to_ej <- 3.6e-6
time_slice_hours <- 6
annual_flow_multiplier <- time_slice_hours
searoute_representative_path <- file.path(searoute_dir, "searoute_r31_representative.geojson")
arrow_threshold <- 10 * annual_flow_multiplier * gwh_to_ej

flow_specs <- tribble(
  ~Symbol, ~Direction,
  "vff_l", "forward",
  "vfb_l", "backward"
)

overlay_specs <- tribble(
  ~Carrier, ~Folder,           ~TitleLabel,     ~LinkPattern,                                  ~FlowLow,  ~FlowHigh,
  "NH3",    "plot_trade_nh3",  "NH3 trade",    "^R31-[A-Z0-9]+-NH3_R31-[A-Z0-9]+-NH3$",       "#f1b6da", "#c51b7d",
  "BMS",    "plot_trade_bio",  "Biomass trade", "^R31-[A-Z0-9]+-BMS_R31-[A-Z0-9]+-BMS$",      "#b2e2e2", "#006d77"
)

trade_map_specs <- tibble(
  Carrier = c("NH3", "BMS"),
  DemandVariables = list(
    "Sec_Ene_Hyd_Amm",
    c("Sec_Ene_Ele_Bio_wo_CCS", "Sec_Ene_Ele_Bio_w_CCS")
  ),
  MapLabel = c(
    "Ammonia demand",
    "Biomass power generation"
  ),
  LegendTitle = c(
    "Sec_Ene_Hyd_Amm (EJ/yr)",
    "Biomass power generation (EJ/yr)"
  ),
  FillLow = c("#fbf7ff", "#f7fcf5"),
  FillHigh = c("#54278f", "#006d2c")
)

read_trade_map_values <- function(gdx_path, map_specs) {
  if (!file.exists(gdx_path)) {
    stop("Missing GDX file: ", gdx_path)
  }

  variable_map <- map_specs %>%
    select(Carrier, DemandVariables) %>%
    unnest(DemandVariables) %>%
    rename(Variable = DemandVariables)

  rgdx.param(gdx_path, "iamc_gdx") %>%
    transmute(
      Scenario = as.character(Sc),
      Region = str_remove(as.character(Sr), "^R31-"),
      Variable = as.character(Sv),
      Year = as.numeric(as.character(Y5)),
      Value = suppressWarnings(as.numeric(as.character(iamc_gdx)))
    ) %>%
    inner_join(variable_map, by = "Variable") %>%
    filter(
      Scenario %in% scenarios,
      Region %in% region_boundaries$region,
      Year == plot_year,
      !is.na(Value)
    ) %>%
    group_by(Scenario, Region, Carrier, Year) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")
}

build_trade_map_data <- function(region_boundaries, trade_map_values, map_specs) {
  map_index <- tidyr::expand_grid(
    Scenario = scenarios,
    Carrier = map_specs$Carrier,
    region = region_boundaries$region
  ) %>%
    left_join(
      trade_map_values,
      by = c("Scenario", "Carrier", "region" = "Region")
    )

  map_index %>%
    left_join(region_boundaries %>% select(region, geometry), by = "region") %>%
    st_as_sf(crs = st_crs(region_boundaries))
}

fixed_fill_limits <- function(values) {
  vmax <- max(values, na.rm = TRUE)
  if (!is.finite(vmax) || vmax <= 0) return(NULL)
  c(0, vmax)
}

make_flow_breaks <- function(vmax) {
  if (!is.finite(vmax) || vmax <= 0) {
    return(1)
  }

  if (vmax < 1) {
    brks <- pretty(c(0, vmax), n = 4)
    return(brks[brks > 0 & brks <= vmax])
  }

  brks <- 10^seq(0, ceiling(log10(vmax)), by = 1)
  brks <- brks[brks <= vmax]
  if (length(brks) == 0) brks <- pretty(c(0, vmax), n = 4)
  brks
}

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
    return(tibble(
      L = character(),
      Value = numeric(),
      Scenario = character(),
      Symbol = character(),
      N0 = character(),
      N1 = character()
    ))
  }

  link_nodes <- str_match(
    df_link$L,
    paste0("^R31-([A-Z0-9]+)-", overlay_spec$Carrier, "_R31-([A-Z0-9]+)-", overlay_spec$Carrier, "$")
  )

  df_link <- df_link %>%
    mutate(
      N0 = link_nodes[, 2],
      N1 = link_nodes[, 3]
    )

  df_link %>%
    mutate(
      Scenario = scenario_name,
      Symbol = value_symbol,
      N0_plot = if (direction == "backward") N1 else N0,
      N1_plot = if (direction == "backward") N0 else N1,
      N0 = N0_plot,
      N1 = N1_plot
    ) %>%
    select(-N0_plot, -N1_plot)
}

make_transport_edges_sf <- function(df_link, nodes_tbl, sea_routes_sf) {
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
    filter(Value > 0)

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
    route_segment <- route_segment %>%
      arrange(coord_order)
    n_points <- nrow(route_segment)
    if (n_points < 2) return(NULL)

    reversed_segment <- route_segment %>%
      arrange(desc(coord_order)) %>%
      mutate(
        step_length = c(
          0,
          sqrt(diff(x)^2 + diff(y)^2)
        ),
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
      segment_share <- if (segment_length == 0) {
        0
      } else {
        (target_length - before_point$distance_from_end) / segment_length
      }
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
    arrange(Value)
}

add_trade_arrows <- function(p, arrow_df) {
  if (nrow(arrow_df) == 0) return(p)

  arrow_breaks <- quantile(arrow_df$Value, probs = c(0.5, 0.85), na.rm = TRUE, names = FALSE)
  arrow_df <- arrow_df %>%
    mutate(
      ArrowGroup = case_when(
        Value >= arrow_breaks[[2]] ~ "large",
        Value >= arrow_breaks[[1]] ~ "medium",
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
        aes(x = x, y = y, xend = xend, yend = yend, linewidth = Value_plot, color = Value_plot),
        alpha = 0.86,
        lineend = "round",
        arrow = arrow(length = unit(arrow_styles$ArrowLength[i], "mm"), type = "closed")
      )
  }

  p
}

plot_trade_flow <- function(edges_sf, scenario_name, overlay_spec, map_data = NULL, map_spec = NULL) {
  if (nrow(edges_sf) == 0) {
    warning("No plottable trade links for scenario: ", scenario_name, " / ", overlay_spec$Carrier)
    return(NULL)
  }

  vmax <- max(edges_sf$Value, na.rm = TRUE)
  brks <- make_flow_breaks(vmax)

  edges_plot_sf <- edges_sf %>%
    mutate(Value_plot = log10(1 + Value))
  edges_plot_df <- edges_plot_sf %>%
    st_drop_geometry()
  edges_arrow_sf <- edges_plot_sf %>% filter(Value > arrow_threshold)
  route_nodes_df <- bind_rows(
    edges_plot_df %>% transmute(N_PSY = N0, lon = x0, lat = y0),
    edges_plot_df %>% transmute(N_PSY = N1, lon = x1, lat = y1)
  ) %>%
    distinct(N_PSY, lon, lat)
  sea_route_line_df <- make_sea_route_df(edges_plot_sf)
  sea_route_arrow_df <- make_route_arrow_df(edges_arrow_sf)

  p <- ggplot()

  if (!is.null(map_data) && !is.null(map_spec)) {
    plot_map_data <- map_data %>%
      filter(Scenario == scenario_name, Carrier == overlay_spec$Carrier)
    fill_limits <- map_data %>%
      filter(Carrier == overlay_spec$Carrier) %>%
      pull(Value) %>%
      fixed_fill_limits()

    p <- p +
      geom_sf(data = plot_map_data, aes(fill = Value), color = "grey35", linewidth = 0.3) +
      scale_fill_gradient(
        low = map_spec$FillLow,
        high = map_spec$FillHigh,
        limits = fill_limits,
        na.value = "grey92",
        labels = scales::label_number(accuracy = 0.01),
        name = map_spec$LegendTitle
      )
  } else {
    p <- p +
      geom_sf(data = region_boundaries, fill = NA, color = "grey45", linewidth = 0.35)
  }

  p <- p +
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
      aes(x = x, y = y, group = segment_id, linewidth = Value_plot, color = Value_plot),
      alpha = 0.86,
      lineend = "round"
    )

  p <- add_trade_arrows(p, sea_route_arrow_df)

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
      labels = scales::label_number(accuracy = 0.00001)(brks),
      name = "vff_l / vfb_l annual flow (EJ/yr)"
    ) +
    scale_color_gradient(
      low = overlay_spec$FlowLow,
      high = overlay_spec$FlowHigh,
      breaks = log10(1 + brks),
      labels = scales::label_number(accuracy = 0.00001)(brks),
      name = "vff_l / vfb_l annual flow (EJ/yr)"
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
        barwidth = unit(48, "mm"),
        barheight = unit(3, "mm")
      ),
      linewidth = "none"
    ) +
    labs(
      title = paste0(
        scenario_name,
        " ",
        overlay_spec$TitleLabel,
        if (!is.null(map_spec)) {
          paste0(" and ", map_spec$MapLabel)
        } else {
          ""
        },
        " combined sea trade flow (",
        plot_year,
        ")"
      )
    )
}

trade_port_nodes_tbl <- load_trade_nodes(searoute_representative_path)
sea_routes_sf <- load_sea_routes(searoute_path)
has_trade_map_data <- file.exists(merged_gdx_path)
if (has_trade_map_data) {
  trade_map_values <- read_trade_map_values(merged_gdx_path, trade_map_specs)
  if (nrow(trade_map_values) == 0) {
    warning("No demand map values were read from iamc_gdx: ", merged_gdx_path)
  }
  trade_map_data <- build_trade_map_data(region_boundaries, trade_map_values, trade_map_specs)
} else {
  warning("Skipping demand-trade overlays because merged_output.gdx is missing: ", merged_gdx_path)
}

for (i in seq_len(nrow(overlay_specs))) {
  overlay_spec <- overlay_specs[i, ]
  map_spec <- trade_map_specs %>%
    filter(Carrier == overlay_spec$Carrier)

  for (scenario_name in scenarios) {
    overlay_dir <- file.path(base_dir, scenario_name, overlay_spec$Folder, "vff_l_vfb_l_combined")
    dir.create(overlay_dir, recursive = TRUE, showWarnings = FALSE)

    edges_combined_sf <- map_dfr(seq_len(nrow(flow_specs)), function(j) {
      read_transport_value(
        scenario_name = scenario_name,
        overlay_spec = overlay_spec,
        flow_spec = flow_specs[j, ]
      )
    }) %>%
      make_transport_edges_sf(trade_port_nodes_tbl, sea_routes_sf)

    p <- plot_trade_flow(
      edges_sf = edges_combined_sf,
      scenario_name = scenario_name,
      overlay_spec = overlay_spec
    )
    if (is.null(p)) next

    ggsave(
      filename = file.path(
        overlay_dir,
        paste0(
          "global_",
          scenario_name,
          "_vff_l_vfb_l_combined_",
          overlay_spec$Carrier,
          "_trade_",
          plot_year,
          ".png"
        )
      ),
      plot = p,
      width = 300,
      height = 150,
      units = "mm",
      dpi = 300,
      bg = "white"
    )

    if (has_trade_map_data) {
      overlay_map_dir <- file.path(base_dir, scenario_name, overlay_spec$Folder, "demand_trade_overlay")
      dir.create(overlay_map_dir, recursive = TRUE, showWarnings = FALSE)

      p_overlay <- plot_trade_flow(
        edges_sf = edges_combined_sf,
        scenario_name = scenario_name,
        overlay_spec = overlay_spec,
        map_data = trade_map_data,
        map_spec = map_spec
      )
      if (is.null(p_overlay)) next

      ggsave(
        filename = file.path(
          overlay_map_dir,
          paste0(
            "global_",
            scenario_name,
            "_vff_l_vfb_l_combined_",
            overlay_spec$Carrier,
            "_demand_trade_overlay_",
            plot_year,
            ".png"
          )
        ),
        plot = p_overlay,
        width = 300,
        height = 150,
        units = "mm",
        dpi = 300,
        bg = "white"
      )
    }
  }
}
