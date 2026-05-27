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
scenario_group <- "global_softlink"
region <- "global"
date <- "2605251345"
plot_year <- 2050
scenarios <- c("300C", "300C-LimCCS", "300C-LimCCS_bio")

base_dir <- file.path("output", "global_260525")
data_dir <- "data"
nh3_node_csv <- file.path(data_dir, "AIM_R31_NH3_trade_nodes.csv")

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
data_dir <- "data"
nh3_node_csv <- file.path(data_dir, "AIM_R31_NH3_trade_nodes.csv")
trade_port_csv <- file.path(data_dir, "AIM_R31_representative_trade_ports.csv")
nv_dist_csv <- Sys.getenv(
  "AIMPSY_NV_DIST_CSV",
  unset = "//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/AIMPSY-master/data/PowerSystemData/global/nv_dist.csv"
)
arrow_threshold <- 10

flow_specs <- tribble(
  ~Symbol, ~Direction,
  "vff_l", "forward",
  "vfb_l", "backward"
)

overlay_specs <- tribble(
  ~Carrier, ~Folder,           ~TitleLabel,    ~LinkPattern,
  "NH3",    "plot_trade_nh3",  "NH3 trade",   "^R31-[A-Z0-9]+-NH3_R31-[A-Z0-9]+-NH3$",
  "BMS",    "plot_trade_bio",  "Biomass trade","^R31-[A-Z0-9]+-BMS_R31-[A-Z0-9]+-BMS$"
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

load_navigation_distances <- function(dist_csv) {
  if (!file.exists(dist_csv)) {
    warning("Navigation distance CSV not found: ", dist_csv)
    return(tibble(N0 = character(), N1 = character(), NavigationDistanceKm = numeric()))
  }

  dist_raw <- read.csv(dist_csv, check.names = FALSE, stringsAsFactors = FALSE)
  names(dist_raw)[[1]] <- "N0_raw"

  dist_raw %>%
    pivot_longer(
      cols = -N0_raw,
      names_to = "N1_raw",
      values_to = "NavigationDistanceKm"
    ) %>%
    transmute(
      N0 = str_remove(as.character(N0_raw), "^R31-"),
      N1 = str_remove(as.character(N1_raw), "^R31-"),
      NavigationDistanceKm = as.numeric(NavigationDistanceKm)
    )
}

make_transport_edges_sf <- function(df_link, nodes_tbl, navigation_distances = NULL) {
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

  if (!is.null(navigation_distances) && nrow(navigation_distances) > 0) {
    edges <- edges %>%
      left_join(navigation_distances, by = c("N0", "N1"))
  }

  if (nrow(edges) == 0) {
    return(st_sf(edges, geometry = st_sfc(crs = 4326)))
  }

  lines_sfc <- pmap(
    edges[, c("x0", "y0", "x1", "y1")],
    ~ st_linestring(matrix(c(..1, ..2, ..3, ..4), ncol = 2, byrow = TRUE))
  )

  st_sf(edges, geometry = st_sfc(lines_sfc, crs = 4326))
}

resolve_trade_port_csv <- function(port_csv) {
  candidates <- unique(c(
    port_csv,
    file.path("master", "data", basename(port_csv)),
    file.path("C:/AIMPSY_plot/master/data", basename(port_csv))
  ))

  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) return(NA_character_)
  existing[[1]]
}

load_trade_nodes <- function(default_node_csv, port_csv) {
  port_path <- resolve_trade_port_csv(port_csv)

  if (!is.na(port_path)) {
    return(
      read.csv(port_path, stringsAsFactors = FALSE) %>%
        transmute(
          N_PSY,
          lon = as.numeric(lon),
          lat = as.numeric(lat)
        )
    )
  }

  warning(
    "Representative trade port CSV not found: ",
    port_csv,
    ". Falling back to regional centroid nodes."
  )

  read.csv(default_node_csv, stringsAsFactors = FALSE) %>%
    transmute(N_PSY, lon = as.numeric(lon), lat = as.numeric(lat))
}

route_region_class <- function(region_code) {
  case_when(
    region_code %in% c("USA", "CAN", "MEX") ~ "north_america",
    region_code %in% c("BRA", "XLM") ~ "latin_america",
    region_code %in% c("JPN", "KOR", "CHN", "XEA", "VNM", "THA", "MYS", "XSE", "IDN") ~ "east_asia",
    region_code %in% c("IND", "XSA") ~ "south_asia",
    region_code %in% c("AUS", "XOC") ~ "oceania",
    region_code %in% c("XME") ~ "middle_east",
    region_code %in% c("XAF", "ZAF", "XENI") ~ "africa",
    region_code %in% c("XE15", "XE10", "XE3", "XEWI", "XEEI", "TUR", "XNF") ~ "europe",
    region_code %in% c("RUS", "XCS") ~ "eurasia",
    TRUE ~ "other"
  )
}

route_hub <- function(name) {
  switch(
    name,
    singapore = tibble(lon = 103.8, lat = 1.3),
    suez = tibble(lon = 32.6, lat = 30.0),
    gibraltar = tibble(lon = -5.4, lat = 36.0),
    panama = tibble(lon = -79.9, lat = 9.0),
    cape = tibble(lon = 18.4, lat = -34.3),
    hormuz = tibble(lon = 56.3, lat = 26.6),
    dateline_north_w = tibble(lon = 179.7, lat = 42.0),
    dateline_north_e = tibble(lon = -179.7, lat = 42.0),
    dateline_south_w = tibble(lon = 179.7, lat = -22.0),
    dateline_south_e = tibble(lon = -179.7, lat = -22.0),
    stop("Unknown route hub: ", name)
  )
}

uses_pacific_route <- function(class0, class1, n0, n1) {
  pacific_americas <- c("north_america", "latin_america")
  pacific_asia <- c("east_asia", "oceania")

  any(c(class0, class1) %in% pacific_americas) &&
    any(c(class0, class1) %in% pacific_asia) &&
    !"BRA" %in% c(n0, n1)
}

build_sea_route_points <- function(edge_row) {
  n0 <- edge_row$N0
  n1 <- edge_row$N1
  class0 <- route_region_class(n0)
  class1 <- route_region_class(n1)

  start <- tibble(lon = edge_row$x0, lat = edge_row$y0)
  end <- tibble(lon = edge_row$x1, lat = edge_row$y1)
  hubs <- list()

  if (uses_pacific_route(class0, class1, n0, n1)) {
    north_route <- edge_row$y0 > 0 && edge_row$y1 > 0
    if (edge_row$x0 > 0 || (edge_row$x0 > edge_row$x1 && abs(edge_row$x0 - edge_row$x1) > 120)) {
      hubs <- if (north_route) {
        list(route_hub("dateline_north_w"), route_hub("dateline_north_e"))
      } else {
        list(route_hub("dateline_south_w"), route_hub("dateline_south_e"))
      }
    } else {
      hubs <- if (north_route) {
        list(route_hub("dateline_north_e"), route_hub("dateline_north_w"))
      } else {
        list(route_hub("dateline_south_e"), route_hub("dateline_south_w"))
      }
    }
  } else if (any(c(class0, class1) %in% c("east_asia", "oceania")) &&
             any(c(class0, class1) %in% c("europe", "middle_east", "africa", "south_asia", "eurasia"))) {
    hubs <- list(route_hub("singapore"))

    if (any(c(class0, class1) %in% c("europe", "eurasia"))) {
      hubs <- c(hubs, list(route_hub("suez"), route_hub("gibraltar")))
    } else if (any(c(class0, class1) %in% "middle_east")) {
      hubs <- c(hubs, list(route_hub("hormuz")))
    } else if (any(c(n0, n1) %in% "ZAF")) {
      hubs <- list(route_hub("cape"))
    }

    if (edge_row$x0 < edge_row$x1) {
      hubs <- rev(hubs)
    }
  } else if (any(c(class0, class1) %in% c("latin_america", "north_america")) &&
             any(c(class0, class1) %in% c("europe", "africa", "middle_east", "south_asia"))) {
    hubs <- list(route_hub("gibraltar"))

    if (any(c(class0, class1) %in% c("middle_east", "south_asia"))) {
      hubs <- c(hubs, list(route_hub("suez")))
    }
    if (any(c(n0, n1) %in% c("XLM", "MEX")) &&
        any(c(class0, class1) %in% c("middle_east", "south_asia"))) {
      hubs <- c(list(route_hub("panama")), hubs)
    }
    if (edge_row$x0 > edge_row$x1) {
      hubs <- rev(hubs)
    }
  } else if (any(c(n0, n1) %in% "BRA") &&
             any(c(class0, class1) %in% c("east_asia", "oceania"))) {
    hubs <- list(route_hub("cape"), route_hub("singapore"))
    if (edge_row$x0 > edge_row$x1) {
      hubs <- rev(hubs)
    }
  }

  bind_rows(c(list(start), hubs, list(end)))
}

split_route_at_dateline <- function(route_points, route_id) {
  segments <- vector("list", max(1, nrow(route_points) - 1))
  out <- list()
  segment_id <- 1

  for (i in seq_len(nrow(route_points) - 1)) {
    p0 <- route_points[i, ]
    p1 <- route_points[i + 1, ]

    if (abs(p1$lon - p0$lon) > 180) {
      edge_lon0 <- if (p0$lon > 0) 180 else -180
      edge_lon1 <- -edge_lon0
      t <- (edge_lon0 - p0$lon) / ((p1$lon + sign(p0$lon) * 360) - p0$lon)
      edge_lat <- p0$lat + t * (p1$lat - p0$lat)

      out[[length(out) + 1]] <- tibble(
        route_id = route_id,
        segment_id = paste0(route_id, "_", segment_id),
        x = c(p0$lon, edge_lon0),
        y = c(p0$lat, edge_lat)
      )
      segment_id <- segment_id + 1
      out[[length(out) + 1]] <- tibble(
        route_id = route_id,
        segment_id = paste0(route_id, "_", segment_id),
        x = c(edge_lon1, p1$lon),
        y = c(edge_lat, p1$lat)
      )
    } else {
      out[[length(out) + 1]] <- tibble(
        route_id = route_id,
        segment_id = paste0(route_id, "_", segment_id),
        x = c(p0$lon, p1$lon),
        y = c(p0$lat, p1$lat)
      )
    }
    segment_id <- segment_id + 1
  }

  bind_rows(out)
}

make_sea_route_df <- function(edges_df) {
  if (nrow(edges_df) == 0) return(tibble())

  map_dfr(seq_len(nrow(edges_df)), function(i) {
    edge_row <- edges_df[i, ]
    route_id <- paste(edge_row$Scenario, edge_row$Symbol, edge_row$N0, edge_row$N1, i, sep = "__")
    route_points <- build_sea_route_points(edge_row)

    split_route_at_dateline(route_points, route_id) %>%
      mutate(
        Value = edge_row$Value,
        Value_plot = edge_row$Value_plot,
        N0 = edge_row$N0,
        N1 = edge_row$N1,
        Scenario = edge_row$Scenario,
        Symbol = edge_row$Symbol
      )
  })
}

make_route_arrow_df <- function(edges_df) {
  if (nrow(edges_df) == 0) return(tibble())

  map_dfr(seq_len(nrow(edges_df)), function(i) {
    edge_row <- edges_df[i, ]
    route_points <- build_sea_route_points(edge_row)
    if (nrow(route_points) < 2) return(tibble())

    p0 <- route_points[nrow(route_points) - 1, ]
    p1 <- route_points[nrow(route_points), ]
    if (abs(p1$lon - p0$lon) > 180) {
      p0$lon <- if (p1$lon < 0) -179.7 else 179.7
    }

    tibble(
      x = p0$lon,
      y = p0$lat,
      xend = p1$lon,
      yend = p1$lat,
      Value = edge_row$Value,
      Value_plot = edge_row$Value_plot
    )
  })
}

plot_trade_flow <- function(edges_sf, scenario_name, overlay_spec) {
  if (nrow(edges_sf) == 0) {
    warning("No plottable trade links for scenario: ", scenario_name, " / ", overlay_spec$Carrier)
    return(NULL)
  }

  vmax <- max(edges_sf$Value, na.rm = TRUE)
  brks <- 10^seq(0, ceiling(log10(vmax)), by = 1)
  brks <- brks[brks <= vmax]
  if (length(brks) == 0) brks <- pretty(c(0, vmax), n = 4)

  edges_plot_df <- edges_sf %>%
    mutate(Value_plot = log10(1 + Value)) %>%
    st_drop_geometry()
  edges_line_df <- edges_plot_df %>% filter(Value <= arrow_threshold)
  edges_arrow_df <- edges_plot_df %>% filter(Value > arrow_threshold)
  route_nodes_df <- bind_rows(
    edges_plot_df %>% transmute(N_PSY = N0, lon = x0, lat = y0),
    edges_plot_df %>% transmute(N_PSY = N1, lon = x1, lat = y1)
  ) %>%
    distinct(N_PSY, lon, lat)
  sea_route_line_df <- make_sea_route_df(edges_plot_df)
  sea_route_arrow_df <- make_route_arrow_df(edges_arrow_df)

  p <- ggplot()
  p <- p +
    geom_sf(data = region_boundaries, fill = NA, color = "grey45", linewidth = 0.35) +
    geom_path(
      data = sea_route_line_df,
      aes(x = x, y = y, group = segment_id),
      color = "white",
      linewidth = 0.9,
      alpha = 0.75,
      lineend = "round",
      show.legend = FALSE
    ) +
    geom_path(
      data = sea_route_line_df,
      aes(x = x, y = y, group = segment_id, linewidth = Value_plot, color = Value_plot),
      alpha = 0.82,
      lineend = "round"
    ) +
    geom_segment(
      data = sea_route_arrow_df,
      aes(x = x, y = y, xend = xend, yend = yend),
      color = "white",
      linewidth = 0.9,
      alpha = 0.75,
      lineend = "round",
      arrow = arrow(length = unit(1.55, "mm"), type = "closed"),
      show.legend = FALSE
    ) +
    geom_segment(
      data = sea_route_arrow_df,
      aes(x = x, y = y, xend = xend, yend = yend, linewidth = Value_plot, color = Value_plot),
      alpha = 0.82,
      lineend = "round",
      arrow = arrow(length = unit(1.45, "mm"), type = "closed")
    ) +
    geom_point(
      data = route_nodes_df,
      aes(x = lon, y = lat),
      shape = 21,
      size = 1.5,
      stroke = 0.25,
      color = "grey20",
      fill = "white",
      alpha = 0.9,
      inherit.aes = FALSE
    )

  p +
    scale_linewidth_continuous(
      range = c(0.25, 1.45),
      breaks = log10(1 + brks),
      labels = scales::label_number(accuracy = 1)(brks),
      name = "vff_l / vfb_l annual flow (GWh/yr)"
    ) +
    scale_color_gradient(
      low = "#ffd34d",
      high = "#b2182b",
      breaks = log10(1 + brks),
      labels = scales::label_number(accuracy = 1)(brks),
      name = "vff_l / vfb_l annual flow (GWh/yr)"
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
        " combined sea trade flow (",
        plot_year,
        ")"
      )
    )
}

if (!file.exists(nh3_node_csv)) {
  stop(
    "Missing NH3 node coordinate CSV: ",
    nh3_node_csv,
    ". Run code/worldmap_plot.R first."
  )
}

trade_port_nodes_tbl <- load_trade_nodes(nh3_node_csv, trade_port_csv)
navigation_distances <- load_navigation_distances(nv_dist_csv)

for (i in seq_len(nrow(overlay_specs))) {
  overlay_spec <- overlay_specs[i, ]

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
      make_transport_edges_sf(trade_port_nodes_tbl, navigation_distances)

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
  }
}
