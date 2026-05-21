# Global softlink transmission flow plots ------------------------------------

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(sf)
library(ggplot2)
library(gdxrrw)
library(rnaturalearth)
library(rnaturalearthdata)

sf_use_s2(FALSE)

#for (gams_sys_dir in c("C:/GAMS/38", "C:/GAMS/win64/26.1")) {
#  if (dir.exists(gams_sys_dir)) {
#    igdx(gams_sys_dir)
#    break
#  }
#}

aimpsy <- "//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output"
scenario_group <- "global_softlink"
region <- "global"
date <- "2605141552"
scenarios <- c("300C")
plot_year <- 2050

ddir <- file.path(aimpsy, scenario_group, region, date)
prm_dir <- file.path(ddir, "gams_output", "gdx_primary")

softlink_dir <- if (basename(normalizePath(getwd(), winslash = "/", mustWork = FALSE)) == "softlink") {
  "."
} else {
  "softlink"
}
data_dir <- file.path(softlink_dir, "data")
nh3_node_csv <- file.path(data_dir, "AIM_R31_NH3_trade_nodes.csv")
base_dir <- file.path(softlink_dir, "output", "global_softlink", "global_compare")
link_types <- tribble(
  ~LinkType, ~Pattern,          ~NodeSuffix, ~Folder,       ~FileTag, ~TitleLabel,      ~Palette,
  "AC",      ".*-AC_.*-AC$",    "-AC",       "plot_link",   "AC",     "AC",            "plasma",
  "H2",      ".*-H2_.*-H2$",    "-H2",       "plot_link_h2","H2",     "H2 pipeline",   "viridis"
)

value_specs <- tribble(
  ~Symbol,  ~Aggregation, ~Direction,  ~FileTag, ~LegendTitle,                         ~TitleMetric,
  "vff_l",  "sum_time",   "forward",   "vff_l",  "vff_l annual forward flow (GWh/yr)", "annual forward flow from vff_l (GWh/yr)",
  "vfb_l",  "sum_time",   "backward",  "vfb_l",  "vfb_l annual backward flow (GWh/yr)","annual backward flow from vfb_l (GWh/yr)",
  "vfs_l",  "as_is",      "none",      "vfs_l",  "vfs_l link capacity (GW)",           "link capacity from vfs_l (GW)"
)

arrow_threshold <- 10

build_region_boundaries <- function() {
  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) %>%
    st_make_valid() %>%
    mutate(iso3 = if_else(iso_a3 == "-99", adm0_a3, iso_a3))

  region_map <- tribble(
    ~iso3, ~region,

    # Individual Asian regions
    "JPN", "JPN",
    "CHN", "CHN",
    "IND", "IND",
    "IDN", "IDN",
    "KOR", "KOR",
    "THA", "THA",
    "MYS", "MYS",
    "VNM", "VNM",

    # Southeast Asia
    "BRN", "XSE", "KHM", "XSE", "LAO", "XSE",
    "MMR", "XSE", "PHL", "XSE", "SGP", "XSE",
    "TLS", "XSE",

    # South Asia
    "AFG", "XSA", "BGD", "XSA", "BTN", "XSA",
    "LKA", "XSA", "MDV", "XSA", "NPL", "XSA",
    "PAK", "XSA",

    # East Asia
    "MNG", "XEA", "PRK", "XEA", "TWN", "XEA",

    # Central Asia
    "KAZ", "XCS", "KGZ", "XCS", "TJK", "XCS",
    "TKM", "XCS", "UZB", "XCS",

    # Middle East
    "ARE", "XME", "BHR", "XME", "IRN", "XME",
    "IRQ", "XME", "ISR", "XME", "JOR", "XME",
    "KWT", "XME", "LBN", "XME", "OMN", "XME",
    "QAT", "XME", "SAU", "XME", "SYR", "XME",
    "YEM", "XME",

    # Oceania
    "AUS", "AUS",
    "NZL", "XOC", "PNG", "XOC", "FJI", "XOC",
    "SLB", "XOC", "VUT", "XOC", "WSM", "XOC",

    # North America
    "CAN", "CAN",
    "USA", "USA",
    "MEX", "MEX",

    # Latin America
    "BRA", "BRA",
    "ARG", "XLM", "BOL", "XLM", "CHL", "XLM",
    "COL", "XLM", "CRI", "XLM", "CUB", "XLM",
    "DOM", "XLM", "ECU", "XLM", "GTM", "XLM",
    "HND", "XLM", "HTI", "XLM", "JAM", "XLM",
    "NIC", "XLM", "PAN", "XLM", "PER", "XLM",
    "PRY", "XLM", "SLV", "XLM", "URY", "XLM",
    "VEN", "XLM", "GUY", "XLM", "SUR", "XLM",
    "TTO", "XLM", "BHS", "XLM", "BRB", "XLM",
    "BLZ", "XLM",

    # Europe
    "TUR", "TUR",
    "RUS", "RUS",

    # EU15
    "AUT", "XE15", "BEL", "XE15", "DNK", "XE15",
    "FIN", "XE15", "FRA", "XE15", "DEU", "XE15",
    "GRC", "XE15", "IRL", "XE15", "ITA", "XE15",
    "LUX", "XE15", "NLD", "XE15", "PRT", "XE15",
    "ESP", "XE15", "SWE", "XE15", "GBR", "XE15",

    # EU10 / Central and Eastern EU
    "BGR", "XE3", "CYP", "XE10", "CZE", "XE10",
    "EST", "XE10", "HUN", "XE10", "LVA", "XE10",
    "LTU", "XE10", "MLT", "XE10", "POL", "XE10",
    "ROU", "XE3", "SVK", "XE10", "SVN", "XE10",

    # Western Europe outside EU15
    "CHE", "XEWI", "NOR", "XEWI", "ISL", "XEWI",

    # Eastern Europe / Balkans / Caucasus
    "ALB", "XEEI", "BIH", "XEEI", "HRV", "XEEI",
    "MNE", "XEEI", "MKD", "XEEI", "SRB", "XEEI",
    "UKR", "XEEI", "BLR", "XEEI", "MDA", "XEEI",
    "ARM", "XEEI", "AZE", "XEEI", "GEO", "XEEI",

    # North Africa
    "DZA", "XENI", "EGY", "XENI", "LBY", "XENI",
    "MAR", "XENI", "TUN", "XENI", "ESH", "XENI",

    # South Africa
    "ZAF", "ZAF",

    # Africa
    "AGO", "XAF", "BEN", "XAF", "BWA", "XAF",
    "BFA", "XAF", "BDI", "XAF", "CMR", "XAF",
    "CAF", "XAF", "TCD", "XAF", "COD", "XAF",
    "COG", "XAF", "CIV", "XAF", "DJI", "XAF",
    "ERI", "XAF", "ETH", "XAF", "GAB", "XAF",
    "GHA", "XAF", "GIN", "XAF", "GNB", "XAF",
    "GNQ", "XAF", "KEN", "XAF", "LBR", "XAF",
    "LSO", "XAF", "MDG", "XAF", "MWI", "XAF",
    "MLI", "XAF", "MRT", "XAF", "MOZ", "XAF",
    "NAM", "XAF", "NER", "XAF", "NGA", "XAF",
    "RWA", "XAF", "SEN", "XAF", "SLE", "XAF",
    "SOM", "XAF", "SSD", "XAF", "SDN", "XAF",
    "SWZ", "XAF", "TZA", "XAF", "TGO", "XAF",
    "UGA", "XAF", "ZMB", "XAF", "ZWE", "XAF",

    # No fuel / unclassified
    "GRL", "XNF"
  )

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

node_path <- "C:/Users/Haru Sakata/OneDrive - Kyoto University/My work/AIM-PSY/nodes_centroid.geojson"
if (!file.exists(node_path)) {
  stop("Missing node coordinate file: ", node_path)
}

nodes_tbl <- st_read(node_path, quiet = TRUE) %>%
  st_drop_geometry() %>%
  transmute(
    N_PSY,
    lon = as.numeric(centroid_x),
    lat = as.numeric(centroid_y)
  )

read_link_value <- function(scenario_name, link_pattern, node_suffix, value_spec) {
  gdx_path <- file.path(prm_dir, scenario_name, paste0(plot_year, ".gdx"))
  if (!file.exists(gdx_path)) {
    stop("Missing GDX file: ", gdx_path)
  }

  value_symbol <- value_spec$Symbol
  aggregation <- value_spec$Aggregation
  direction <- value_spec$Direction

  df_raw <- rgdx.param(gdx_path, value_symbol) %>%
    mutate(across(where(is.factor), as.character)) %>%
    filter(str_detect(L, link_pattern))

  df_link <- if (aggregation == "sum_time") {
    df_raw %>%
      group_by(L) %>%
      summarise(Value = sum(.data[[value_symbol]], na.rm = TRUE), .groups = "drop")
  } else if (aggregation == "as_is") {
    df_raw %>%
      transmute(L, Value = .data[[value_symbol]])
  } else {
    stop("Unknown aggregation: ", aggregation)
  }

  df_link %>%
    separate(L, sep = "_", into = c("N0", "N1"), remove = FALSE) %>%
    mutate(
      Scenario = scenario_name,
      Symbol = value_symbol,
      N0 = str_remove_all(N0, fixed(node_suffix)),
      N1 = str_remove_all(N1, fixed(node_suffix))
    ) %>%
    mutate(
      N0_plot = if (direction == "backward") N1 else N0,
      N1_plot = if (direction == "backward") N0 else N1,
      N0 = N0_plot,
      N1 = N1_plot
    ) %>%
    select(-N0_plot, -N1_plot)
}

make_edges_sf <- function(df_link) {
  edges <- df_link %>%
    left_join(nodes_tbl, by = c("N0" = "N_PSY")) %>%
    rename(x0 = lon, y0 = lat) %>%
    left_join(nodes_tbl, by = c("N1" = "N_PSY")) %>%
    rename(x1 = lon, y1 = lat) %>%
    filter(!is.na(x0), !is.na(y0), !is.na(x1), !is.na(y1)) %>%
    filter(Value > 0)

  if (nrow(edges) == 0) {
    return(st_sf(edges, geometry = st_sfc(crs = 4326)))
  }

  lines_sfc <- pmap(
    edges[, c("x0", "y0", "x1", "y1")],
    ~ st_linestring(matrix(c(..1, ..2, ..3, ..4), ncol = 2, byrow = TRUE))
  )

  st_sf(edges, geometry = st_sfc(lines_sfc, crs = 4326))
}

plot_transmission <- function(edges_sf, scenario_name, title_label, palette, value_spec) {
  if (nrow(edges_sf) == 0) {
    warning(
      "No plottable ",
      title_label,
      " links for scenario: ",
      scenario_name,
      " / ",
      value_spec$Symbol
    )
    return(NULL)
  }

  vmax <- max(edges_sf$Value, na.rm = TRUE)
  brks <- 10^seq(0, ceiling(log10(vmax)), by = 1)
  brks <- brks[brks <= vmax]
  if (length(brks) == 0) brks <- pretty(c(0, vmax), n = 4)

  edges_plot <- edges_sf %>%
    mutate(Value_plot = log10(1 + Value))

  edges_plot_df <- edges_plot %>% st_drop_geometry()
  edges_line_df <- edges_plot_df %>% filter(Value <= arrow_threshold)
  edges_arrow_df <- edges_plot_df %>% filter(Value > arrow_threshold)
  linewidth_range <- if (value_spec$Direction %in% c("forward", "backward")) {
    c(0.08, 1.05)
  } else {
    c(0.10, 2.2)
  }

  p <- ggplot() +
    geom_sf(data = region_boundaries, fill = NA, color = "grey45", linewidth = 0.35) +
    scale_linewidth_continuous(
      range = linewidth_range,
      breaks = log10(1 + brks),
      labels = scales::label_number(accuracy = 1)(brks),
      name = value_spec$LegendTitle
    ) +
    scale_color_viridis_c(
      option = palette,
      direction = -1,
      breaks = log10(1 + brks),
      labels = scales::label_number(accuracy = 1)(brks),
      name = value_spec$LegendTitle
    ) +
    coord_sf(ylim = c(-60, 75), expand = FALSE) +
    theme_void() +
    theme(
      legend.position = c(0.08, 0.32),
      legend.direction = "vertical",
      legend.background = element_rect(fill = "white", color = "grey85"),
      legend.key.width = unit(8, "mm"),
      legend.key.height = unit(4, "mm"),
      plot.title = element_text(hjust = 0.5)
    ) +
    labs(
      title = paste0(
        scenario_name,
        " inter-regional ",
        title_label,
        " ",
        value_spec$TitleMetric,
        " (",
        plot_year,
        ")"
      )
    )

  if (value_spec$Direction %in% c("forward", "backward")) {
    p +
      geom_segment(
        data = edges_line_df,
        aes(x = x0, y = y0, xend = x1, yend = y1, linewidth = Value_plot, color = Value_plot),
        alpha = 0.85,
        lineend = "round"
      ) +
      geom_segment(
        data = edges_arrow_df,
        aes(x = x0, y = y0, xend = x1, yend = y1, linewidth = Value_plot, color = Value_plot),
        alpha = 0.85,
        lineend = "round",
        arrow = arrow(length = unit(1.45, "mm"), type = "closed")
      )
  } else {
    p +
      geom_sf(
        data = edges_plot,
        aes(linewidth = Value_plot, color = Value_plot),
        alpha = 0.85
      )
  }
}

plot_combined_flow <- function(edges_sf, scenario_name, title_label, palette) {
  if (nrow(edges_sf) == 0) {
    warning("No plottable combined flow links for scenario: ", scenario_name)
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

  ggplot() +
    geom_sf(data = region_boundaries, fill = NA, color = "grey45", linewidth = 0.35) +
    geom_segment(
      data = edges_line_df,
      aes(x = x0, y = y0, xend = x1, yend = y1, linewidth = Value_plot, color = Value_plot),
      alpha = 0.78,
      lineend = "round"
    ) +
    geom_segment(
      data = edges_arrow_df,
      aes(x = x0, y = y0, xend = x1, yend = y1, linewidth = Value_plot, color = Value_plot),
      alpha = 0.78,
      lineend = "round",
      arrow = arrow(length = unit(1.45, "mm"), type = "closed")
    ) +
    scale_linewidth_continuous(
      range = c(0.08, 1.05),
      breaks = log10(1 + brks),
      labels = scales::label_number(accuracy = 1)(brks),
      name = "vff_l / vfb_l annual flow (GWh/yr)"
    ) +
    scale_color_viridis_c(
      option = palette,
      direction = -1,
      breaks = log10(1 + brks),
      labels = scales::label_number(accuracy = 1)(brks),
      name = "vff_l / vfb_l annual flow (GWh/yr)"
    ) +
    coord_sf(ylim = c(-60, 75), expand = FALSE) +
    theme_void() +
    theme(
      legend.position = c(0.13, 0.32),
      legend.direction = "vertical",
      legend.background = element_rect(fill = "white", color = "grey85"),
      legend.key.width = unit(8, "mm"),
      legend.key.height = unit(4, "mm"),
      plot.title = element_text(hjust = 0.5)
    ) +
    labs(
      title = paste0(
        scenario_name,
        " inter-regional ",
        title_label,
        " combined annual flow from vff_l and vfb_l (GWh/yr, ",
        plot_year,
        ")"
      )
    )
}

walk(seq_len(nrow(link_types)), function(i) {
  spec <- link_types[i, ]

  walk(seq_len(nrow(value_specs)), function(j) {
    value_spec <- value_specs[j, ]

    for (scenario_name in scenarios) {
      plot_dir <- file.path(base_dir, scenario_name, spec$Folder, value_spec$Symbol)
      dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

      edges_sf <- read_link_value(
        scenario_name = scenario_name,
        link_pattern = spec$Pattern,
        node_suffix = spec$NodeSuffix,
        value_spec = value_spec
      ) %>%
        make_edges_sf()

      p <- plot_transmission(
        edges_sf = edges_sf,
        scenario_name = scenario_name,
        title_label = spec$TitleLabel,
        palette = spec$Palette,
        value_spec = value_spec
      )
      if (is.null(p)) next

      ggsave(
        filename = file.path(
          plot_dir,
          paste0(
            "global_",
            scenario_name,
            "_",
            value_spec$FileTag,
            "_",
            spec$FileTag,
            "_",
            plot_year,
            ".png"
          )
        ),
        plot = p,
        width = 300,
        height = 130,
        units = "mm",
        dpi = 300,
        bg = "white"
      )
    }
  })

  flow_specs <- value_specs %>%
    filter(Symbol %in% c("vff_l", "vfb_l"))

  for (scenario_name in scenarios) {
    combined_dir <- file.path(base_dir, scenario_name, spec$Folder, "vff_l_vfb_l_combined")
    dir.create(combined_dir, recursive = TRUE, showWarnings = FALSE)

    edges_combined_sf <- map_dfr(seq_len(nrow(flow_specs)), function(j) {
      read_link_value(
        scenario_name = scenario_name,
        link_pattern = spec$Pattern,
        node_suffix = spec$NodeSuffix,
        value_spec = flow_specs[j, ]
      )
    }) %>%
      make_edges_sf()

    p <- plot_combined_flow(
      edges_sf = edges_combined_sf,
      scenario_name = scenario_name,
      title_label = spec$TitleLabel,
      palette = spec$Palette
    )
    if (is.null(p)) next

    ggsave(
      filename = file.path(
        combined_dir,
        paste0(
          "global_",
          scenario_name,
          "_vff_l_vfb_l_combined_",
          spec$FileTag,
          "_",
          plot_year,
          ".png"
        )
      ),
      plot = p,
      width = 300,
      height = 130,
      units = "mm",
      dpi = 300,
      bg = "white"
    )
  }
})

# NH3 trade flow plots --------------------------------------------------------

nh3_plot_threshold <- 1

if (!file.exists(nh3_node_csv)) {
  stop(
    "Missing NH3 node coordinate CSV: ",
    nh3_node_csv,
    ". Run code/worldmap_plot.R first."
  )
}

nh3_nodes_tbl <- read.csv(nh3_node_csv, stringsAsFactors = FALSE) %>%
  transmute(N_PSY, lon = as.numeric(lon), lat = as.numeric(lat))

read_nh3_trade_value <- function(scenario_name, value_spec) {
  gdx_path <- file.path(prm_dir, scenario_name, paste0(plot_year, ".gdx"))
  if (!file.exists(gdx_path)) {
    stop("Missing GDX file: ", gdx_path)
  }

  value_symbol <- value_spec$Symbol
  direction <- value_spec$Direction

  df_raw <- rgdx.param(gdx_path, value_symbol) %>%
    mutate(across(where(is.factor), as.character)) %>%
    filter(str_detect(L, "^R31-[A-Z0-9]+-NH3_R31-[A-Z0-9]+-NH3$"))

  df_link <- df_raw %>%
    group_by(L) %>%
    summarise(Value = sum(.data[[value_symbol]], na.rm = TRUE), .groups = "drop") %>%
    filter(Value > nh3_plot_threshold)

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

  link_nodes <- str_match(df_link$L, "^R31-([A-Z0-9]+)-NH3_R31-([A-Z0-9]+)-NH3$")

  df_link %>%
    mutate(
      Scenario = scenario_name,
      Symbol = value_symbol,
      N0 = link_nodes[, 2],
      N1 = link_nodes[, 3]
    ) %>%
    mutate(
      N0_plot = if (direction == "backward") N1 else N0,
      N1_plot = if (direction == "backward") N0 else N1,
      N0 = N0_plot,
      N1 = N1_plot
    ) %>%
    select(-N0_plot, -N1_plot)
}

make_nh3_trade_edges_sf <- function(df_link) {
  missing_nodes <- setdiff(unique(c(df_link$N0, df_link$N1)), nh3_nodes_tbl$N_PSY)
  if (length(missing_nodes) > 0) {
    stop("Missing NH3 node coordinates: ", paste(missing_nodes, collapse = ", "))
  }

  edges <- df_link %>%
    inner_join(nh3_nodes_tbl, by = c("N0" = "N_PSY")) %>%
    rename(x0 = lon, y0 = lat) %>%
    inner_join(nh3_nodes_tbl, by = c("N1" = "N_PSY")) %>%
    rename(x1 = lon, y1 = lat) %>%
    filter(!is.na(x0), !is.na(y0), !is.na(x1), !is.na(y1)) %>%
    filter(Value > nh3_plot_threshold)

  if (nrow(edges) == 0) {
    return(st_sf(edges, geometry = st_sfc(crs = 4326)))
  }

  lines_sfc <- pmap(
    edges[, c("x0", "y0", "x1", "y1")],
    ~ st_linestring(matrix(c(..1, ..2, ..3, ..4), ncol = 2, byrow = TRUE))
  )

  st_sf(edges, geometry = st_sfc(lines_sfc, crs = 4326))
}

nh3_value_specs <- value_specs %>%
  filter(Symbol %in% c("vff_l", "vfb_l"))

walk(seq_len(nrow(nh3_value_specs)), function(j) {
  value_spec <- nh3_value_specs[j, ]

  for (scenario_name in scenarios) {
    plot_dir <- file.path(base_dir, scenario_name, "plot_trade_nh3", value_spec$Symbol)
    dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

    edges_sf <- read_nh3_trade_value(
      scenario_name = scenario_name,
      value_spec = value_spec
    ) %>%
      make_nh3_trade_edges_sf()

    p <- plot_transmission(
      edges_sf = edges_sf,
      scenario_name = scenario_name,
      title_label = "NH3 trade",
      palette = "magma",
      value_spec = value_spec
    )
    if (is.null(p)) next

    ggsave(
      filename = file.path(
        plot_dir,
        paste0(
          "global_",
          scenario_name,
          "_",
          value_spec$FileTag,
          "_NH3_trade_",
          plot_year,
          ".png"
        )
      ),
      plot = p,
      width = 300,
      height = 130,
      units = "mm",
      dpi = 300,
      bg = "white"
    )
  }
})

for (scenario_name in scenarios) {
  nh3_combined_dir <- file.path(base_dir, scenario_name, "plot_trade_nh3", "vff_l_vfb_l_combined")
  dir.create(nh3_combined_dir, recursive = TRUE, showWarnings = FALSE)

  edges_combined_sf <- map_dfr(seq_len(nrow(nh3_value_specs)), function(j) {
    read_nh3_trade_value(
      scenario_name = scenario_name,
      value_spec = nh3_value_specs[j, ]
    )
  }) %>%
    make_nh3_trade_edges_sf()

  p <- plot_combined_flow(
    edges_sf = edges_combined_sf,
    scenario_name = scenario_name,
    title_label = "NH3 trade",
    palette = "magma"
  )
  if (is.null(p)) next

  ggsave(
    filename = file.path(
      nh3_combined_dir,
      paste0(
        "global_",
        scenario_name,
        "_vff_l_vfb_l_combined_NH3_trade_",
        plot_year,
        ".png"
      )
    ),
    plot = p,
    width = 300,
    height = 130,
    units = "mm",
    dpi = 300,
    bg = "white"
  )
}
