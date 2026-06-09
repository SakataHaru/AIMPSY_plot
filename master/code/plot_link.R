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
date <- "2605271157"
scenarios <- c("300C", "300C-LimCCS", "300C-LimCCS_bio", "300C-H2low", "300C-extremeH2")
plot_year <- 2050

ddir <- file.path(aimpsy, scenario_group, region, date)
prm_dir <- file.path(ddir, "gams_output", "gdx_primary")

data_dir <- "data"
base_dir <- file.path("output", paste0("global_", substr(date, 1, 6)))
gwh_to_ej <- 3.6e-6
sampled_days_per_year <- 12
days_per_year <- 365
link_types <- tribble(
  ~LinkType, ~Pattern,          ~NodeSuffix, ~Folder,       ~FileTag, ~TitleLabel,      ~Palette,
  "AC",      ".*-AC_.*-AC$",    "-AC",       "plot_link_ele","AC",    "AC",            "plasma",
  "H2",      ".*-H2_.*-H2$",    "-H2",       "plot_link_h2","H2",     "H2 pipeline",   "viridis"
)

value_specs <- tribble(
  ~Symbol,  ~Aggregation, ~Direction,  ~FileTag, ~LegendTitle,                         ~TitleMetric,
  "vff_l",  "sum_time",   "forward",   "vff_l",  "vff_l annual forward flow (GWh/yr)", "annual forward flow from vff_l (GWh/yr)",
  "vfb_l",  "sum_time",   "backward",  "vfb_l",  "vfb_l annual backward flow (GWh/yr)","annual backward flow from vfb_l (GWh/yr)",
  "vfs_l",  "as_is",      "none",      "vfs_l",  "vfs_l link capacity (GW)",           "link capacity from vfs_l (GW)"
)

arrow_threshold <- 10
h2_link_low <- "#bdd7e7"
h2_link_high <- "#2b8cbe"

is_h2_annual_flow <- function(link_spec, value_spec = NULL) {
  link_spec$LinkType == "H2" && (is.null(value_spec) || value_spec$Symbol %in% c("vff_l", "vfb_l"))
}

is_link_annual_flow <- function(value_spec = NULL) {
  is.null(value_spec) || value_spec$Symbol %in% c("vff_l", "vfb_l")
}

link_value_multiplier <- function(link_spec, value_spec) {
  annualize_multiplier <- if (is_link_annual_flow(value_spec)) days_per_year / sampled_days_per_year else 1
  unit_multiplier <- if (is_h2_annual_flow(link_spec, value_spec)) gwh_to_ej else 1

  annualize_multiplier * unit_multiplier
}

link_arrow_threshold <- function(link_spec, value_spec = NULL) {
  annualize_multiplier <- if (is_link_annual_flow(value_spec)) days_per_year / sampled_days_per_year else 1
  unit_multiplier <- if (is_h2_annual_flow(link_spec, value_spec)) gwh_to_ej else 1

  arrow_threshold * annualize_multiplier * unit_multiplier
}

link_flow_legend_title <- function(link_spec, value_spec = NULL, combined = FALSE) {
  if (combined) {
    if (is_h2_annual_flow(link_spec, value_spec)) {
      "vff_l / vfb_l annual flow (EJ/yr)"
    } else {
      "vff_l / vfb_l annual flow (GWh/yr)"
    }
  } else if (is_h2_annual_flow(link_spec, value_spec)) {
    str_replace(value_spec$LegendTitle, "GWh/yr", "EJ/yr")
  } else {
    value_spec$LegendTitle
  }
}

link_flow_title_metric <- function(link_spec, value_spec, combined = FALSE) {
  if (combined) {
    if (is_h2_annual_flow(link_spec, value_spec)) {
      "combined annual flow from vff_l and vfb_l (EJ/yr)"
    } else {
      "combined annual flow from vff_l and vfb_l (GWh/yr)"
    }
  } else if (is_h2_annual_flow(link_spec, value_spec)) {
    str_replace(value_spec$TitleMetric, "GWh/yr", "EJ/yr")
  } else {
    value_spec$TitleMetric
  }
}

link_flow_labeler <- function(link_spec, value_spec = NULL) {
  if (is_h2_annual_flow(link_spec, value_spec)) {
    scales::label_number(accuracy = 0.00001)
  } else {
    scales::label_number(accuracy = 1)
  }
}

link_flow_color_scale <- function(link_spec, brks, legend_title, value_spec = NULL) {
  flow_labels <- link_flow_labeler(link_spec, value_spec)(brks)
  if (link_spec$LinkType == "H2") {
    scale_color_gradient(
      low = h2_link_low,
      high = h2_link_high,
      breaks = log10(1 + brks),
      labels = flow_labels,
      name = legend_title
    )
  } else {
    scale_color_viridis_c(
      option = link_spec$Palette,
      direction = -1,
      breaks = log10(1 + brks),
      labels = flow_labels,
      name = legend_title
    )
  }
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

node_path <- "C:/Users/Haru Sakata/OneDrive - Kyoto University/My work/AIM-PSY/data/node/nodes_centroid.geojson"
node_csv <- file.path(data_dir, "AIM_R31_NH3_trade_nodes.csv")

if (file.exists(node_path)) {
  nodes_tbl <- st_read(node_path, quiet = TRUE) %>%
    st_drop_geometry() %>%
    transmute(
      N_PSY = as.character(N_PSY),
      lon = as.numeric(centroid_x),
      lat = as.numeric(centroid_y)
    )
} else if (file.exists(node_csv)) {
  nodes_tbl <- read.csv(node_csv, stringsAsFactors = FALSE) %>%
    transmute(
      N_PSY = str_remove(as.character(N_PSY), "^R31-"),
      lon = as.numeric(lon),
      lat = as.numeric(lat)
    )
} else {
  stop("Missing node coordinate file: ", node_path, " or ", node_csv)
}

normalize_link_node <- function(node_name, node_suffix) {
  node_name %>%
    str_remove_all(fixed(node_suffix)) %>%
    str_remove("^R31-")
}

read_link_value <- function(scenario_name, link_pattern, node_suffix, value_spec, value_multiplier = 1) {
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
    mutate(Value = Value * value_multiplier) %>%
    separate(L, sep = "_", into = c("N0", "N1"), remove = FALSE) %>%
    mutate(
      Scenario = scenario_name,
      Symbol = value_symbol,
      N0 = normalize_link_node(N0, node_suffix),
      N1 = normalize_link_node(N1, node_suffix)
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

plot_transmission <- function(edges_sf, scenario_name, link_spec, value_spec) {
  if (nrow(edges_sf) == 0) {
    warning(
      "No plottable ",
      link_spec$TitleLabel,
      " links for scenario: ",
      scenario_name,
      " / ",
      value_spec$Symbol
    )
    return(NULL)
  }

  vmax <- max(edges_sf$Value, na.rm = TRUE)
  brks <- make_flow_breaks(vmax)

  edges_plot <- edges_sf %>%
    mutate(Value_plot = log10(1 + Value))

  edges_plot_df <- edges_plot %>% st_drop_geometry()
  plot_arrow_threshold <- link_arrow_threshold(link_spec, value_spec)
  legend_title <- link_flow_legend_title(link_spec, value_spec)
  title_metric <- link_flow_title_metric(link_spec, value_spec)
  flow_labels <- link_flow_labeler(link_spec, value_spec)(brks)
  edges_line_df <- edges_plot_df %>% filter(Value <= plot_arrow_threshold)
  edges_arrow_df <- edges_plot_df %>% filter(Value > plot_arrow_threshold)
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
      labels = flow_labels,
      name = legend_title
    ) +
    link_flow_color_scale(link_spec, brks, legend_title, value_spec) +
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
        link_spec$TitleLabel,
        " ",
        title_metric,
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

plot_combined_flow <- function(edges_sf, scenario_name, link_spec) {
  if (nrow(edges_sf) == 0) {
    warning("No plottable combined flow links for scenario: ", scenario_name)
    return(NULL)
  }

  vmax <- max(edges_sf$Value, na.rm = TRUE)
  brks <- make_flow_breaks(vmax)

  edges_plot_df <- edges_sf %>%
    mutate(Value_plot = log10(1 + Value)) %>%
    st_drop_geometry()
  plot_arrow_threshold <- link_arrow_threshold(link_spec)
  legend_title <- link_flow_legend_title(link_spec, combined = TRUE)
  title_metric <- link_flow_title_metric(link_spec, value_spec = NULL, combined = TRUE)
  flow_labels <- link_flow_labeler(link_spec)(brks)
  edges_line_df <- edges_plot_df %>% filter(Value <= plot_arrow_threshold)
  edges_arrow_df <- edges_plot_df %>% filter(Value > plot_arrow_threshold)

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
      labels = flow_labels,
      name = legend_title
    ) +
    link_flow_color_scale(link_spec, brks, legend_title) +
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
        link_spec$TitleLabel,
        " ",
        title_metric,
        ", ",
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
        value_spec = value_spec,
        value_multiplier = link_value_multiplier(spec, value_spec)
      ) %>%
        make_edges_sf()

      p <- plot_transmission(
        edges_sf = edges_sf,
        scenario_name = scenario_name,
        link_spec = spec,
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
          value_spec = flow_specs[j, ],
          value_multiplier = link_value_multiplier(spec, flow_specs[j, ])
        )
      }) %>%
      make_edges_sf()

    p <- plot_combined_flow(
      edges_sf = edges_combined_sf,
      scenario_name = scenario_name,
      link_spec = spec
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

# Demand maps and link overlays ----------------------------------------------

main_dir <- file.path(aimpsy, scenario_group, region, date, "main")
merged_gdx_path <- file.path(main_dir, "merged_output.gdx")
link_demand_specs <- tribble(
  ~LinkType, ~Folder,          ~DemandVariable, ~DemandLabel,          ~FillLow,   ~FillHigh,
  "AC",      "plot_link_ele",  "Sec_Ene_Ele",   "Electricity supply",  "#f7fbff", "#08306b",
  "H2",      "plot_link_h2",   "Sec_Ene_Hyd",   "Hydrogen demand",     "#f7f7f7", "#252525"
)

read_demand_values <- function(gdx_path, variables) {
  if (!file.exists(gdx_path)) {
    stop("Missing GDX file: ", gdx_path)
  }

  rgdx.param(gdx_path, "iamc_gdx") %>%
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
      Variable %in% variables,
      Year == plot_year,
      !is.na(Value)
    ) %>%
    group_by(Scenario, Region, Variable, Year) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")
}

link_demand_values <- read_demand_values(merged_gdx_path, link_demand_specs$DemandVariable)

fixed_fill_limits <- function(values) {
  vmax <- max(values, na.rm = TRUE)
  if (!is.finite(vmax) || vmax <= 0) return(NULL)
  c(0, vmax)
}

plot_link_demand_map <- function(map_data, scenario_name, demand_spec) {
  plot_data <- map_data %>%
    filter(Scenario == scenario_name, Variable == demand_spec$DemandVariable)
  fill_limits <- map_data %>%
    filter(Variable == demand_spec$DemandVariable) %>%
    pull(Value) %>%
    fixed_fill_limits()

  ggplot(plot_data) +
    geom_sf(aes(fill = Value), color = "grey35", linewidth = 0.3) +
    scale_fill_gradient(
      low = demand_spec$FillLow,
      high = demand_spec$FillHigh,
      limits = fill_limits,
      na.value = "grey92",
      labels = scales::label_number(accuracy = 0.01),
      name = "EJ/yr"
    ) +
    coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
    theme_void() +
    theme(
      legend.position = c(0.08, 0.42),
      legend.direction = "vertical",
      legend.background = element_rect(fill = "white", color = "grey85"),
      legend.key.width = unit(8, "mm"),
      legend.key.height = unit(18, "mm"),
      plot.title = element_text(hjust = 0.5, size = 12)
    ) +
    labs(title = paste0(scenario_name, " ", demand_spec$DemandLabel, " by R31 region (", plot_year, ")"))
}

plot_link_demand_overlay <- function(map_data, edges_sf, scenario_name, link_spec, demand_spec) {
  if (nrow(edges_sf) == 0) {
    warning("No plottable overlay links for scenario: ", scenario_name, " / ", link_spec$LinkType)
    return(NULL)
  }

  plot_data <- map_data %>%
    filter(Scenario == scenario_name, Variable == demand_spec$DemandVariable)
  fill_limits <- map_data %>%
    filter(Variable == demand_spec$DemandVariable) %>%
    pull(Value) %>%
    fixed_fill_limits()

  vmax <- max(edges_sf$Value, na.rm = TRUE)
  brks <- make_flow_breaks(vmax)

  edges_plot_df <- edges_sf %>%
    mutate(Value_plot = log10(1 + Value)) %>%
    st_drop_geometry()
  plot_arrow_threshold <- link_arrow_threshold(link_spec)
  legend_title <- link_flow_legend_title(link_spec, combined = TRUE)
  flow_labels <- link_flow_labeler(link_spec)(brks)
  edges_line_df <- edges_plot_df %>% filter(Value <= plot_arrow_threshold)
  edges_arrow_df <- edges_plot_df %>% filter(Value > plot_arrow_threshold)

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
      arrow = arrow(length = unit(1.45, "mm"), type = "closed")
    ) +
    scale_fill_gradient(
      low = demand_spec$FillLow,
      high = demand_spec$FillHigh,
      limits = fill_limits,
      na.value = "grey92",
      labels = scales::label_number(accuracy = 0.01),
      name = "Demand (EJ/yr)"
    ) +
    scale_linewidth_continuous(
      range = c(0.25, 1.45),
      breaks = log10(1 + brks),
      labels = flow_labels,
      name = legend_title
    ) +
    link_flow_color_scale(link_spec, brks, legend_title) +
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
      fill = guide_colorbar(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        barwidth = unit(48, "mm"),
        barheight = unit(3, "mm")
      ),
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
        demand_spec$DemandLabel,
        " and ",
        link_spec$TitleLabel,
        " combined transport flow (",
        plot_year,
        ")"
      )
    )
}

link_demand_map_data <- region_boundaries %>%
  left_join(link_demand_values, by = c("region" = "Region"))

link_flow_specs <- value_specs %>% filter(Symbol %in% c("vff_l", "vfb_l"))

for (i in seq_len(nrow(link_types))) {
  link_spec <- link_types[i, ]
  demand_spec <- link_demand_specs %>% filter(LinkType == link_spec$LinkType)

  for (scenario_name in scenarios) {
    demand_dir <- file.path(base_dir, scenario_name, link_spec$Folder, "demand_map")
    overlay_dir <- file.path(base_dir, scenario_name, link_spec$Folder, "demand_link_overlay")
    dir.create(demand_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(overlay_dir, recursive = TRUE, showWarnings = FALSE)

    p_demand <- plot_link_demand_map(link_demand_map_data, scenario_name, demand_spec)
    ggsave(
      filename = file.path(
        demand_dir,
        paste0("global_", scenario_name, "_", demand_spec$DemandVariable, "_demand_map_", plot_year, ".png")
      ),
      plot = p_demand,
      width = 300,
      height = 130,
      units = "mm",
      dpi = 300,
      bg = "white"
    )

    edges_combined_sf <- map_dfr(seq_len(nrow(link_flow_specs)), function(j) {
        read_link_value(
          scenario_name = scenario_name,
          link_pattern = link_spec$Pattern,
          node_suffix = link_spec$NodeSuffix,
          value_spec = link_flow_specs[j, ],
          value_multiplier = link_value_multiplier(link_spec, link_flow_specs[j, ])
        )
      }) %>%
      make_edges_sf()

    p_overlay <- plot_link_demand_overlay(
      map_data = link_demand_map_data,
      edges_sf = edges_combined_sf,
      scenario_name = scenario_name,
      link_spec = link_spec,
      demand_spec = demand_spec
    )
    if (is.null(p_overlay)) next

    ggsave(
      filename = file.path(
        overlay_dir,
        paste0(
          "global_",
          scenario_name,
          "_",
          demand_spec$DemandVariable,
          "_vff_l_vfb_l_combined_",
          link_spec$FileTag,
          "_demand_link_overlay_",
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
