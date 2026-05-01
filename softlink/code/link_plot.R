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

#for (gams_sys_dir in c("C:/GAMS/38", "C:/GAMS/win64/26.1")) {
#  if (dir.exists(gams_sys_dir)) {
#    igdx(gams_sys_dir)
#    break
#  }
#}

aimpsy <- "//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output"
scenario_group <- "global_softlink"
region <- "global"
date <- "2604281658"
scenarios <- c("300C", "700C")
plot_year <- 2050

ddir <- file.path(aimpsy, scenario_group, region, date)
prm_dir <- file.path(ddir, "gams_output", "gdx_primary")

softlink_dir <- if (basename(normalizePath(getwd(), winslash = "/", mustWork = FALSE)) == "softlink") {
  "."
} else {
  "softlink"
}
out_dir <- file.path(softlink_dir, "output", "global_softlink", "global_12day")
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

  world <- ne_countries(scale = "medium", returnclass = "sf")
  if (is.na(st_crs(world))) world <- st_set_crs(world, 4326)

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
    geom_sf(data = world, fill = NA, color = "grey65", linewidth = 0.25) +
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

  world <- ne_countries(scale = "medium", returnclass = "sf")
  if (is.na(st_crs(world))) world <- st_set_crs(world, 4326)

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
    geom_sf(data = world, fill = NA, color = "grey65", linewidth = 0.25) +
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
    plot_dir <- file.path(out_dir, spec$Folder, value_spec$Symbol)
    dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

    for (scenario_name in scenarios) {
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

  combined_dir <- file.path(out_dir, spec$Folder, "vff_l_vfb_l_combined")
  dir.create(combined_dir, recursive = TRUE, showWarnings = FALSE)

  flow_specs <- value_specs %>%
    filter(Symbol %in% c("vff_l", "vfb_l"))

  for (scenario_name in scenarios) {
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
