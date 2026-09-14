# FIGURE 3 SHARED MAP GEOMETRY ------------------------------------------------
# The coordinate sources and undirected gross-flow treatment follow
# master/code/ALLPLOT_TRADING.R. Straight lines are network schematics, not
# geographic routes. NH3/SYF links use R31 representative coordinates.

FIG3_GENERATION_COLORS <- c(
  "Solar" = "lightsalmon", "Wind" = "lightskyblue3",
  "Hydro" = "lightsteelblue", "Nuclear" = "moccasin",
  "Biomass" = "#A9D65D", "Thermal / other" = "grey65"
)

FIG3_H2_NODE_COLORS <- c(
  "Electrolysis" = unname(CARRIER_COLORS[["Hydrogen"]]),
  "NH3 cracking" = unname(CARRIER_COLORS[["Ammonia"]]),
  "Direct / other H2 use" = "#8E5EA2",
  "NH3 synthesis" = unname(CARRIER_COLORS[["Ammonia"]]),
  "Synfuel synthesis" = unname(CARRIER_COLORS[["Synfuel"]])
)

fig3_detailed_iamc_rows <- function(dataset, geodata) {
  # PSM IAMC output contains both detailed PSM nodes and aggregate regions.
  # Fig. 3 node glyphs use only rows that match the established detailed-node
  # coordinate table, preventing aggregate and detailed values being combined.
  dataset %>%
    dplyr::semi_join(geodata$nodes, by = c("Region" = "Node"))
}

fig3_link_node <- function(node) {
  sub("-(AC|H2|H2TRD|NH3|NH3TRD|SYF|SYFTRD|SYM|SYMTRD)$", "", node)
}

fig3_map_base <- function(geodata) {
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = geodata$basemap, fill = "grey97", color = "grey85", linewidth = 0.16) +
    ggplot2::coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.position = "bottom", legend.direction = "horizontal",
      legend.title = ggplot2::element_text(size = 8),
      legend.text = ggplot2::element_text(size = 8),
      plot.caption = ggplot2::element_text(size = 7, hjust = 0)
    )
}

fig3_gross_links <- function(primary, scenario, pattern, coordinates,
                             coordinate_kind = c("node", "region")) {
  coordinate_kind <- match.arg(coordinate_kind)
  symbol_rows <- dplyr::bind_rows(primary$psm_vff_l, primary$psm_vfb_l) %>%
    dplyr::filter(.data$Scenario == scenario, grepl(pattern, .data$Link), .data$Value > 0)
  if (nrow(symbol_rows) == 0L) return(tibble::tibble(
    PhysicalLink = character(), Node0 = character(), Node1 = character(),
    FlowEJ = numeric(), Point0 = character(), Point1 = character(),
    lon0 = numeric(), lat0 = numeric(), lon1 = numeric(), lat1 = numeric()
  ))
  link_rows <- split_primary_links(symbol_rows) %>%
    dplyr::mutate(
      PhysicalLink = ifelse(.data$Node0 <= .data$Node1,
                            paste(.data$Node0, .data$Node1, sep = "__"),
                            paste(.data$Node1, .data$Node0, sep = "__"))
    )
  directional_records <- link_rows %>%
    dplyr::group_by(.data$PhysicalLink) %>%
    dplyr::summarise(DirectionalLabels = dplyr::n_distinct(.data$Link), .groups = "drop")
  if (any(directional_records$DirectionalLabels > 1L)) {
    stop("Fig. 3 found both directional link labels for one physical link; gross flow may double count.")
  }
  links <- link_rows %>%
    dplyr::group_by(.data$PhysicalLink) %>%
    dplyr::summarise(
      Node0 = dplyr::first(.data$Node0), Node1 = dplyr::first(.data$Node1),
      FlowEJ = sum(.data$Value), .groups = "drop"
    )
  if (coordinate_kind == "node") {
    links <- links %>% dplyr::mutate(
      Point0 = sub("-(AC|H2|H2TRD)$", "", .data$Node0),
      Point1 = sub("-(AC|H2|H2TRD)$", "", .data$Node1)
    )
    key <- "Node"
  } else {
    links <- links %>% dplyr::mutate(
      Point0 = sub("^R31-([A-Z0-9]+)-.*$", "\\1", .data$Node0),
      Point1 = sub("^R31-([A-Z0-9]+)-.*$", "\\1", .data$Node1)
    )
    key <- "Region"
  }
  lookup <- coordinates[!duplicated(coordinates[[key]]), ]
  links %>%
    dplyr::mutate(
      lon0 = lookup$lon[match(.data$Point0, lookup[[key]])],
      lat0 = lookup$lat[match(.data$Point0, lookup[[key]])],
      lon1 = lookup$lon[match(.data$Point1, lookup[[key]])],
      lat1 = lookup$lat[match(.data$Point1, lookup[[key]])]
    ) %>%
    dplyr::filter(
      is.finite(.data$lon0), is.finite(.data$lat0),
      is.finite(.data$lon1), is.finite(.data$lat1),
      abs(.data$lon1 - .data$lon0) <= 180
    )
}

fig3_sector_polygons <- function(values, max_value, max_radius = 5) {
  if (nrow(values) == 0L) return(tibble::tibble())
  values <- values %>%
    dplyr::filter(.data$Value > 0, is.finite(.data$lon), is.finite(.data$lat)) %>%
    dplyr::group_by(.data$Region, .data$Half) %>%
    dplyr::arrange(.data$Category, .by_group = TRUE) %>%
    dplyr::mutate(
      HalfTotal = sum(.data$Value),
      AngleStart = dplyr::lag(cumsum(.data$Value), default = 0) / .data$HalfTotal,
      AngleEnd = cumsum(.data$Value) / .data$HalfTotal
    ) %>% dplyr::ungroup()
  dplyr::bind_rows(lapply(seq_len(nrow(values)), function(i) {
    z <- values[i, ]
    radius <- max_radius * sqrt(z$HalfTotal / max_value)
    theta <- if (z$Half == "full") {
      2 * pi * seq(z$AngleStart, z$AngleEnd, length.out = 32)
    } else if (z$Half == "upper") {
      pi * seq(z$AngleStart, z$AngleEnd, length.out = 24)
    } else {
      pi * (1 + seq(z$AngleStart, z$AngleEnd, length.out = 24))
    }
    lon_scale <- max(cos(z$lat * pi / 180), 0.35)
    tibble::tibble(
      Region = z$Region, Category = z$Category,
      Piece = paste(z$Region, z$Half, z$Category, i, sep = "__"),
      lon = c(z$lon, z$lon + radius * cos(theta) / lon_scale, z$lon),
      lat = c(z$lat, z$lat + radius * sin(theta), z$lat)
    )
  }))
}
