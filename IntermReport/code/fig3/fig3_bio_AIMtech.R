# AIM-TECHNOLOGY BIOMASS POTENTIAL AND TRADE MAP ----------------------------
# Copied from fig3_bio.R; only model input preparation is changed.
# Domestic resource availability: emax(ME,MK), PJ/yr -> EJ/yr / 1000.
# Directional trade: trade_merged, already EJ/yr; CRNR2 solid, CRNR1 liquid.
make_fig3_bio_AIMtech <- function(aimt_biomass, geodata, scenario,
                                  node_threshold = 0.1, bio_threshold = 0.01) {
  if (!all(c("limits", "trade", "metadata") %in% names(aimt_biomass)) ||
      aimt_biomass$metadata$scenario != scenario || aimt_biomass$metadata$year != 2050) {
    stop("AIM-Technology biomass inputs must match the scenario and year 2050.")
  }
  limits <- aimt_biomass$limits %>%
    dplyr::filter(.data$Scenario == scenario) %>%
    dplyr::mutate(Region = sub("^R31-", "", .data$Region)) %>%
    dplyr::filter(.data$Region %in% geodata$regions$Region)
  resources <- c("CRNAGR", "CRNFRR", "CRNMAN", "CRNMSW",
                  paste0("CRNP0", 1:8))
  expected <- tidyr::expand_grid(Region = unique(geodata$regions$Region), Resource = resources)
  if (nrow(dplyr::anti_join(expected, limits, by = c("Region", "Resource"))) ||
      anyDuplicated(limits[c("Region", "Resource")]) ||
      any(!is.finite(limits$Value) | limits$Value < 0)) {
    stop("Incomplete, duplicate or invalid AIM-Technology biomass resource limits.")
  }
  # Sparse GDX parameter records omit zero CRN1ST limits in nine regions.
  # Other twelve resource caps must be present; do not infer missing data.
  limits <- limits %>% tidyr::complete(Region, Resource = c(resources, "CRN1ST"),
                                       fill = list(Value = 0, Scenario = scenario))
  node_totals <- limits %>% dplyr::group_by(.data$Region) %>%
    dplyr::summarise(Total = sum(.data$Value), .groups = "drop")
  max_total <- max(node_totals$Total)
  if (!is.finite(max_total) || max_total <= 0) {
    stop("AIM-Technology biomass potential must have a positive finite maximum.")
  }
  node_colors <- c("Energy crops" = "#A9D65D", "Other biomass" = "#238B45")
  node_values <- limits %>%
    dplyr::mutate(Category = ifelse(grepl("^CRNP0[1-8]$", .data$Resource),
                                    "Energy crops", "Other biomass")) %>%
    dplyr::group_by(.data$Region, .data$Category) %>%
    dplyr::summarise(Value = sum(.data$Value), .groups = "drop") %>%
    dplyr::left_join(node_totals, by = "Region") %>%
    dplyr::filter(.data$Total >= node_threshold, .data$Total > 0) %>%
    dplyr::mutate(Half = "full", Order = match(.data$Category, names(node_colors))) %>%
    dplyr::left_join(geodata$regions, by = "Region")
  max_radius_mm <- 6.2
  size_legend <- fig3_size_legend(
    max_value = max_total, max_radius = max_radius_mm,
    anchor_lon = -165, anchor_lat = -45,
    direction = "vertical", title = "Availability (EJ/yr)"
  )
  color_legend <- fig3_color_legend(
    categories = names(node_colors),
    anchor_lon = -125, anchor_lat = -32,
    title = "", spacing_lat = 9
  )

  trade <- aimt_biomass$trade %>%
    dplyr::filter(.data$Scenario == scenario) %>%
    dplyr::mutate(Origin = sub("^R31-", "", .data$Origin),
                  Destination = sub("^R31-", "", .data$Destination))
  missing_regions <- setdiff(unique(c(trade$Origin, trade$Destination)), geodata$regions$Region)
  if (length(missing_regions)) stop("Unmapped AIM-Technology trade regions: ",
                                     paste(missing_regions, collapse = ", "))
  trade <- trade %>%
    dplyr::group_by(.data$Origin, .data$Destination, .data$Commodity) %>%
    dplyr::summarise(FlowEJ = sum(.data$Value), .groups = "drop") %>%
    dplyr::mutate(
      lon0 = geodata$regions$lon[match(.data$Origin, geodata$regions$Region)],
      lat0 = geodata$regions$lat[match(.data$Origin, geodata$regions$Region)],
      lon1 = geodata$regions$lon[match(.data$Destination, geodata$regions$Region)],
      lat1 = geodata$regions$lat[match(.data$Destination, geodata$regions$Region)]
    ) %>%
    dplyr::filter(.data$Origin != .data$Destination, .data$FlowEJ >= bio_threshold,
                  abs(.data$lon1 - .data$lon0) <= 180)
  solid_links <- trade %>% dplyr::filter(.data$Commodity == "CRNR2") %>%
    dplyr::mutate(Carrier = "Solid biomass")
  liquid_links <- trade %>% dplyr::filter(.data$Commodity == "CRNR1") %>%
    dplyr::mutate(Carrier = "Liquid biomass")
  transport_colors <- c("Solid biomass" = "#359EAA", "Liquid biomass" = "#006D77")
  p <- fig3_map_base(geodata) +
    ggplot2::geom_point(
      data = geodata$regions,
      ggplot2::aes(x = .data$lon, y = .data$lat),
      shape = 21, size = 2.1, stroke = 0.45,
      color = "grey25", fill = "white",
      inherit.aes = FALSE, show.legend = FALSE
    ) +
    # Opposite curvature separates solid and liquid biomass when both use the same
    # schematic R31 connection.
    ggplot2::geom_curve(
      data = solid_links,
      ggplot2::aes(
        x = .data$lon0, y = .data$lat0, xend = .data$lon1, yend = .data$lat1,
        linewidth = .data$FlowEJ, color = .data$Carrier
      ),
      curvature = 0.12, alpha = 0.85, lineend = "round",
      arrow = grid::arrow(length = grid::unit(1.8, "mm"), type = "closed")
    ) +
    ggplot2::geom_curve(
      data = liquid_links,
      ggplot2::aes(
        x = .data$lon0, y = .data$lat0, xend = .data$lon1, yend = .data$lat1,
        linewidth = .data$FlowEJ, color = .data$Carrier
      ),
      curvature = -0.12, alpha = 0.85, lineend = "round",
      arrow = grid::arrow(length = grid::unit(1.8, "mm"), type = "closed")
    ) +
    fig3_pie_layer(
      node_values, max_value = max_total, max_radius_mm = max_radius_mm
    ) +
    ggplot2::geom_point(
      data = size_legend$labels,
      ggplot2::aes(x = .data$CenterLon, y = .data$CenterLat, size = .data$Value),
      inherit.aes = FALSE, shape = 21, fill = "white", color = "grey25",
      stroke = 0.35, show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = size_legend$labels,
      ggplot2::aes(
        x = .data$LabelLon, y = .data$LabelLat,
        label = .data$Label, hjust = .data$LabelHjust
      ),
      inherit.aes = FALSE, size = 5.2
    ) +
    ggplot2::geom_text(
      data = size_legend$title,
      ggplot2::aes(x = .data$lon, y = .data$lat, label = .data$label),
      inherit.aes = FALSE, hjust = 0, size = 5.6, fontface = "bold"
    ) +
    ggplot2::geom_point(
      data = color_legend$items,
      ggplot2::aes(x = .data$lon, y = .data$lat, fill = .data$Category),
      inherit.aes = FALSE, shape = 22, size = 3.2,
      color = "grey35", stroke = 0.2, show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = color_legend$items,
      ggplot2::aes(x = .data$label_lon, y = .data$lat, label = .data$label),
      inherit.aes = FALSE, hjust = 0, size = 5.2
    ) +
    ggplot2::geom_text(
      data = color_legend$title,
      ggplot2::aes(x = .data$lon, y = .data$lat, label = .data$label),
      inherit.aes = FALSE, hjust = 0, size = 5.6, fontface = "bold"
    ) +
    ggplot2::scale_color_manual(
      values = transport_colors,
      limits = names(transport_colors), breaks = names(transport_colors), drop = FALSE,
      name = ""
    ) +
    ggplot2::scale_fill_manual(
      values = node_colors,
      breaks = names(node_colors), drop = FALSE,
      name = "Biomass availability", guide = "none"
    ) +
    ggplot2::scale_size_area(
      limits = c(0, max_total), max_size = 2 * max_radius_mm,
      guide = "none"
    ) +
    ggplot2::scale_linewidth_continuous(
      range = c(0.15, 3), trans = "sqrt",
      breaks = scales::breaks_pretty(n = 4), name = "Transport volume (EJ/yr)"
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(linewidth = 1.1, alpha = 1), nrow = 1, order = 1
      ),
      fill = "none",
      linewidth = ggplot2::guide_legend(nrow = 1, order = 2)
    ) +
    ggplot2::theme(
      legend.position = "inside",
      legend.position.inside = c(0.57, 0.025),
      legend.justification = c(0.5, 0),
      legend.box = "horizontal",
      legend.background = ggplot2::element_rect(
        fill = scales::alpha("white", 0.82), color = NA
      ),
      legend.margin = ggplot2::margin(1, 2, 1, 2, unit = "mm")
    )

  attr(p, "fig3_bio_AIMtech_limits") <- limits
  attr(p, "fig3_bio_AIMtech_totals") <- node_totals
  attr(p, "fig3_bio_AIMtech_nodes") <- node_values
  attr(p, "fig3_bio_AIMtech_transport") <- list(
    solid_biomass = solid_links, liquid_biomass = liquid_links
  )
  attr(p, "fig3_bio_AIMtech_size_legend_values") <- size_legend$values
  p
}
