# FIGURE 3: BIOMASS TRADE AND DOMESTIC AVAILABILITY -------------------------
# Copied from fig3_b.R, retaining map geometry, circle sizes and legends.
# pmax(ME,MK) is the actual EQ_PSMX annual constraint (TWh -> EJ: 0.0036).
# Domestic resources: R_CRNAGR/FRR/MAN/MSW, R_CRNP01--08 and R_CRN1ST.
# Exclude imports. Apply CRNALL as an additional total cap when it exists.
make_fig3_bio <- function(data, primary_2050, geodata, scenario,
                          node_threshold = 0.1, bio_threshold = 0.01) {
  assert_panel_data(data)
  assert_primary_data(primary_2050)
  if (is.null(primary_2050$psm_biomass_limits)) {
    stop("Reload primary data with load_psm_primary() to read biomass pmax limits.")
  }
  limits <- primary_2050$psm_biomass_limits %>%
    dplyr::filter(.data$Scenario == scenario) %>%
    dplyr::mutate(Region = sub("^R31-", "", .data$Region))
  if (nrow(limits) == 0L || any(!is.finite(limits$Value) | limits$Value < 0)) {
    stop("Missing or invalid biomass pmax limits for scenario: ", scenario)
  }
  if (anyDuplicated(limits[c("Region", "Resource")])) {
    stop("Duplicate region/resource biomass pmax limits.")
  }
  missing_regions <- setdiff(unique(limits$Region), geodata$regions$Region)
  if (length(missing_regions)) {
    stop("Unmapped biomass-limit regions: ", paste(missing_regions, collapse = ", "))
  }
  node_totals <- limits %>%
    dplyr::group_by(.data$Region) %>%
    dplyr::summarise(
      ResourcePotential = if (any(.data$Resource != "CRNALL"))
        sum(.data$Value[.data$Resource != "CRNALL"]) else Inf,
      TotalCap = if (any(.data$Resource == "CRNALL"))
        sum(.data$Value[.data$Resource == "CRNALL"]) else Inf,
      .groups = "drop"
    ) %>%
    dplyr::mutate(Total = pmin(.data$ResourcePotential, .data$TotalCap))
  max_total <- max(node_totals$Total)
  if (!is.finite(max_total) || max_total <= 0) {
    stop("Biomass availability must have a positive finite maximum.")
  }
  # A binding aggregate cap has no resource allocation; do not invent shares.
  if (any(node_totals$TotalCap < node_totals$ResourcePotential)) {
    stop("Binding CRNALL cap: resource shares require an explicit allocation rule.")
  }
  node_colors <- c("Energy crops" = "#A9D65D", "Other biomass" = "#238B45")
  node_values <- limits %>%
    dplyr::filter(.data$Resource != "CRNALL") %>%
    dplyr::mutate(Category = ifelse(grepl("^R_CRNP0[1-8]$", .data$Resource),
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

  solid_links <- fig3_directional_trade_links(
    primary_2050, scenario,
    "^R31-[A-Z0-9]+-(CRN|CRNTRD)_R31-[A-Z0-9]+-(CRN|CRNTRD)$",
    geodata$regions
  ) %>%
    dplyr::filter(.data$Origin != .data$Destination, .data$FlowEJ >= bio_threshold) %>%
    dplyr::mutate(Carrier = "Solid biomass")
  liquid_links <- fig3_directional_trade_links(
    primary_2050, scenario,
    "^R31-[A-Z0-9]+-(BMS|BMSTRD)_R31-[A-Z0-9]+-(BMS|BMSTRD)$",
    geodata$regions
  ) %>%
    dplyr::filter(.data$Origin != .data$Destination, .data$FlowEJ >= bio_threshold) %>%
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

  attr(p, "fig3_bio_limits") <- limits
  attr(p, "fig3_bio_totals") <- node_totals
  attr(p, "fig3_bio_nodes") <- node_values
  attr(p, "fig3_bio_transport") <- list(
    solid_biomass = solid_links, liquid_biomass = liquid_links
  )
  attr(p, "fig3_bio_size_legend_values") <- size_legend$values
  p
}
