# FIGURE 3B: HYDROGEN-BASED CARRIER SYSTEM MAP -------------------------------

make_fig3_b <- function(data, primary_2050, geodata, scenario,
                        node_threshold = 0.1, h2_threshold = 0.01,
                        nh3_threshold = 0.01, syf_threshold = 0.01) {
  assert_panel_data(data)
  assert_primary_data(primary_2050)
  assert_variables_present(data$psm_iamc, "Sec_Ene_Hyd", "Fig. 3-b PSM IAMC data")

  total_h2_use <- data$psm_iamc %>%
    dplyr::filter(
      .data$Scenario == scenario, .data$Year == 2050,
      .data$Variable == "Sec_Ene_Hyd"
    ) %>%
    fig3_detailed_iamc_rows(geodata) %>%
    dplyr::group_by(Region = .data$Region) %>%
    dplyr::summarise(TotalH2Use = sum(.data$Value), .groups = "drop")

  conversion_rows <- function(dataset, direction) {
    dataset %>%
      dplyr::filter(.data$Scenario == scenario, .data$Value > 1e-8) %>%
      split_primary_links() %>%
      dplyr::mutate(
        FromNode = if (direction == "forward") .data$Node0 else .data$Node1,
        ToNode = if (direction == "forward") .data$Node1 else .data$Node0,
        FromCarrier = sub("^.*-", "", .data$FromNode),
        ToCarrier = sub("^.*-", "", .data$ToNode),
        Region = fig3_link_node(.data$FromNode),
        Category = dplyr::case_when(
          .data$FromCarrier == "H2" & .data$ToCarrier == "NH3" ~ "NH3",
          .data$FromCarrier == "H2" & .data$ToCarrier %in% c("SYF", "SYM") ~ "Synfuel",
          TRUE ~ NA_character_
        )
      ) %>%
      dplyr::filter(!is.na(.data$Category), !is.na(.data$Region)) %>%
      dplyr::select("Region", "Category", "Value")
  }

  conversion_use <- dplyr::bind_rows(
    conversion_rows(primary_2050$psm_vff_l, "forward"),
    conversion_rows(primary_2050$psm_vfb_l, "backward")
  ) %>%
    dplyr::group_by(.data$Region, .data$Category) %>%
    dplyr::summarise(Value = sum(.data$Value), .groups = "drop") %>%
    tidyr::complete(Region, Category = c("NH3", "Synfuel"), fill = list(Value = 0)) %>%
    tidyr::pivot_wider(names_from = "Category", values_from = "Value", values_fill = 0)

  # Sec_Ene_Hyd is the existing PSM IAMC total used in ALLPLOT_TRADING.R.
  # Conversion inputs are allocated to NH3 and Synfuel; the non-negative
  # remainder is kept as direct H2 use. Production is intentionally omitted.
  node_values <- total_h2_use %>%
    dplyr::full_join(conversion_use, by = "Region") %>%
    dplyr::mutate(
      TotalH2Use = tidyr::replace_na(.data$TotalH2Use, 0),
      NH3 = tidyr::replace_na(.data$NH3, 0),
      Synfuel = tidyr::replace_na(.data$Synfuel, 0),
      `Direct H2` = pmax(.data$TotalH2Use - .data$NH3 - .data$Synfuel, 0)
    ) %>%
    dplyr::select("Region", "Direct H2", "NH3", "Synfuel") %>%
    tidyr::pivot_longer(-"Region", names_to = "Category", values_to = "Value") %>%
    dplyr::mutate(
      Half = "full",
      Order = match(.data$Category, c("Direct H2", "NH3", "Synfuel"))
    ) %>%
    dplyr::semi_join(geodata$nodes, by = c("Region" = "Node")) %>%
    dplyr::left_join(geodata$nodes, by = c("Region" = "Node"))

  node_totals <- node_values %>%
    dplyr::group_by(.data$Region) %>%
    dplyr::summarise(Total = sum(.data$Value), .groups = "drop")
  active_regions <- node_totals %>% dplyr::filter(.data$Total >= node_threshold)
  max_total <- max(node_totals$Total, na.rm = TRUE)
  node_values <- node_values %>% dplyr::inner_join(active_regions, by = "Region")
  max_radius_mm <- 6.2
  size_legend <- fig3_size_legend(
    max_value = max_total, max_radius = max_radius_mm,
    anchor_lon = -165, anchor_lat = -45,
    direction = "vertical", title = "Scale of production (EJ/yr)"
  )
  color_legend <- fig3_color_legend(
    categories = names(FIG3_H2_NODE_COLORS),
    anchor_lon = -125, anchor_lat = -20,
    title = "", spacing_lat = 9
  )

  h2_links <- fig3_gross_links(
    primary_2050, scenario, ".*-(H2|H2TRD)_.*-(H2|H2TRD)$",
    geodata$nodes, "node"
  ) %>%
    dplyr::filter(.data$FlowEJ >= h2_threshold) %>%
    dplyr::mutate(Carrier = "H2")

  nh3_links <- fig3_directional_trade_links(
    primary_2050, scenario,
    "^R31-[A-Z0-9]+-(NH3|NH3TRD)_R31-[A-Z0-9]+-(NH3|NH3TRD)$",
    geodata$regions
  ) %>%
    dplyr::filter(.data$FlowEJ >= nh3_threshold) %>%
    dplyr::mutate(Carrier = "NH3")

  syf_links <- fig3_directional_trade_links(
    primary_2050, scenario,
    "^R31-[A-Z0-9]+-(SYF|SYFTRD|SYM|SYMTRD)_R31-[A-Z0-9]+-(SYF|SYFTRD|SYM|SYMTRD)$",
    geodata$regions
  ) %>%
    dplyr::filter(.data$FlowEJ >= syf_threshold) %>%
    dplyr::mutate(Carrier = "Synfuel")

  transport_colors <- c(
    H2 = unname(FIG3_H2_NODE_COLORS[["Direct H2"]]),
    NH3 = unname(FIG3_H2_NODE_COLORS[["NH3"]]),
    Synfuel = unname(CARRIER_COLORS[["Synfuel"]])
  )

  p <- fig3_map_base(geodata) +
    ggplot2::geom_point(
      data = geodata$regions,
      ggplot2::aes(x = .data$lon, y = .data$lat),
      shape = 21, size = 2.1, stroke = 0.45,
      color = "grey25", fill = "white",
      inherit.aes = FALSE, show.legend = FALSE
    ) +
    # Opposite curvature separates NH3 and Synfuel when both use the same
    # schematic R31 connection.
    ggplot2::geom_curve(
      data = nh3_links,
      ggplot2::aes(
        x = .data$lon0, y = .data$lat0, xend = .data$lon1, yend = .data$lat1,
        linewidth = .data$FlowEJ, color = .data$Carrier
      ),
      curvature = 0.12, alpha = 0.85, lineend = "round",
      arrow = grid::arrow(length = grid::unit(1.8, "mm"), type = "closed")
    ) +
    ggplot2::geom_curve(
      data = syf_links,
      ggplot2::aes(
        x = .data$lon0, y = .data$lat0, xend = .data$lon1, yend = .data$lat1,
        linewidth = .data$FlowEJ, color = .data$Carrier
      ),
      curvature = -0.12, alpha = 0.85, lineend = "round",
      arrow = grid::arrow(length = grid::unit(1.8, "mm"), type = "closed")
    ) +
    ggplot2::geom_curve(
      data = h2_links,
      ggplot2::aes(x = .data$lon0, y = .data$lat0, xend = .data$lon1, yend = .data$lat1),
      curvature = 0.08, color = "white", linewidth = 0.65,
      alpha = 0.9, lineend = "round", show.legend = FALSE
    ) +
    ggplot2::geom_curve(
      data = h2_links,
      ggplot2::aes(
        x = .data$lon0, y = .data$lat0, xend = .data$lon1, yend = .data$lat1,
        linewidth = .data$FlowEJ, color = .data$Carrier
      ),
      curvature = 0.08, alpha = 0.9, lineend = "round"
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
      values = FIG3_H2_NODE_COLORS,
      breaks = names(FIG3_H2_NODE_COLORS), drop = FALSE,
      name = "Hydrogen use", guide = "none"
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

  attr(p, "fig3_b_nodes") <- node_values
  attr(p, "fig3_b_transport") <- list(
    hydrogen = h2_links, ammonia = nh3_links, synfuel = syf_links
  )
  attr(p, "fig3_b_size_legend_values") <- size_legend$values
  p
}
