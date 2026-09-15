# FIGURE 2C PANEL --------------------------------------------------------------

make_fig2_c <- function(primary_2050) {
  assert_primary_data(primary_2050)
  scenario_lookup <- primary_scenario_crosswalk(primary_2050)
  carrier_levels <- c("Hydrogen", "Ammonia", "Synfuel")

  directed_flow <- function(dataset, direction) {
    dataset %>%
      dplyr::filter(.data$Value > 0) %>%
      dplyr::mutate(Carrier = classify_transport_link(.data$Link)) %>%
      dplyr::filter(!is.na(.data$Carrier)) %>%
      split_primary_links() %>%
      dplyr::mutate(
        FromNode = if (direction == "forward") .data$Node0 else .data$Node1,
        ToNode = if (direction == "forward") .data$Node1 else .data$Node0,
        FromLocation = sub("-(H2|H2TRD|NH3|NH3TRD|SYF|SYFTRD|SYM|SYMTRD)$", "", .data$FromNode),
        ToLocation = sub("-(H2|H2TRD|NH3|NH3TRD|SYF|SYFTRD|SYM|SYMTRD)$", "", .data$ToNode),
        FromRegion = primary_node_region(.data$FromNode, primary_2050$psm_region_map),
        ToRegion = primary_node_region(.data$ToNode, primary_2050$psm_region_map),
        R31Scope = dplyr::case_when(
          is.na(.data$FromRegion) | is.na(.data$ToRegion) ~ "Unmapped",
          .data$FromRegion == .data$ToRegion ~ "Within R31 region",
          TRUE ~ "Across R31 regions"
        )
      ) %>%
      # R31Scope is retained for auditing; the panel totals every physical
      # node-to-node link, including links within one R31 region.
      dplyr::filter(.data$FromLocation != .data$ToLocation) %>%
      dplyr::select(
        "Scenario", "Carrier", "FromNode", "ToNode", "FromLocation", "ToLocation",
        "FromRegion", "ToRegion", "R31Scope", "Value"
      )
  }

  flows <- dplyr::bind_rows(
    directed_flow(primary_2050$psm_vff_l, "forward"),
    directed_flow(primary_2050$psm_vfb_l, "backward")
  )
  plot_data <- flows %>%
    dplyr::group_by(.data$Scenario, .data$Carrier) %>%
    dplyr::summarise(TransportEJ = sum(.data$Value), .groups = "drop") %>%
    tidyr::complete(
      Scenario = unname(primary_2050$metadata$scenario_map),
      Carrier = carrier_levels,
      fill = list(TransportEJ = 0)
    ) %>%
    dplyr::inner_join(scenario_lookup, by = "Scenario") %>%
    dplyr::mutate(Carrier = factor(.data$Carrier, levels = carrier_levels))

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$ScenarioKey, y = .data$TransportEJ, fill = .data$Carrier)
  ) +
    ggplot2::geom_col(width = 0.65, color = "white", linewidth = 0.2) +
    ggplot2::facet_wrap(ggplot2::vars(.data$Carrier), nrow = 1, drop = FALSE) +
    ggplot2::scale_fill_manual(values = CARRIER_COLORS, drop = FALSE) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(
      title = "Energy transport",
      x = NULL,
      y = "Gross link throughput in 2050 (EJ/yr)",
      fill = "Carrier"
    ) +
    MyTheme +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 9),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 6.5),
      axis.text.y = ggplot2::element_text(size = 6.5),
      axis.title.y = ggplot2::element_text(size = 7.5),
      legend.position = "none",
      plot.caption = ggplot2::element_text(size = 5.5, hjust = 0)
    )

  attr(p, "fig2_c_data") <- plot_data
  attr(p, "fig2_c_link_data") <- flows
  p
}
