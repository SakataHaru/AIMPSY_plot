# FIGURE 2B PANEL --------------------------------------------------------------

make_fig2_b <- function(primary_2050) {
  assert_primary_data(primary_2050)
  scenario_lookup <- primary_scenario_crosswalk(primary_2050)

  capacity <- primary_2050$psm_vfs_l %>%
    dplyr::filter(grepl("-AC_.*-AC$", .data$Link), .data$Value > 0) %>%
    split_primary_links() %>%
    dplyr::mutate(
      Region0 = primary_node_region(.data$Node0, primary_2050$psm_region_map),
      Region1 = primary_node_region(.data$Node1, primary_2050$psm_region_map),
      PhysicalLink = ifelse(
        .data$Node0 <= .data$Node1,
        paste(.data$Node0, .data$Node1, sep = "__"),
        paste(.data$Node1, .data$Node0, sep = "__")
      )
    ) %>%
    dplyr::filter(!is.na(.data$Region0), !is.na(.data$Region1), .data$Region0 != .data$Region1)

  duplicate_check <- capacity %>%
    dplyr::group_by(.data$Scenario, .data$PhysicalLink) %>%
    dplyr::summarise(
      Records = dplyr::n(),
      MinGW = min(.data$Value),
      MaxGW = max(.data$Value),
      .groups = "drop"
    )
  inconsistent <- duplicate_check %>%
    dplyr::filter(.data$Records > 1L, abs(.data$MaxGW - .data$MinGW) > 1e-8)
  if (nrow(inconsistent) > 0L) {
    stop("Fig. 2-b found directional records with different capacities for one physical AC link.")
  }

  plot_data <- capacity %>%
    dplyr::group_by(.data$Scenario, .data$PhysicalLink) %>%
    dplyr::summarise(CapacityGW = max(.data$Value), .groups = "drop") %>%
    dplyr::group_by(.data$Scenario) %>%
    dplyr::summarise(CapacityTW = sum(.data$CapacityGW) / 1000, .groups = "drop") %>%
    dplyr::inner_join(scenario_lookup, by = "Scenario")

  if (nrow(plot_data) != nrow(scenario_lookup)) {
    stop("Fig. 2-b lacks interregional AC capacity for one or more scenarios.")
  }

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$ScenarioKey, y = .data$CapacityTW, fill = .data$ScenarioKey)
  ) +
    ggplot2::geom_col(width = 0.65, color = "white", linewidth = 0.2) +
    ggplot2::scale_fill_manual(values = SCENARIO_COLORS, drop = FALSE) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(
      title = "Transmission",
      x = NULL,
      y = "Capacity in 2050 (TW)",
      fill = "Scenario"
    ) +
    MyTheme +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 9),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 6.5),
      axis.text.y = ggplot2::element_text(size = 6.5),
      axis.title.y = ggplot2::element_text(size = 7.5),
      legend.position = "none"
    )

  attr(p, "fig2_b_data") <- plot_data
  attr(p, "fig2_b_duplicate_check") <- duplicate_check
  p
}
