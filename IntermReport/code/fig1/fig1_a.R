# FIGURE 1A PANEL --------------------------------------------------------------

make_fig1_a <- function(data) {
  assert_panel_data(data)

  scenario_lookup <- scenario_crosswalk(data)
  years <- seq(2020, 2050, by = 5)

  aimt_data <- data$aimt_iamc %>%
    dplyr::filter(.data$Region == "World", .data$Year %in% years) %>%
    dplyr::inner_join(scenario_lookup, by = "Scenario")
  assert_variables_present(
    aimt_data,
    unique(FINAL_ENERGY_SPEC$Variable),
    "Fig. 1-a AIM-Technology input"
  )

  plot_data <- aimt_data %>%
    dplyr::inner_join(FINAL_ENERGY_SPEC, by = "Variable", relationship = "many-to-many") %>%
    dplyr::group_by(.data$ScenarioKey, .data$Year, .data$Legend) %>%
    dplyr::summarise(Value = sum(.data$Value * .data$Multiplier), .groups = "drop") %>%
    tidyr::complete(
      ScenarioKey,
      Year = years,
      Legend = CARRIER_LEVELS,
      fill = list(Value = 0)
    ) %>%
    dplyr::mutate(Legend = factor(.data$Legend, levels = CARRIER_LEVELS))

  if (any(plot_data$Value < -1e-8)) {
    stop("Fig. 1-a produced a negative carrier value after separating ammonia from hydrogen.")
  }

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$Year, y = .data$Value, fill = .data$Legend)) +
    ggplot2::geom_area(
      position = ggplot2::position_stack(reverse = TRUE),
      linewidth = 0
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data$ScenarioKey), nrow = 1) +
    ggplot2::scale_fill_manual(values = CARRIER_COLORS, drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = seq(2020, 2050, by = 10)) +
    ggplot2::scale_y_continuous(
      breaks = scales::breaks_pretty(n = 3),
      expand = ggplot2::expansion(mult = c(0, 0.04))
    ) +
    ggplot2::labs(
      title = NULL,
      x = NULL,
      y = "Final energy (EJ/yr)",
      fill = NULL
    ) +
    MyTheme +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1, reverse = TRUE))
}
