# FIGURE 1B PANEL --------------------------------------------------------------

make_fig1_b <- function(data) {
  assert_panel_data(data)

  scenario_lookup <- scenario_crosswalk(data)
  sectors <- c(
    Industry = "Fin_Ene_Ind",
    Buildings = "Fin_Ene_Res_and_Com",
    Transport = "Fin_Ene_Tra"
  )
  suffix_spec <- FINAL_ENERGY_SPEC %>%
    dplyr::mutate(Suffix = sub("^Fin_Ene", "", .data$Variable)) %>%
    dplyr::select("Suffix", "Legend", "Multiplier")
  sector_spec <- dplyr::bind_rows(lapply(
    names(sectors),
    function(sector_name) {
      suffix_spec %>%
        dplyr::mutate(
          Sector = sector_name,
          Variable = paste0(unname(sectors[[sector_name]]), .data$Suffix)
        )
    }
  ))

  plot_data <- data$aimt_iamc %>%
    dplyr::filter(.data$Region == "World", .data$Year == 2050) %>%
    dplyr::inner_join(scenario_lookup, by = "Scenario") %>%
    dplyr::inner_join(sector_spec, by = "Variable", relationship = "many-to-many") %>%
    dplyr::group_by(.data$ScenarioKey, .data$Sector, .data$Legend) %>%
    dplyr::summarise(Value = sum(.data$Value * .data$Multiplier), .groups = "drop") %>%
    tidyr::complete(
      ScenarioKey,
      Sector = names(sectors),
      Legend = CARRIER_LEVELS,
      fill = list(Value = 0)
    ) %>%
    dplyr::mutate(
      Sector = factor(.data$Sector, levels = names(sectors)),
      Legend = factor(.data$Legend, levels = CARRIER_LEVELS)
    )

  if (any(plot_data$Value < -1e-8)) {
    stop("Fig. 1-b produced a negative carrier value after separating ammonia from hydrogen.")
  }

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$ScenarioKey, y = .data$Value, fill = .data$Legend)) +
    ggplot2::geom_col(
      width = 0.72,
      position = ggplot2::position_stack(reverse = TRUE)
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data$Sector), nrow = 1) +
    ggplot2::scale_fill_manual(
      values = CARRIER_COLORS,
      breaks = rev(CARRIER_LEVELS),
      drop = FALSE
    ) +
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
    ggplot2::theme(legend.position = "none")
}
