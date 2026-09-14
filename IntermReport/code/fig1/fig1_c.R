# FIGURE 1C PANEL --------------------------------------------------------------

make_fig1_c <- function(data) {
  assert_panel_data(data)

  scenario_lookup <- scenario_crosswalk(data)
  emission_spec <- data$variable_specs$emisec %>%
    dplyr::mutate(
      Variable = as.character(.data$Variable),
      Legend = as.character(.data$Legend),
      Color = as.character(.data$Color)
    )
  energy_supply_variable <- "Emi_CO2_Ene_Sup"
  power_variable <- "Emi_CO2_Ene_Sup_Ele"
  other_emission_spec <- emission_spec %>%
    dplyr::filter(.data$Variable != energy_supply_variable)
  emission_levels <- c("Power", "Other supply", other_emission_spec$Legend)
  emission_colors <- c(
    "Power" = EMISSION_POWER_COLOR,
    "Other supply" = emission_spec$Color[emission_spec$Variable == energy_supply_variable],
    stats::setNames(other_emission_spec$Color, other_emission_spec$Legend)
  )

  assert_variables_present(
    data$aimt_iamc,
    c(emission_spec$Variable, power_variable),
    "Fig. 1-c AIM-Technology input"
  )

  emissions_2050 <- data$aimt_iamc %>%
    dplyr::filter(.data$Region == "World", .data$Year == 2050) %>%
    dplyr::inner_join(scenario_lookup, by = "Scenario")

  supply_data <- emissions_2050 %>%
    dplyr::filter(.data$Variable %in% c(energy_supply_variable, power_variable)) %>%
    dplyr::group_by(.data$ScenarioKey, .data$Variable) %>%
    dplyr::summarise(Value = sum(.data$Value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = "Variable", values_from = "Value") %>%
    dplyr::transmute(
      ScenarioKey,
      Power = .data[[power_variable]],
      `Other supply` = .data[[energy_supply_variable]] - .data[[power_variable]]
    ) %>%
    tidyr::pivot_longer(
      cols = c("Power", "Other supply"),
      names_to = "Legend",
      values_to = "Value"
    )
  sector_data <- emissions_2050 %>%
    dplyr::inner_join(other_emission_spec, by = "Variable") %>%
    dplyr::group_by(.data$ScenarioKey, .data$Legend) %>%
    dplyr::summarise(Value = sum(.data$Value), .groups = "drop")

  plot_data <- dplyr::bind_rows(supply_data, sector_data) %>%
    tidyr::complete(
      ScenarioKey,
      Legend = emission_levels,
      fill = list(Value = 0)
    ) %>%
    dplyr::mutate(Legend = factor(.data$Legend, levels = emission_levels))

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$ScenarioKey, y = .data$Value, fill = .data$Legend)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = "grey35") +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_manual(values = emission_colors, drop = FALSE) +
    ggplot2::labs(
      title = NULL,
      x = NULL,
      y = expression("CO"[2] * " emissions (Mt CO"[2] * "/yr)"),
      fill = NULL
    ) +
    MyTheme +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1))
}
