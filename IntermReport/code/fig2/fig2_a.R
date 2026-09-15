# FIGURE 2A PANEL --------------------------------------------------------------

make_fig2_a <- function(data) {
  assert_panel_data(data)

  scenario_lookup <- scenario_crosswalk(data)
  years <- data$psm_iamc %>%
    dplyr::filter(
      .data$Scenario %in% scenario_lookup$Scenario,
      .data$Region %in% PSM_WORLD_REGIONS,
      .data$Year >= 2020,
      .data$Year <= 2050
    ) %>%
    dplyr::distinct(.data$Year) %>%
    dplyr::arrange(.data$Year) %>%
    dplyr::pull(.data$Year)
  if (length(years) == 0L) {
    stop("Fig. 2-a found no PSM model years from 2020 through 2050.")
  }

  capacity_spec <- tibble::tribble(
    ~Variable, ~Technology,
    "Cap_Ele_SolarPV", "Solar PV",
    "Cap_Ele_Win", "Wind",
    "Cap_Ele_Sto_Bat", "Battery",
    "Cap_Hyd_Ele", "Electrolyzer"
  )
  required_variables <- capacity_spec$Variable
  assert_variables_present(data$psm_iamc, required_variables, "Fig. 2-a PSM input")

  capacity_data <- data$psm_iamc %>%
    dplyr::filter(
      .data$Scenario %in% scenario_lookup$Scenario,
      .data$Region %in% PSM_WORLD_REGIONS,
      .data$Year %in% years,
      .data$Variable %in% required_variables
    ) %>%
    dplyr::inner_join(scenario_lookup, by = "Scenario") %>%
    dplyr::group_by(.data$ScenarioKey, .data$Year, .data$Variable) %>%
    dplyr::summarise(Value = sum(.data$Value), .groups = "drop") %>%
    # iamc_gdx is sparse: a missing capacity record is a model zero.
    # This fills only existing model years and does not interpolate years.
    tidyr::complete(
      ScenarioKey,
      Year = years,
      Variable = required_variables,
      fill = list(Value = 0)
    ) %>%
    dplyr::inner_join(capacity_spec, by = "Variable") %>%
    dplyr::mutate(
      Technology = factor(
        .data$Technology,
        levels = c("Solar PV", "Wind", "Battery", "Electrolyzer")
      ),
      # PSM IAMC capacity output is GW; display it in TW for readable axes.
      Value = .data$Value / 1000
    )

  ggplot2::ggplot(
    capacity_data,
    ggplot2::aes(
      x = .data$Year,
      y = .data$Value,
      color = .data$ScenarioKey,
      group = .data$ScenarioKey
    )
  ) +
    ggplot2::geom_line(linewidth = 0.65) +
    ggplot2::geom_point(
      ggplot2::aes(shape = .data$ScenarioKey),
      size = 1.35
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(.data$Technology),
      nrow = 1,
      scales = "free_y",
      drop = FALSE
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::scale_color_manual(values = SCENARIO_COLORS, drop = FALSE) +
    ggplot2::scale_shape_manual(values = SCENARIO_SHAPES, drop = FALSE) +
    ggplot2::scale_x_continuous(
      breaks = years[years %in% c(2020, 2030, 2040, 2050)]
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Installed capacity (TW)",
      color = "Scenario",
      shape = "Scenario"
    ) +
    MyTheme +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 6.5),
      axis.text.y = ggplot2::element_text(size = 6.5),
      axis.title.y = ggplot2::element_text(size = 7.5),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.text = ggplot2::element_text(size = 6.5),
      legend.title = ggplot2::element_text(size = 6.5)
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(nrow = 1),
      shape = ggplot2::guide_legend(nrow = 1)
    )
}
