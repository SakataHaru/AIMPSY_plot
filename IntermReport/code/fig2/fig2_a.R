# FIGURE 2A PANEL --------------------------------------------------------------

make_fig2_a <- function(data) {
  assert_panel_data(data)
  if (!requireNamespace("ggh4x", quietly = TRUE)) {
    stop("Fig. 2-a requires the installed ggh4x package for a facet-specific secondary axis.")
  }

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
  hours_per_year <- 8760
  gwh_to_ej <- 3.6e-6
  capacity_spec <- tibble::tribble(
    ~Variable, ~Technology,
    "Cap_Ele_SolarPV", "Solar PV",
    "Cap_Ele_Win", "Wind",
    "Cap_Ele_Sto_Bat", "Battery",
    "Cap_Hyd_Ele", "Electrolyzer"
  )
  required_variables <- c(capacity_spec$Variable, "Sec_Ene_Inp_Ele_Hyd")
  assert_variables_present(data$psm_iamc, required_variables, "Fig. 2-a PSM input")

  psm_world <- data$psm_iamc %>%
    dplyr::filter(
      .data$Region %in% PSM_WORLD_REGIONS,
      .data$Year %in% years,
      .data$Variable %in% required_variables
    ) %>%
    dplyr::inner_join(scenario_lookup, by = "Scenario") %>%
    dplyr::group_by(.data$ScenarioKey, .data$Year, .data$Variable) %>%
    dplyr::summarise(Value = sum(.data$Value), .groups = "drop") %>%
    # iamc_gdx is sparse: a missing capacity/activity record is a model zero.
    # This fills only existing model years and does not interpolate years.
    tidyr::complete(
      ScenarioKey,
      Year = years,
      Variable = required_variables,
      fill = list(Value = 0)
    )

  capacity_data <- psm_world %>%
    dplyr::inner_join(capacity_spec, by = "Variable") %>%
    dplyr::mutate(
      Technology = factor(
        .data$Technology,
        levels = c("Solar PV", "Wind", "Battery", "Electrolyzer")
      ),
      Metric = "Installed capacity"
    )

  electrolyzer_data <- psm_world %>%
    dplyr::filter(.data$Variable %in% c("Cap_Hyd_Ele", "Sec_Ene_Inp_Ele_Hyd")) %>%
    dplyr::select("ScenarioKey", "Year", "Variable", "Value") %>%
    tidyr::pivot_wider(names_from = "Variable", values_from = "Value") %>%
    dplyr::mutate(
      CapacityFactor = dplyr::if_else(
        .data$Cap_Hyd_Ele > 0,
        100 * .data$Sec_Ene_Inp_Ele_Hyd /
          (.data$Cap_Hyd_Ele * hours_per_year * gwh_to_ej),
        NA_real_
      )
    )

  if (any(electrolyzer_data$CapacityFactor < 0 |
          electrolyzer_data$CapacityFactor > 100, na.rm = TRUE)) {
    stop("Fig. 2-a calculated an electrolyzer capacity factor outside 0-100%.")
  }

  electrolyzer_capacity_max <- max(electrolyzer_data$Cap_Hyd_Ele, na.rm = TRUE)
  if (!is.finite(electrolyzer_capacity_max) || electrolyzer_capacity_max <= 0) {
    stop("Fig. 2-a cannot scale the capacity-factor axis because electrolyzer capacity is zero.")
  }
  capacity_factor_scale <- electrolyzer_capacity_max / 100
  capacity_factor_data <- electrolyzer_data %>%
    dplyr::transmute(
      ScenarioKey,
      Year,
      Technology = factor("Electrolyzer", levels = levels(capacity_data$Technology)),
      Value = .data$CapacityFactor * capacity_factor_scale,
      Metric = "Capacity factor"
    )

  capacity_y_scales <- list(
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))),
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))),
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))),
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.05)),
      sec.axis = ggplot2::sec_axis(
        ~ . / capacity_factor_scale,
        name = "Electrolyzer capacity factor (%)"
      )
    )
  )

  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = capacity_data,
      ggplot2::aes(
        x = .data$Year,
        y = .data$Value,
        color = .data$ScenarioKey,
        linetype = .data$Metric,
        group = .data$ScenarioKey
      ),
      linewidth = 0.65
    ) +
    ggplot2::geom_point(
      data = capacity_data,
      ggplot2::aes(x = .data$Year, y = .data$Value, color = .data$ScenarioKey),
      size = 1.15
    ) +
    ggplot2::geom_line(
      data = capacity_factor_data,
      ggplot2::aes(
        x = .data$Year,
        y = .data$Value,
        color = .data$ScenarioKey,
        linetype = .data$Metric,
        group = .data$ScenarioKey
      ),
      linewidth = 0.65,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(
      data = capacity_factor_data,
      ggplot2::aes(x = .data$Year, y = .data$Value, color = .data$ScenarioKey),
      shape = 1,
      size = 1.35,
      na.rm = TRUE
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(.data$Technology),
      nrow = 1,
      scales = "free_y",
      drop = FALSE
    ) +
    ggh4x::facetted_pos_scales(y = capacity_y_scales) +
    ggplot2::scale_color_manual(values = SCENARIO_COLORS, drop = FALSE) +
    ggplot2::scale_linetype_manual(
      values = c("Installed capacity" = "solid", "Capacity factor" = "dashed")
    ) +
    ggplot2::scale_x_continuous(
      breaks = years[years %in% c(2020, 2030, 2040, 2050)]
    ) +
    ggplot2::labs(
      title = "Capacity expansion and utilization",
      x = NULL,
      y = "Installed capacity (GW)",
      color = "Scenario",
      linetype = NULL,
      caption = "Electrolyzer capacity factor uses annual electricity input to electrolysis and 8,760 h/yr."
    ) +
    MyTheme +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 9),
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 6.5),
      axis.text.y = ggplot2::element_text(size = 6.5),
      axis.title.y = ggplot2::element_text(size = 7.5),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.text = ggplot2::element_text(size = 6.5),
      legend.title = ggplot2::element_text(size = 6.5),
      plot.caption = ggplot2::element_text(size = 5.5, hjust = 0)
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(nrow = 1),
      linetype = ggplot2::guide_legend(nrow = 1)
    )
}
