# FIGURE 1D PANEL --------------------------------------------------------------

make_fig1_d <- function(data) {
  assert_panel_data(data)

  scenario_lookup <- scenario_crosswalk(data)
  years <- seq(2020, 2050, by = 5)
  # Active ele_map.gms mappings. Coal includes coal-biomass and coal-hydrogen
  # cofiring. Gas-hydrogen cofiring is exported separately and is added to Gas.
  generation_spec <- tibble::tribble(
    ~Variable, ~Legend, ~Color, ~Sign,
    "Sec_Ene_Ele_Oil",         "Oil",       "sandybrown",     1,
    "Sec_Ene_Ele_Coa",         "Coal",      "grey60",        1,
    "Sec_Ene_Ele_Gas",         "Gas",       "lightgoldenrod", 1,
    "Sec_Ene_Ele_Gas_Hyd_Cof", "Gas",       "lightgoldenrod", 1,
    "Sec_Ene_Ele_Nuc",         "Nuclear",   "moccasin",       1,
    "Sec_Ene_Ele_Hyp",         "Hydro",     "lightsteelblue", 1,
    "Sec_Ene_Ele_Bio",         "Biomass",   "#A9D65D",        1,
    "Sec_Ene_Ele_Geo",         "Geothermal", "peru",          1,
    "Sec_Ene_Ele_Solar",       "Solar",     "lightsalmon",    1,
    "Sec_Ene_Ele_Win",         "Wind",      "lightskyblue3",  1,
    "Sec_Ene_Ele_Hyd",         "Hydrogen",  "thistle2",       1
  )
  loss_spec <- tibble::tribble(
    ~Variable, ~Legend, ~Color, ~Sign,
    "Sec_Ene_Ele_Cur", "Curtailment", "deeppink3", -1,
    "Sec_Ene_Ele_Sto_Los", "Storage loss", "springgreen2", -1,
    "Sec_Ene_Ele_Tra_Los", "Transmission loss", "seashell3", -1
  )
  electricity_spec <- dplyr::bind_rows(generation_spec, loss_spec)

  assert_variables_present(
    data$psm_iamc,
    c(
      "Sec_Ene_Ele_Solar", "Sec_Ene_Ele_Win", "Sec_Ene_Ele_Hyp",
      "Sec_Ene_Ele_Nuc", "Sec_Ene_Ele_Bio", "Sec_Ene_Ele_Gas",
      "Sec_Ene_Ele_Cur", "Sec_Ene_Ele_Sto_Los", "Sec_Ene_Ele_Tra_Los"
    ),
    "Fig. 1-d PSM input"
  )
  # iamc_gdx is sparse: variables that are zero for the entire run are absent.
  # Keep only exported series so zero-only optional technologies do not abort.
  electricity_spec <- electricity_spec %>%
    dplyr::filter(.data$Variable %in% unique(data$psm_iamc$Variable))

  plot_data <- data$psm_iamc %>%
    dplyr::filter(
      .data$Region %in% PSM_WORLD_REGIONS,
      .data$Year %in% years
    ) %>%
    dplyr::inner_join(scenario_lookup, by = "Scenario") %>%
    dplyr::inner_join(electricity_spec, by = "Variable") %>%
    dplyr::group_by(.data$ScenarioKey, .data$Year, .data$Legend, .data$Color, .data$Sign) %>%
    dplyr::summarise(Value = sum(.data$Value) * dplyr::first(.data$Sign), .groups = "drop")

  legend_spec <- electricity_spec[
    !duplicated(electricity_spec$Legend),
    c("Legend", "Color")
  ]
  legend_levels <- legend_spec$Legend
  generation_levels <- unique(generation_spec$Legend)
  loss_levels <- unique(loss_spec$Legend)
  legend_display_levels <- c(
    rev(intersect(generation_levels, legend_levels)),
    rev(intersect(loss_levels, legend_levels))
  )
  legend_colors <- stats::setNames(legend_spec$Color, legend_spec$Legend)
  plot_data <- plot_data %>%
    tidyr::complete(
      ScenarioKey,
      Year = years,
      Legend = legend_levels,
      fill = list(Value = 0)
    ) %>%
    dplyr::mutate(Legend = factor(.data$Legend, levels = legend_levels))

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$Year, y = .data$Value, fill = .data$Legend)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = "grey35") +
    ggplot2::geom_area(
      position = ggplot2::position_stack(reverse = TRUE),
      linewidth = 0
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data$ScenarioKey), nrow = 1) +
    ggplot2::scale_fill_manual(
      values = legend_colors,
      breaks = legend_display_levels,
      drop = FALSE
    ) +
    ggplot2::scale_x_continuous(breaks = seq(2020, 2050, by = 10)) +
    ggplot2::labs(
      title = NULL,
      x = NULL,
      y = "Electricity (EJ/yr)",
      fill = NULL,
      caption = "Generation is positive; curtailment and system losses are negative."
    ) +
    MyTheme +
    ggplot2::theme(
      legend.position = "right",
      plot.caption = ggplot2::element_text(size = 6.5, hjust = 0)
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1))
}
