# Based on IntermReport/code/fig2/fig2_c.R: one bar panel per carrier,
# gross forward + backward throughput across physical node-to-node links.
# Extend NH3 classification to distinguish detailed-node truck links from
# R31-to-R31 sea trade. Include SYM as well as SYF in synthetic fuels.
make_fig2_c <- function(primary_2050) {
  assert_primary_data(primary_2050)
  scenario_lookup <- primary_scenario_crosswalk(primary_2050)
  carrier_levels <- c('Hydrogen', 'Ammonia', 'Synfuel')
  transport_levels <- c('Hydrogen pipeline', 'Ammonia sea', 'Ammonia truck', 'Synfuel sea')
  transport_colors <- c(
    'Hydrogen pipeline' = unname(CARRIER_COLORS[['Hydrogen']]),
    'Ammonia sea' = unname(CARRIER_COLORS[['Ammonia']]),
    'Ammonia truck' = '#E69F00',
    'Synfuel sea' = unname(CARRIER_COLORS[['Synfuel']])
  )
  directed_flow <- function(dataset, direction) {
    dataset %>%
      dplyr::filter(.data$Value > 0) %>%
      dplyr::mutate(Carrier = dplyr::case_when(
        grepl('.*-(H2|H2TRD)_.*-(H2|H2TRD)$', .data$Link) ~ 'Hydrogen',
        grepl('.*-(NH3|NH3TRD)_.*-(NH3|NH3TRD)$', .data$Link) ~ 'Ammonia',
        grepl('^R31-[A-Z0-9]+-(SYF|SYFTRD|SYM|SYMTRD)_R31-[A-Z0-9]+-(SYF|SYFTRD|SYM|SYMTRD)$', .data$Link) ~ 'Synfuel',
        TRUE ~ NA_character_
      )) %>%
      dplyr::filter(!is.na(.data$Carrier)) %>%
      split_primary_links() %>%
      dplyr::mutate(
        FromNode = if (direction == 'forward') .data$Node0 else .data$Node1,
        ToNode = if (direction == 'forward') .data$Node1 else .data$Node0,
        FromLocation = fig3_link_node(.data$FromNode),
        ToLocation = fig3_link_node(.data$ToNode),
        FromRegion = primary_node_region(.data$FromNode, primary_2050$psm_region_map),
        ToRegion = primary_node_region(.data$ToNode, primary_2050$psm_region_map),
        R31Scope = dplyr::case_when(
          is.na(.data$FromRegion) | is.na(.data$ToRegion) ~ 'Unmapped',
          .data$FromRegion == .data$ToRegion ~ 'Within R31 region',
          TRUE ~ 'Across R31 regions'
        ),
        Transport = dplyr::case_when(
          .data$Carrier == 'Hydrogen' ~ 'Hydrogen pipeline',
          .data$Carrier == 'Synfuel' ~ 'Synfuel sea',
          grepl('^R31-', .data$FromLocation) & grepl('^R31-', .data$ToLocation) ~ 'Ammonia sea',
          !grepl('^R31-', .data$FromLocation) & !grepl('^R31-', .data$ToLocation) ~ 'Ammonia truck',
          TRUE ~ NA_character_
        )
      ) %>%
      # Retain links inside a single R31 region; remove same-location interfaces.
      dplyr::filter(.data$FromLocation != .data$ToLocation) %>%
      dplyr::select('Scenario', 'Carrier', 'Transport', 'FromNode', 'ToNode',
                    'FromLocation', 'ToLocation', 'FromRegion', 'ToRegion', 'R31Scope', 'Value')
  }
  flows <- dplyr::bind_rows(
    directed_flow(primary_2050$psm_vff_l, 'forward'),
    directed_flow(primary_2050$psm_vfb_l, 'backward')
  )
  if (anyNA(flows$Transport) || any(flows$R31Scope == 'Unmapped')) {
    stop('Unclassified carrier transport link; inspect carrier/mode and node-region mapping')
  }
  if (any(!is.finite(flows$Value))) stop('Non-finite carrier transport flow')
  specification <- tibble::tibble(
    Carrier = c('Hydrogen', 'Ammonia', 'Ammonia', 'Synfuel'),
    Transport = transport_levels
  )
  plot_data <- tidyr::crossing(
    Scenario = unname(primary_2050$metadata$scenario_map), specification
  ) %>%
    dplyr::left_join(flows %>% dplyr::group_by(.data$Scenario, .data$Carrier, .data$Transport) %>%
      dplyr::summarise(TransportEJ = sum(.data$Value), .groups = 'drop'),
      by = c('Scenario', 'Carrier', 'Transport')) %>%
    dplyr::mutate(TransportEJ = tidyr::replace_na(.data$TransportEJ, 0)) %>%
    dplyr::inner_join(scenario_lookup, by = 'Scenario') %>%
    dplyr::mutate(Carrier = factor(.data$Carrier, levels = carrier_levels),
                  Transport = factor(.data$Transport, levels = transport_levels))
  totals <- plot_data %>% dplyr::group_by(.data$ScenarioKey, .data$Carrier) %>%
    dplyr::summarise(TransportEJ = sum(.data$TransportEJ), .groups = 'drop')
  # Stacked mode totals must exactly reproduce the selected physical-link total.
  if (abs(sum(plot_data$TransportEJ) - sum(flows$Value)) > 1e-8) stop('Transport totals do not reconcile')
  p <- ggplot2::ggplot(plot_data,
    ggplot2::aes(x = .data$ScenarioKey, y = .data$TransportEJ, fill = .data$Transport)) +
    ggplot2::geom_col(width = 0.65, color = 'white', linewidth = 0.2,
                      position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_text(data = totals,
      ggplot2::aes(x = .data$ScenarioKey, y = .data$TransportEJ,
                   label = sprintf('%.2f', .data$TransportEJ)),
      inherit.aes = FALSE, vjust = -0.4, size = 3.6) +
    ggplot2::facet_wrap(ggplot2::vars(.data$Carrier), nrow = 1, drop = FALSE) +
    ggplot2::scale_fill_manual(values = transport_colors, breaks = transport_levels, drop = FALSE) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(title = paste0('World: energy transport, ', primary_2050$metadata$year),
      x = NULL, y = paste0('Gross link throughput in ', primary_2050$metadata$year, ' (EJ/yr)'),
      fill = NULL, caption = paste0(
        'Sum of forward + backward annual flows on all spatial links, including links within each R31 region.\n',
        'NH3: purple = sea trade (R31 links); orange = truck transport (detailed-node links). No map display threshold applied.')) +
    MyTheme +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 14),
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, size = 9),
      axis.text.y = ggplot2::element_text(size = 9),
      axis.title.y = ggplot2::element_text(size = 10),
      strip.text = ggplot2::element_text(size = 11),
      legend.position = 'bottom', legend.text = ggplot2::element_text(size = 10),
      plot.caption = ggplot2::element_text(size = 8, hjust = 0)) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
  attr(p, 'fig2_c_data') <- plot_data
  attr(p, 'fig2_c_link_data') <- flows
  p
}
