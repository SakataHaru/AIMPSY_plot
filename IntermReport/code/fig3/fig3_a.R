# FIGURE 3A: ELECTRICITY SYSTEM MAP ------------------------------------------

make_fig3_a <- function(data, primary_2050, geodata, scenario,
                        node_threshold = 0.1, flow_threshold = 0.01) {
  assert_panel_data(data); assert_primary_data(primary_2050)
  generation_spec <- tibble::tribble(
    ~Variable, ~Category,
    "Sec_Ene_Ele_Solar", "Solar", "Sec_Ene_Ele_Win", "Wind",
    "Sec_Ene_Ele_Hyp", "Hydro", "Sec_Ene_Ele_Nuc", "Nuclear",
    "Sec_Ene_Ele_Bio", "Biomass", "Sec_Ene_Ele_Oil", "Other",
    "Sec_Ene_Ele_Coa", "Other", "Sec_Ene_Ele_Gas", "Other",
    "Sec_Ene_Ele_Gas_Hyd_Cof", "Other", "Sec_Ene_Ele_Geo", "Other",
    "Sec_Ene_Ele_Hyd", "Other"
  )
  available <- intersect(generation_spec$Variable, unique(data$psm_iamc$Variable))
  generation <- data$psm_iamc %>%
    dplyr::filter(.data$Scenario == scenario, .data$Year == 2050, .data$Variable %in% available) %>%
    fig3_detailed_iamc_rows(geodata) %>%
    dplyr::inner_join(generation_spec, by = "Variable") %>%
    dplyr::group_by(Region = .data$Region, .data$Category) %>%
    dplyr::summarise(Value = sum(.data$Value), .groups = "drop") %>%
    dplyr::left_join(geodata$nodes, by = c("Region" = "Node")) %>%
    dplyr::mutate(
      Half = "full",
      Order = match(.data$Category, c("Solar", "Wind", "Biomass", "Nuclear", "Hydro", "Other"))
    )
  node_totals <- generation %>% dplyr::group_by(.data$Region) %>%
    dplyr::summarise(Total = sum(.data$Value), .groups = "drop")
  generation <- generation %>% dplyr::inner_join(
    node_totals %>% dplyr::filter(.data$Total >= node_threshold), by = "Region"
  )
  max_total <- max(node_totals$Total, na.rm = TRUE)
  max_radius_mm <- 4.6
  size_legend <- fig3_size_legend(
    max_value = max_total, max_radius = max_radius_mm,
    anchor_lon = -165, anchor_lat = -45,
    direction = "vertical", title = "Power generation (EJ/yr)"
  )
  color_legend <- fig3_color_legend(
    categories = names(FIG3_GENERATION_COLORS),
    anchor_lon = -125, anchor_lat = -20,
    title = "", spacing_lat = 7.2
  )
  transmission <- fig3_gross_links(primary_2050, scenario, ".*-AC_.*-AC$", geodata$nodes, "node") %>%
    dplyr::filter(.data$FlowEJ >= flow_threshold)

  p <- fig3_map_base(geodata) +
    ggplot2::geom_curve(
      data = transmission,
      ggplot2::aes(x = .data$lon0, y = .data$lat0, xend = .data$lon1,
                   yend = .data$lat1, linewidth = .data$FlowEJ),
      curvature = 0.08, color = "#333333", alpha = 0.48, lineend = "round"
    ) +
    fig3_pie_layer(
      generation, max_value = max_total, max_radius_mm = max_radius_mm
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
    ggplot2::scale_fill_manual(
      values = FIG3_GENERATION_COLORS,
      breaks = names(FIG3_GENERATION_COLORS), drop = FALSE,
      guide = "none"
    ) +
    ggplot2::scale_size_area(
      limits = c(0, max_total), max_size = 2 * max_radius_mm,
      guide = "none"
    ) +
    ggplot2::scale_linewidth_continuous(range = c(0.15, 2.8), trans = "sqrt",
      breaks = scales::breaks_pretty(n = 4), name = "Transmission volume (EJ/yr)") +
    ggplot2::labs(fill = "Generation") +
    ggplot2::guides(
      fill = "none",
      linewidth = ggplot2::guide_legend(nrow = 1)
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
  attr(p, "fig3_a_generation") <- generation
  attr(p, "fig3_a_transmission") <- transmission
  attr(p, "fig3_a_size_legend_values") <- size_legend$values
  p
}
