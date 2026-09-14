# FIGURE 3A: ELECTRICITY SYSTEM MAP ------------------------------------------

make_fig3_a <- function(data, primary_2050, geodata, scenario,
                        node_threshold = 0.1, flow_threshold = 0.01) {
  assert_panel_data(data); assert_primary_data(primary_2050)
  generation_spec <- tibble::tribble(
    ~Variable, ~Category,
    "Sec_Ene_Ele_Solar", "Solar", "Sec_Ene_Ele_Win", "Wind",
    "Sec_Ene_Ele_Hyp", "Hydro", "Sec_Ene_Ele_Nuc", "Nuclear",
    "Sec_Ene_Ele_Bio", "Biomass", "Sec_Ene_Ele_Oil", "Thermal / other",
    "Sec_Ene_Ele_Coa", "Thermal / other", "Sec_Ene_Ele_Gas", "Thermal / other",
    "Sec_Ene_Ele_Gas_Hyd_Cof", "Thermal / other", "Sec_Ene_Ele_Geo", "Thermal / other",
    "Sec_Ene_Ele_Hyd", "Thermal / other"
  )
  available <- intersect(generation_spec$Variable, unique(data$psm_iamc$Variable))
  generation <- data$psm_iamc %>%
    dplyr::filter(.data$Scenario == scenario, .data$Year == 2050, .data$Variable %in% available) %>%
    fig3_detailed_iamc_rows(geodata) %>%
    dplyr::inner_join(generation_spec, by = "Variable") %>%
    dplyr::group_by(Region = .data$Region, .data$Category) %>%
    dplyr::summarise(Value = sum(.data$Value), .groups = "drop") %>%
    dplyr::left_join(geodata$nodes, by = c("Region" = "Node")) %>%
    dplyr::mutate(Half = "full")
  node_totals <- generation %>% dplyr::group_by(.data$Region) %>%
    dplyr::summarise(Total = sum(.data$Value), .groups = "drop")
  generation <- generation %>% dplyr::inner_join(
    node_totals %>% dplyr::filter(.data$Total >= node_threshold), by = "Region"
  )
  max_total <- max(node_totals$Total, na.rm = TRUE)
  node_polygons <- fig3_sector_polygons(generation, max_total, max_radius = 7.5)
  transmission <- fig3_gross_links(primary_2050, scenario, ".*-AC_.*-AC$", geodata$nodes, "node") %>%
    dplyr::filter(.data$FlowEJ >= flow_threshold)

  p <- fig3_map_base(geodata) +
    ggplot2::geom_curve(
      data = transmission,
      ggplot2::aes(x = .data$lon0, y = .data$lat0, xend = .data$lon1,
                   yend = .data$lat1, linewidth = .data$FlowEJ),
      curvature = 0.08, color = "#4D7298", alpha = 0.45, lineend = "round"
    ) +
    ggplot2::geom_polygon(
      data = node_polygons,
      ggplot2::aes(x = .data$lon, y = .data$lat, group = .data$Piece, fill = .data$Category),
      color = "white", linewidth = 0.12
    ) +
    ggplot2::scale_fill_manual(values = FIG3_GENERATION_COLORS, drop = FALSE) +
    ggplot2::scale_linewidth_continuous(range = c(0.15, 2.8), trans = "sqrt",
      breaks = scales::breaks_pretty(n = 4), name = "Gross transmission (EJ/yr)") +
    ggplot2::labs(fill = "Generation", caption =
      "Node area is proportional to annual generation. Lines show gross annual link throughput.")
  attr(p, "fig3_a_generation") <- generation
  attr(p, "fig3_a_transmission") <- transmission
  p
}
