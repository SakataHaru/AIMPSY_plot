# FIGURE 3B: HYDROGEN-BASED CARRIER SYSTEM MAP -------------------------------

make_fig3_b <- function(data, primary_2050, geodata, scenario,
                        node_threshold = 0.1, h2_threshold = 0.01,
                        nh3_threshold = 0.01, syf_threshold = 0.01) {
  assert_panel_data(data); assert_primary_data(primary_2050)
  available <- unique(data$psm_iamc$Variable)
  iamc_h2 <- data$psm_iamc %>%
    dplyr::filter(
      .data$Scenario == scenario, .data$Year == 2050,
      .data$Variable %in% intersect(c("Sec_Ene_Hyd_Ele", "Sec_Ene_Hyd"), available)
    ) %>%
    fig3_detailed_iamc_rows(geodata) %>%
    dplyr::group_by(Region = .data$Region, .data$Variable) %>%
    dplyr::summarise(Value = sum(.data$Value), .groups = "drop") %>%
    tidyr::complete(
      Region,
      Variable = c("Sec_Ene_Hyd_Ele", "Sec_Ene_Hyd"),
      fill = list(Value = 0)
    ) %>%
    tidyr::pivot_wider(names_from = "Variable", values_from = "Value", values_fill = 0)

  conversion_rows <- function(dataset, direction) {
    dataset %>% dplyr::filter(.data$Scenario == scenario, .data$Value > 1e-8) %>%
      split_primary_links() %>%
      dplyr::mutate(
        FromNode = if (direction == "forward") .data$Node0 else .data$Node1,
        ToNode = if (direction == "forward") .data$Node1 else .data$Node0,
        FromCarrier = sub("^.*-", "", .data$FromNode),
        ToCarrier = sub("^.*-", "", .data$ToNode),
        Region = fig3_link_node(.data$FromNode),
        Category = dplyr::case_when(
          .data$FromCarrier == "H2" & .data$ToCarrier == "NH3" ~ "NH3 synthesis",
          .data$FromCarrier == "H2" & .data$ToCarrier %in% c("SYF", "SYM") ~ "Synfuel synthesis",
          .data$FromCarrier == "NH3" & .data$ToCarrier == "H2" ~ "NH3 cracking",
          TRUE ~ NA_character_
        ),
        Efficiency = dplyr::case_when(
          .data$Category == "NH3 cracking" ~ FIG2_CONVERSION_EFFICIENCY[["nh3_cracking"]],
          TRUE ~ 1
        ),
        Half = as.character(ifelse(.data$Category == "NH3 cracking", "upper", "lower")),
        Value = .data$Value * .data$Efficiency
      ) %>% dplyr::filter(!is.na(.data$Category), !is.na(.data$Region)) %>%
      dplyr::select("Region", "Category", "Half", "Value")
  }
  conversion_nodes <- dplyr::bind_rows(
    conversion_rows(primary_2050$psm_vff_l, "forward"),
    conversion_rows(primary_2050$psm_vfb_l, "backward")
  ) %>% dplyr::group_by(.data$Region, .data$Category, .data$Half) %>%
    dplyr::summarise(Value = sum(.data$Value), .groups = "drop")

  # Sec_Ene_Hyd is the existing PSM IAMC series used as hydrogen demand in
  # ALLPLOT_TRADING.R. Known synthesis inputs are removed; the residual keeps
  # currently unresolved hydrogen power use and other uses visible rather than
  # silently dropping them.
  known_h2_use <- conversion_nodes %>%
    dplyr::filter(.data$Half == "lower") %>%
    dplyr::group_by(.data$Region) %>%
    dplyr::summarise(KnownUse = sum(.data$Value), .groups = "drop")
  iamc_nodes <- iamc_h2 %>%
    dplyr::left_join(known_h2_use, by = "Region") %>%
    dplyr::mutate(
      KnownUse = tidyr::replace_na(.data$KnownUse, 0),
      `Direct / other H2 use` = pmax(.data$Sec_Ene_Hyd - .data$KnownUse, 0)
    ) %>%
    dplyr::select("Region", "Sec_Ene_Hyd_Ele", "Direct / other H2 use") %>%
    dplyr::rename(Electrolysis = "Sec_Ene_Hyd_Ele") %>%
    tidyr::pivot_longer(-"Region", names_to = "Category", values_to = "Value") %>%
    dplyr::mutate(Half = ifelse(.data$Category == "Electrolysis", "upper", "lower"))

  node_values <- dplyr::bind_rows(iamc_nodes, conversion_nodes) %>%
    dplyr::group_by(.data$Region, .data$Category, .data$Half) %>%
    dplyr::summarise(Value = sum(.data$Value), .groups = "drop") %>%
    dplyr::semi_join(geodata$nodes, by = c("Region" = "Node")) %>%
    dplyr::left_join(geodata$nodes, by = c("Region" = "Node"))
  node_totals <- node_values %>% dplyr::group_by(.data$Region, .data$Half) %>%
    dplyr::summarise(Total = sum(.data$Value), .groups = "drop")
  max_total <- max(node_totals$Total, na.rm = TRUE)
  active_regions <- node_totals %>% dplyr::group_by(.data$Region) %>%
    dplyr::summarise(Total = max(.data$Total), .groups = "drop") %>%
    dplyr::filter(.data$Total >= node_threshold)
  node_values <- node_values %>% dplyr::inner_join(active_regions, by = "Region")
  node_polygons <- fig3_sector_polygons(node_values, max_total, max_radius = 7.5)

  h2_links <- fig3_gross_links(primary_2050, scenario, ".*-(H2|H2TRD)_.*-(H2|H2TRD)$",
    geodata$nodes, "node") %>% dplyr::filter(.data$FlowEJ >= h2_threshold) %>%
    dplyr::mutate(Carrier = "Hydrogen")
  nh3_links <- fig3_gross_links(primary_2050, scenario,
    "^R31-[A-Z0-9]+-(NH3|NH3TRD)_R31-[A-Z0-9]+-(NH3|NH3TRD)$",
    geodata$regions, "region") %>% dplyr::filter(.data$FlowEJ >= nh3_threshold) %>%
    dplyr::mutate(Carrier = "Ammonia")
  syf_links <- fig3_gross_links(primary_2050, scenario,
    "^R31-[A-Z0-9]+-(SYF|SYFTRD|SYM|SYMTRD)_R31-[A-Z0-9]+-(SYF|SYFTRD|SYM|SYMTRD)$",
    geodata$regions, "region") %>% dplyr::filter(.data$FlowEJ >= syf_threshold) %>%
    dplyr::mutate(Carrier = "Synfuel")
  all_links <- dplyr::bind_rows(h2_links, nh3_links, syf_links)

  p <- fig3_map_base(geodata) +
    ggplot2::geom_curve(
      data = all_links,
      ggplot2::aes(x = .data$lon0, y = .data$lat0, xend = .data$lon1,
                   yend = .data$lat1, linewidth = .data$FlowEJ,
                   color = .data$Carrier, linetype = .data$Carrier),
      curvature = 0.12, alpha = 0.58, lineend = "round"
    ) +
    ggplot2::geom_polygon(
      data = node_polygons,
      ggplot2::aes(x = .data$lon, y = .data$lat, group = .data$Piece, fill = .data$Category),
      color = "white", linewidth = 0.12
    ) +
    ggplot2::scale_color_manual(values = CARRIER_COLORS[c("Hydrogen", "Ammonia", "Synfuel")]) +
    ggplot2::scale_linetype_manual(values = c(Hydrogen = "solid", Ammonia = "longdash", Synfuel = "dotted")) +
    ggplot2::scale_fill_manual(values = FIG3_H2_NODE_COLORS, drop = FALSE) +
    ggplot2::scale_linewidth_continuous(range = c(0.15, 3), trans = "sqrt",
      breaks = scales::breaks_pretty(n = 4), name = "Gross transport (EJ/yr)") +
    ggplot2::labs(color = "Carrier", linetype = "Carrier", fill = "H2 supply / use",
      caption = paste("Upper semicircles show H2 supply; lower semicircles show H2 use.",
        "H2 is a detailed network; NH3/Synfuel lines connect coarse R31 representatives."))
  attr(p, "fig3_b_nodes") <- node_values
  attr(p, "fig3_b_transport") <- all_links
  p
}
