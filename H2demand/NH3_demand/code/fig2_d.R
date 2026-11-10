# FIGURE 2D PANEL --------------------------------------------------------------

make_fig2_d <- function(data, primary) {
  assert_panel_data(data)
  assert_primary_data(primary)
  scenario_lookup <- scenario_crosswalk(data)
  flow_tolerance <- 1e-5

  carrier_from_node <- function(node) {
    dplyr::case_when(
      grepl("-AC$", node) ~ "Electricity",
      grepl("-H2$", node) ~ "Hydrogen",
      grepl("-NH3$", node) ~ "Ammonia",
      grepl("-(SYF|SYM)$", node) ~ "Synfuel",
      TRUE ~ NA_character_
    )
  }
  directed_conversion <- function(dataset, direction) {
    dataset %>%
      dplyr::filter(.data$Value > flow_tolerance) %>%
      split_primary_links() %>%
      dplyr::mutate(
        FromNode = if (direction == "forward") .data$Node0 else .data$Node1,
        ToNode = if (direction == "forward") .data$Node1 else .data$Node0,
        FromCarrier = carrier_from_node(.data$FromNode),
        ToCarrier = carrier_from_node(.data$ToNode)
      ) %>%
      dplyr::filter(
        !is.na(.data$FromCarrier), !is.na(.data$ToCarrier),
        .data$FromCarrier != .data$ToCarrier
      ) %>%
      dplyr::select("Scenario", "FromCarrier", "ToCarrier", "ToNode", "Value")
  }
  activity <- dplyr::bind_rows(
    directed_conversion(primary$psm_vff_l, "forward"),
    directed_conversion(primary$psm_vfb_l, "backward")
  ) %>%
    dplyr::mutate(
      Conversion = dplyr::case_when(
        .data$FromCarrier == "Electricity" & .data$ToCarrier == "Hydrogen" ~ "Electrolysis",
        .data$FromCarrier == "Hydrogen" & .data$ToCarrier == "Ammonia" ~ "H2 to NH3",
        .data$FromCarrier == "Hydrogen" & .data$ToCarrier == "Synfuel" ~ "H2 to Synfuel",
        .data$FromCarrier == "Ammonia" & .data$ToCarrier == "Hydrogen" ~ "NH3 to H2",
        TRUE ~ NA_character_
      ),
      Efficiency = dplyr::case_when(
        .data$Conversion == "Electrolysis" ~ FIG2_CONVERSION_EFFICIENCY[["electrolysis"]],
        .data$Conversion == "H2 to NH3" ~ FIG2_CONVERSION_EFFICIENCY[["haber_bosch"]],
        .data$Conversion == "H2 to Synfuel" & grepl("-SYF$", .data$ToNode) ~
          FIG2_CONVERSION_EFFICIENCY[["fischer_tropsch"]],
        .data$Conversion == "H2 to Synfuel" & grepl("-SYM$", .data$ToNode) ~
          FIG2_CONVERSION_EFFICIENCY[["methanation"]],
        .data$Conversion == "NH3 to H2" ~ FIG2_CONVERSION_EFFICIENCY[["nh3_cracking"]],
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::filter(!is.na(.data$Conversion), !is.na(.data$Efficiency)) %>%
    dplyr::group_by(.data$Scenario, .data$Conversion) %>%
    dplyr::summarise(
      InputEJ = sum(.data$Value),
      OutputEJ = sum(.data$Value * .data$Efficiency),
      LossEJ = sum(.data$Value * (1 - .data$Efficiency)),
      .groups = "drop"
    )

  get_activity <- function(scenario, conversion, field) {
    value <- activity[[field]][activity$Scenario == scenario & activity$Conversion == conversion]
    if (length(value) == 0L) 0 else sum(value)
  }
  get_aimt <- function(scenario, variables) {
    data$aimt_iamc %>%
      dplyr::filter(
        .data$Scenario == scenario, .data$Region == "World",
        .data$Year == primary$metadata$year,
        .data$Variable %in% variables
      ) %>%
      dplyr::summarise(Value = sum(.data$Value), .groups = "drop") %>%
      dplyr::pull("Value") %>%
      {if (length(.) == 0L) 0 else .}
  }
  get_psm <- function(scenario, variable) {
    data$psm_iamc %>%
      dplyr::filter(
        .data$Scenario == scenario, .data$Region %in% PSM_WORLD_REGIONS,
        .data$Year == primary$metadata$year, .data$Variable == variable
      ) %>%
      dplyr::summarise(Value = sum(.data$Value), .groups = "drop") %>%
      dplyr::pull("Value") %>%
      {if (length(.) == 0L) 0 else .}
  }

  assert_variables_present(
    data$aimt_iamc,
    c("Fin_Ene_Hyd", "Fin_Ene_Hyd_Amm", "Fin_Ene_Liq_Hyd_syn", "Fin_Ene_Gas_Hyd_syn"),
    "Fig. 2-d AIM-Technology input"
  )

  audit <- dplyr::bind_rows(lapply(unname(primary$metadata$scenario_map), function(scenario) {
    h2_total <- get_aimt(scenario, "Fin_Ene_Hyd")
    nh3_demand <- get_aimt(scenario, "Fin_Ene_Hyd_Amm")
    tibble::tibble(
      Scenario = scenario,
      H2Production = get_activity(scenario, "Electrolysis", "OutputEJ"),
      H2ToNH3Input = get_activity(scenario, "H2 to NH3", "InputEJ"),
      H2ToNH3Output = get_activity(scenario, "H2 to NH3", "OutputEJ"),
      H2ToNH3Loss = get_activity(scenario, "H2 to NH3", "LossEJ"),
      H2ToSynInput = get_activity(scenario, "H2 to Synfuel", "InputEJ"),
      H2ToSynOutput = get_activity(scenario, "H2 to Synfuel", "OutputEJ"),
      H2ToSynLoss = get_activity(scenario, "H2 to Synfuel", "LossEJ"),
      NH3ToH2Input = get_activity(scenario, "NH3 to H2", "InputEJ"),
      NH3ToH2Output = get_activity(scenario, "NH3 to H2", "OutputEJ"),
      NH3ToH2Loss = get_activity(scenario, "NH3 to H2", "LossEJ"),
      H2Demand = max(h2_total - nh3_demand, 0),
      NH3Demand = nh3_demand,
      SynfuelDemand = get_aimt(scenario, c("Fin_Ene_Liq_Hyd_syn", "Fin_Ene_Gas_Hyd_syn")),
      IAMC_H2Production = get_psm(scenario, "Sec_Ene_Hyd_Ele"),
      IAMC_NH3Production = get_psm(scenario, "Sec_Ene_Hyd_Amm"),
      IAMC_SynfuelProduction = get_psm(scenario, "Sec_Ene_Hyd_Syn")
    )
  })) %>%
    dplyr::mutate(
      H2AvailableAfterConversion = .data$H2Production - .data$H2ToNH3Input -
        .data$H2ToSynInput + .data$NH3ToH2Output,
      H2Other = .data$H2AvailableAfterConversion - .data$H2Demand,
      NH3Other = .data$H2ToNH3Output - .data$NH3ToH2Input - .data$NH3Demand,
      SynfuelOther = .data$H2ToSynOutput - .data$SynfuelDemand,
      H2ProductionDifference = .data$H2Production - .data$IAMC_H2Production,
      NH3ProductionDifference = .data$H2ToNH3Output - .data$IAMC_NH3Production,
      SynfuelProductionDifference = .data$H2ToSynOutput - .data$IAMC_SynfuelProduction,
      IAMCValidationMismatch =
        abs(.data$H2ProductionDifference) > pmax(0.01 * abs(.data$IAMC_H2Production), flow_tolerance) |
        abs(.data$NH3ProductionDifference) > pmax(0.01 * abs(.data$IAMC_NH3Production), flow_tolerance) |
        abs(.data$SynfuelProductionDifference) > pmax(0.01 * abs(.data$IAMC_SynfuelProduction), flow_tolerance)
    ) %>%
    dplyr::inner_join(scenario_lookup, by = "Scenario")
  if (any(audit$IAMCValidationMismatch)) {
    warning(
      "Fig. 2-d primary conversion output differs from the corresponding PSM IAMC value by more than 1%; ",
      "see attr(plot, 'fig2_d_balance_audit')."
    )
  }

  add_link <- function(tbl, source, target, value, category, source_x, target_x) {
    if (!is.finite(value) || value <= flow_tolerance) return(tbl)
    dplyr::bind_rows(tbl, tibble::tibble(
      Source = source, Target = target, Value = value, Category = category,
      SourceX = source_x, TargetX = target_x
    ))
  }
  make_links <- function(row) {
    links <- tibble::tibble()
    # Direct hydrogen demand is supplied first by recovered H2, then production.
    crack_to_h2 <- min(row$NH3ToH2Output, row$H2Demand)
    production_after_conversion <- row$H2Production - row$H2ToNH3Input - row$H2ToSynInput
    production_to_h2 <- min(max(production_after_conversion, 0), row$H2Demand - crack_to_h2)
    h2_deficit <- max(row$H2Demand - crack_to_h2 - production_to_h2, 0)
    h2_surplus <- max(production_after_conversion - production_to_h2, 0)
    crack_surplus <- max(row$NH3ToH2Output - crack_to_h2, 0)

    links <- add_link(links, "H2 production", "H2 → NH3", row$H2ToNH3Input, "Hydrogen", 1, 2)
    links <- add_link(links, "H2 production", "H2 → Synfuel", row$H2ToSynInput, "Hydrogen", 1, 2)
    links <- add_link(links, "H2 production", "Direct H2 use", production_to_h2, "Hydrogen", 1, 4)
    links <- add_link(links, "H2 production", "Other", h2_surplus, "Other", 1, 4)
    links <- add_link(links, "Other supply", "Direct H2 use", h2_deficit, "Other", 1, 4)

    nh3_for_cracking <- min(row$H2ToNH3Output, row$NH3ToH2Input)
    nh3_after_cracking <- max(row$H2ToNH3Output - nh3_for_cracking, 0)
    nh3_for_demand <- min(nh3_after_cracking, row$NH3Demand)
    nh3_deficit <- max(row$NH3Demand - nh3_for_demand, 0)
    nh3_other <- max(nh3_after_cracking - nh3_for_demand, 0)
    crack_deficit <- max(row$NH3ToH2Input - nh3_for_cracking, 0)
    links <- add_link(links, "H2 → NH3", "NH3 → H2", nh3_for_cracking, "Ammonia", 2, 3)
    links <- add_link(links, "H2 → NH3", "NH3 final use", nh3_for_demand, "Ammonia", 2, 4)
    links <- add_link(links, "H2 → NH3", "Other", nh3_other, "Other", 2, 4)
    links <- add_link(links, "H2 → NH3", "Loss", row$H2ToNH3Loss, "Loss", 2, 4)
    links <- add_link(links, "Other supply", "NH3 final use", nh3_deficit, "Other", 1, 4)
    links <- add_link(links, "Other supply", "NH3 → H2", crack_deficit, "Other", 1, 3)

    syn_for_demand <- min(row$H2ToSynOutput, row$SynfuelDemand)
    links <- add_link(links, "H2 → Synfuel", "Synfuel final use", syn_for_demand, "Synfuel", 2, 4)
    links <- add_link(links, "H2 → Synfuel", "Other", max(row$H2ToSynOutput - syn_for_demand, 0), "Other", 2, 4)
    links <- add_link(links, "H2 → Synfuel", "Loss", row$H2ToSynLoss, "Loss", 2, 4)
    links <- add_link(links, "Other supply", "Synfuel final use", max(row$SynfuelDemand - syn_for_demand, 0), "Other", 1, 4)

    links <- add_link(links, "NH3 → H2", "Direct H2 use", crack_to_h2, "Cracked H2", 3, 4)
    links <- add_link(links, "NH3 → H2", "Other", crack_surplus, "Other", 3, 4)
    links <- add_link(links, "NH3 → H2", "Loss", row$NH3ToH2Loss, "Loss", 3, 4)
    links
  }
  links <- dplyr::bind_rows(lapply(seq_len(nrow(audit)), function(i) {
    make_links(audit[i, ]) %>%
      dplyr::mutate(ScenarioKey = audit$ScenarioKey[i])
  })) %>%
    dplyr::group_by(
      .data$ScenarioKey, .data$Source, .data$Target, .data$Category,
      .data$SourceX, .data$TargetX
    ) %>%
    dplyr::summarise(Value = sum(.data$Value), .groups = "drop")

  node_order <- c(
    "H2 production" = 1, "Other supply" = 2,
    "H2 → NH3" = 1, "H2 → Synfuel" = 2,
    "NH3 → H2" = 1,
    "Direct H2 use" = 1, "NH3 final use" = 2, "Synfuel final use" = 3,
    "Other" = 4, "Loss" = 5
  )
  node_labels <- c(
    "H2 production" = "H2", "Other supply" = "Other",
    "H2 → NH3" = "NH3", "H2 → Synfuel" = "Synfuel",
    "NH3 → H2" = "H2", "Direct H2 use" = "H2",
    "NH3 final use" = "NH3", "Synfuel final use" = "Synfuel",
    "Other" = "Other", "Loss" = "Loss"
  )
  nodes <- dplyr::bind_rows(
    links %>% dplyr::transmute(ScenarioKey, Node = .data$Source, X = .data$SourceX),
    links %>% dplyr::transmute(ScenarioKey, Node = .data$Target, X = .data$TargetX)
  ) %>%
    dplyr::distinct() %>%
    dplyr::left_join(
      links %>% dplyr::group_by(.data$ScenarioKey, Node = .data$Source) %>%
        dplyr::summarise(Out = sum(.data$Value), .groups = "drop"),
      by = c("ScenarioKey", "Node")
    ) %>%
    dplyr::left_join(
      links %>% dplyr::group_by(.data$ScenarioKey, Node = .data$Target) %>%
        dplyr::summarise(In = sum(.data$Value), .groups = "drop"),
      by = c("ScenarioKey", "Node")
    ) %>%
    dplyr::mutate(
      Out = tidyr::replace_na(.data$Out, 0), In = tidyr::replace_na(.data$In, 0),
      NodeValue = pmax(.data$Out, .data$In), Order = unname(node_order[.data$Node]),
      NodeLabel = unname(node_labels[.data$Node]),
      LabelHjust = dplyr::case_when(.data$X == 1 ~ 0, .data$X == 4 ~ 1, TRUE ~ 0.5),
      DrawNode = TRUE,
      NodeCategory = dplyr::case_when(
        # A conversion node takes the output carrier's color at the conversion column.
        .data$Node == "H2 → NH3" ~ "Ammonia",
        .data$Node == "H2 → Synfuel" ~ "Synfuel",
        .data$Node == "NH3 → H2" ~ "Hydrogen",
        grepl("H2|Hydrogen", .data$Node) ~ "Hydrogen",
        grepl("NH3", .data$Node) ~ "Ammonia",
        grepl("Synfuel", .data$Node) ~ "Synfuel",
        .data$Node == "Loss" ~ "Loss",
        TRUE ~ "Other"
      )
    )

  # Reserve the direct-H2 band at both conversion positions. These are layout
  # spacers only: the existing direct-use ribbon remains the visible flow.
  direct_h2_spacers <- tidyr::crossing(
    audit %>% dplyr::select("ScenarioKey", NodeValue = "H2Demand"),
    X = c(2, 3)
  ) %>%
    dplyr::filter(.data$NodeValue > flow_tolerance) %>%
    dplyr::mutate(
      Node = paste0("Direct H2 spacer ", .data$X),
      Out = .data$NodeValue,
      In = .data$NodeValue,
      Order = 0,
      NodeLabel = "",
      LabelHjust = 0.5,
      DrawNode = FALSE,
      NodeCategory = "Hydrogen"
    )

  nodes <- dplyr::bind_rows(nodes, direct_h2_spacers) %>%
    dplyr::group_by(.data$ScenarioKey, .data$X) %>%
    dplyr::arrange(.data$Order, .by_group = TRUE) %>%
    dplyr::mutate(
      # Final-use blocks are contiguous so their displayed heights preserve the balance.
      NodeGap = dplyr::if_else(.data$X == 4, 0, 0.12),
      YMin = cumsum(dplyr::lag(.data$NodeValue, default = 0)) +
        (dplyr::row_number() - 1) * .data$NodeGap,
      YMax = .data$YMin + .data$NodeValue
    ) %>%
    dplyr::ungroup()

  link_layout <- links %>%
    dplyr::left_join(nodes %>% dplyr::select(ScenarioKey, Source = Node, SourceYMin = YMin, SourceOrder = Order),
                     by = c("ScenarioKey", "Source")) %>%
    dplyr::left_join(nodes %>% dplyr::select(ScenarioKey, Target = Node, TargetYMin = YMin, TargetOrder = Order),
                     by = c("ScenarioKey", "Target")) %>%
    dplyr::group_by(.data$ScenarioKey, .data$Source) %>%
    # Put direct H2 use at the bottom of the production node, matching Use.
    dplyr::arrange(
      !(.data$Source == "H2 production" & .data$Target == "Direct H2 use"),
      .data$TargetX, .data$TargetOrder, .by_group = TRUE
    ) %>%
    dplyr::mutate(SourceOffset = cumsum(dplyr::lag(.data$Value, default = 0))) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$ScenarioKey, .data$Target) %>%
    dplyr::arrange(.data$SourceX, .data$SourceOrder, .by_group = TRUE) %>%
    dplyr::mutate(TargetOffset = cumsum(dplyr::lag(.data$Value, default = 0))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      SourceBottom = .data$SourceYMin + .data$SourceOffset,
      SourceTop = .data$SourceBottom + .data$Value,
      TargetBottom = .data$TargetYMin + .data$TargetOffset,
      TargetTop = .data$TargetBottom + .data$Value,
      LinkId = dplyr::row_number()
    )
  ribbon_data <- dplyr::bind_rows(lapply(seq_len(nrow(link_layout)), function(i) {
    z <- link_layout[i, ]
    tt <- seq(0, 1, length.out = 25)
    smooth <- 3 * tt^2 - 2 * tt^3
    tibble::tibble(
      ScenarioKey = z$ScenarioKey,
      LinkId = z$LinkId,
      Category = z$Category,
      x = c(z$SourceX + (z$TargetX - z$SourceX) * tt,
            rev(z$SourceX + (z$TargetX - z$SourceX) * tt)),
      y = c(z$SourceTop + (z$TargetTop - z$SourceTop) * smooth,
            rev(z$SourceBottom + (z$TargetBottom - z$SourceBottom) * smooth))
    )
  }))
  y_max <- max(nodes$YMax, na.rm = TRUE) * 1.04
  node_width <- 0.12
  flow_colors <- c(FIG2_FLOW_COLORS, "Cracked H2" = "#D73027")
  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = ribbon_data %>% dplyr::filter(.data$Category != "Cracked H2"),
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$LinkId, fill = .data$Category),
      alpha = 0.45, color = NA
    ) +
    ggplot2::geom_polygon(
      data = ribbon_data %>% dplyr::filter(.data$Category == "Cracked H2"),
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$LinkId, fill = .data$Category),
      alpha = 0.9, color = "#8B1A1A", linewidth = 0.25
    ) +
    ggplot2::geom_rect(
      data = nodes %>% dplyr::filter(.data$DrawNode),
      ggplot2::aes(
        xmin = .data$X - node_width, xmax = .data$X + node_width,
        ymin = .data$YMin, ymax = .data$YMax, fill = .data$NodeCategory
      ),
      color = "white", linewidth = 0.15
    ) +
    ggplot2::geom_text(
      data = nodes %>% dplyr::filter(.data$NodeValue > flow_tolerance),
      ggplot2::aes(
        x = .data$X, y = (.data$YMin + .data$YMax) / 2,
        label = .data$NodeLabel, hjust = .data$LabelHjust
      ),
      size = 2.3, lineheight = 0.9, check_overlap = TRUE
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data$ScenarioKey), nrow = 1, drop = FALSE) +
    ggplot2::scale_fill_manual(values = flow_colors, drop = FALSE) +
    ggplot2::scale_x_continuous(
      breaks = 1:4, labels = c("Production", "", "", "Use"),
      limits = c(0.75, 4.25), expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(limits = c(0, y_max), expand = c(0, 0)) +
    ggplot2::labs(
      x = NULL, y = paste0("Energy flow in ", primary$metadata$year, " (EJ/yr)"), fill = "Carrier / balance"
    ) +
    MyTheme +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.spacing.x = grid::unit(4, "mm"),
      axis.ticks.x = ggplot2::element_line(colour = "black", linewidth = 0.4),
      plot.title = ggplot2::element_text(size = 9),
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 7.5),
      axis.text.y = ggplot2::element_text(size = 6.5),
      axis.title.y = ggplot2::element_text(size = 7.5),
      legend.position = "none"
    )

  attr(p, "fig2_d_node_table") <- nodes
  attr(p, "fig2_d_link_table") <- links
  attr(p, "fig2_d_balance_audit") <- audit
  p
}
