suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(gdxrrw)
  library(sf)
  library(tibble)
})

source("data_import.R")
source("plot_common.R")
source("fig2/fig2_c.R")
source("fig3/fig3_common.R")

scenario_map <- c(ELE = "500C-ELE", H2 = "500C-H2", SYF = "500C-SYF")
psm_primary_dir <- "../data/AIM-PSY/global_softlink_IntermReport/Global/2609121358/gams_output/gdx_primary"
primary <- load_psm_primary(
  psm_primary_dir = psm_primary_dir,
  scenario_map = scenario_map,
  year = 2050,
  time_resol = "6HOUR",
  gams_system_dir = "C:/GAMS/win64/26.1",
  region_map_path = "../data/M_R272.csv"
)
geodata <- load_fig3_geodata("../data")

h2_pattern <- ".*-(H2|H2TRD)_.*-(H2|H2TRD)$"
nh3_pattern <- "^R31-[A-Z0-9]+-(NH3|NH3TRD)_R31-[A-Z0-9]+-(NH3|NH3TRD)$"
syf_pattern <- "^R31-[A-Z0-9]+-(SYF|SYFTRD|SYM|SYMTRD)_R31-[A-Z0-9]+-(SYF|SYFTRD|SYM|SYMTRD)$"

fig2_plot <- make_fig2_c(primary)
fig2_flows <- attr(fig2_plot, "fig2_c_link_data")

audit_h2 <- function(scenario) {
  raw <- bind_rows(
    primary$psm_vff_l %>% mutate(Symbol = "vff_l"),
    primary$psm_vfb_l %>% mutate(Symbol = "vfb_l")
  ) %>%
    filter(.data$Scenario == scenario, grepl(h2_pattern, .data$Link), .data$Value > 0) %>%
    split_primary_links() %>%
    mutate(
      Location0 = sub("-(H2|H2TRD)$", "", .data$Node0),
      Location1 = sub("-(H2|H2TRD)$", "", .data$Node1),
      SameLocation = .data$Location0 == .data$Location1,
      PhysicalLink = if_else(
        .data$Node0 <= .data$Node1,
        paste(.data$Node0, .data$Node1, sep = "__"),
        paste(.data$Node1, .data$Node0, sep = "__")
      )
    )

  physical <- raw %>%
    group_by(.data$PhysicalLink) %>%
    summarise(
      Node0 = first(.data$Node0), Node1 = first(.data$Node1),
      SameLocation = all(.data$SameLocation), FlowEJ = sum(.data$Value),
      .groups = "drop"
    ) %>%
    mutate(
      Point0 = sub("-(AC|H2|H2TRD)$", "", .data$Node0),
      Point1 = sub("-(AC|H2|H2TRD)$", "", .data$Node1),
      lon0 = geodata$nodes$lon[match(.data$Point0, geodata$nodes$Node)],
      lat0 = geodata$nodes$lat[match(.data$Point0, geodata$nodes$Node)],
      lon1 = geodata$nodes$lon[match(.data$Point1, geodata$nodes$Node)],
      lat1 = geodata$nodes$lat[match(.data$Point1, geodata$nodes$Node)],
      Mapped = is.finite(.data$lon0) & is.finite(.data$lat0) &
        is.finite(.data$lon1) & is.finite(.data$lat1),
      Dateline = .data$Mapped & abs(.data$lon1 - .data$lon0) > 180,
      ZeroLength = .data$Mapped & .data$lon0 == .data$lon1 & .data$lat0 == .data$lat1,
      AboveThreshold = .data$FlowEJ >= 0.01,
      EligibleFig2 = !.data$SameLocation,
      VisibleFig3 = .data$Mapped & !.data$Dateline & !.data$ZeroLength & .data$AboveThreshold
    )

  stages <- tibble(
    Scenario = scenario,
    Stage = c(
      "Fig2 inter-location", "Coordinate mapped", "After dateline filter",
      "Above 0.01 threshold", "Actually drawable"
    ),
    Links = c(
      sum(physical$EligibleFig2),
      sum(physical$EligibleFig2 & physical$Mapped),
      sum(physical$EligibleFig2 & physical$Mapped & !physical$Dateline),
      sum(physical$EligibleFig2 & physical$Mapped & !physical$Dateline & physical$AboveThreshold),
      sum(physical$EligibleFig2 & physical$VisibleFig3)
    ),
    FlowEJ = c(
      sum(physical$FlowEJ[physical$EligibleFig2]),
      sum(physical$FlowEJ[physical$EligibleFig2 & physical$Mapped]),
      sum(physical$FlowEJ[physical$EligibleFig2 & physical$Mapped & !physical$Dateline]),
      sum(physical$FlowEJ[physical$EligibleFig2 & physical$Mapped & !physical$Dateline & physical$AboveThreshold]),
      sum(physical$FlowEJ[physical$EligibleFig2 & physical$VisibleFig3])
    )
  )

  coordinate_pairs <- physical %>%
    filter(.data$EligibleFig2, .data$Mapped, !.data$Dateline, .data$AboveThreshold) %>%
    mutate(
      CoordinatePair = if_else(
        .data$Point0 <= .data$Point1,
        paste(.data$Point0, .data$Point1, sep = "__"),
        paste(.data$Point1, .data$Point0, sep = "__")
      )
    ) %>%
    group_by(.data$CoordinatePair) %>%
    summarise(PhysicalLinks = n(), FlowEJ = sum(.data$FlowEJ), .groups = "drop")

  missing <- physical %>%
    filter(.data$EligibleFig2, !.data$VisibleFig3) %>%
    mutate(
      Reason = case_when(
        !.data$Mapped ~ "unmapped endpoint",
        .data$Dateline ~ "dateline",
        .data$ZeroLength ~ "zero-length coordinates",
        !.data$AboveThreshold ~ "below threshold",
        TRUE ~ "other"
      )
    ) %>%
    arrange(desc(.data$FlowEJ)) %>%
    select("PhysicalLink", "Point0", "Point1", "FlowEJ", "Reason")

  list(raw = raw, physical = physical, stages = stages,
       coordinate_pairs = coordinate_pairs, missing = missing)
}

audits <- lapply(unname(scenario_map), audit_h2)
names(audits) <- names(scenario_map)

cat("\n=== FIG2 TOTALS ===\n")
print(
  fig2_flows %>%
    group_by(.data$Scenario, .data$Carrier) %>%
    summarise(Links = n(), FlowEJ = sum(.data$Value), .groups = "drop") %>%
    arrange(.data$Scenario, .data$Carrier),
  n = Inf
)

cat("\n=== H2 RETENTION STAGES ===\n")
print(bind_rows(lapply(audits, `[[`, "stages")), n = Inf)

cat("\n=== H2 EXCLUSION REASONS ===\n")
for (key in names(audits)) {
  cat("\n-- ", key, " --\n", sep = "")
  print(
    audits[[key]]$missing %>%
      group_by(.data$Reason) %>%
      summarise(Links = n(), FlowEJ = sum(.data$FlowEJ), .groups = "drop") %>%
      arrange(desc(.data$FlowEJ)),
    n = Inf
  )
  cat("Top excluded links:\n")
  print(head(audits[[key]]$missing, 12), n = 12)
}

cat("\n=== H2 LINK LABEL TYPES (FIG2-ELIGIBLE) ===\n")
for (key in names(audits)) {
  cat("\n-- ", key, " --\n", sep = "")
  print(
    audits[[key]]$raw %>%
      filter(!.data$SameLocation) %>%
      mutate(
        Type0 = sub("^.*-", "", .data$Node0),
        Type1 = sub("^.*-", "", .data$Node1),
        Type = paste(.data$Type0, .data$Type1, sep = " -> ")
      ) %>%
      group_by(.data$Type) %>%
      summarise(Rows = n(), FlowEJ = sum(.data$Value), .groups = "drop") %>%
      arrange(desc(.data$FlowEJ)),
    n = Inf
  )
}

cat("\n=== H2 COORDINATE-PAIR OVERLAP ===\n")
for (key in names(audits)) {
  pairs <- audits[[key]]$coordinate_pairs
  cat(key, ": drawable physical links=", sum(pairs$PhysicalLinks),
      ", unique coordinate pairs=", nrow(pairs),
      ", pairs with overlap=", sum(pairs$PhysicalLinks > 1), "\n", sep = "")
  print(head(arrange(filter(pairs, .data$PhysicalLinks > 1), desc(.data$FlowEJ)), 10), n = 10)
}

cat("\n=== FIG3 PLOTTED-LINK SCALE AUDIT ===\n")
for (key in names(scenario_map)) {
  scenario <- unname(scenario_map[[key]])
  h2 <- fig3_gross_links(primary, scenario, h2_pattern, geodata$nodes, "node") %>%
    filter(.data$FlowEJ >= 0.01) %>% transmute(Carrier = "H2", FlowEJ)
  nh3 <- fig3_directional_trade_links(primary, scenario, nh3_pattern, geodata$regions) %>%
    filter(.data$FlowEJ >= 0.01) %>% transmute(Carrier = "NH3", FlowEJ)
  syf <- fig3_directional_trade_links(primary, scenario, syf_pattern, geodata$regions) %>%
    filter(.data$FlowEJ >= 0.01) %>% transmute(Carrier = "Synfuel", FlowEJ)
  plotted <- bind_rows(h2, nh3, syf)
  transformed_range <- range(sqrt(plotted$FlowEJ))
  plotted <- plotted %>% mutate(
    Width = if (diff(transformed_range) == 0) 1.575 else
      0.15 + (sqrt(.data$FlowEJ) - transformed_range[1]) /
        diff(transformed_range) * (3 - 0.15)
  )
  summary <- plotted %>%
    group_by(.data$Carrier) %>%
    summarise(
      Links = n(), TotalEJ = sum(.data$FlowEJ), MinEJ = min(.data$FlowEJ),
      MedianEJ = median(.data$FlowEJ), MaxEJ = max(.data$FlowEJ),
      MinWidth = min(.data$Width), MedianWidth = median(.data$Width),
      MaxWidth = max(.data$Width), .groups = "drop"
    )
  cat("\n-- ", key, " / ", scenario, " --\n", sep = "")
  print(summary, n = Inf)
}
