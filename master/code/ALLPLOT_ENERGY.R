# Integrated regional summary plots ------------------------------------------

library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(ggplot2)
library(gdxrrw)
library(patchwork)

options(device = function(...) grDevices::pdf(file = NULL, ...))

script_args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_args[grepl("^--file=", script_args)])
script_dir <- if (length(script_path) > 0) {
  dirname(normalizePath(script_path[[1]], winslash = "/", mustWork = FALSE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}
project_dir <- if (file.exists(file.path(script_dir, "code", "dataframe.R"))) {
  script_dir
} else {
  normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)
}

source(file.path(project_dir, "code", "dataframe.R"))

# for (gams_sys_dir in c("C:/GAMS/38", "C:/GAMS/win64/26.1")) {
#   if (dir.exists(gams_sys_dir)) {
#     igdx(gams_sys_dir)
#     break
#   }
# }

psm_main_dir <- "//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output/global_softlink_Sc/Global/2606171718/main"
aimt_main_dir <- "//SY_3600_01/atfs02/Individual/hsakata/AIMEnduse/output/softlink_Sc/2606111707/main"
psm_primary_dir <- file.path(dirname(psm_main_dir), "gams_output", "gdx_primary")
r272_map_path <- "//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/AIMPSY-master/data/set/M_R272.csv"

target_scenarios <- c("NPi500", "NPi500-ZF2050")
plot_year <- 2050
time_years <- seq(2020, 2050, by = 5)
time_slice_hours <- 6
gwh_to_ej <- 3.6e-6
base_dir <- file.path(project_dir, "output", "global_260617")

region_list <- c(
  "World",
  "JPN", "CHN", "IND", "IDN", "KOR", "THA", "MYS",
  "VNM", "XSE", "XSA", "XEA", "XCS", "XME", "AUS",
  "XOC", "CAN", "USA", "XE15", "XE10", "XE3", "TUR",
  "XEWI", "XEEI", "XENI", "RUS", "MEX", "BRA", "XLM",
  "ZAF", "XAF", "XNF"
)
regions_per_page <- 4
region_pages <- split(region_list, ceiling(seq_along(region_list) / regions_per_page))

read_iamc_gdx <- function(main_dir, model_name) {
  gdx_path <- file.path(main_dir, "merged_output.gdx")
  if (!file.exists(gdx_path)) {
    stop("Missing GDX file: ", gdx_path)
  }

  rgdx.param(gdx_path, "iamc_gdx") %>%
    transmute(
      Scenario = as.character(Sc),
      Region = str_remove(as.character(Sr), "^R31-"),
      Variable = as.character(Sv),
      Year = as.numeric(as.character(Y5)),
      Value = suppressWarnings(as.numeric(as.character(iamc_gdx))),
      Model = model_name
    ) %>%
    filter(!is.na(Value))
}

add_world_rows <- function(df, regions, replace_existing = FALSE) {
  world <- df %>%
    filter(Region %in% regions) %>%
    group_by(Scenario, Variable, Year, Model) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    mutate(Region = "World")

  if (replace_existing) {
    df <- df %>% filter(Region != "World")
  }

  bind_rows(df, world)
}

aimt_data <- read_iamc_gdx(aimt_main_dir, "AIM-Technology")
psm_data <- read_iamc_gdx(psm_main_dir, "PSM")

region_base <- setdiff(region_list, "World")
aimt_plot_data <- aimt_data %>%
  filter(Scenario %in% target_scenarios)
psm_plot_data <- psm_data %>%
  filter(Scenario %in% target_scenarios) %>%
  add_world_rows(region_base, replace_existing = TRUE)

all_data <- bind_rows(aimt_plot_data, psm_plot_data) %>%
  filter(Region %in% region_list) %>%
  mutate(
    Scenario = factor(Scenario, levels = target_scenarios),
    Region = factor(Region, levels = region_list),
    Model = factor(Model, levels = c("AIM-Technology", "PSM"))
  )

normalize_link_node <- function(node_name) {
  node_name %>%
    str_remove_all(fixed("-AC")) %>%
    str_remove("^R31-")
}

normalize_carrier_node <- function(node_name, carrier_suffix_pattern) {
  node_name %>%
    str_remove(carrier_suffix_pattern) %>%
    str_remove("^R31-")
}

read_r272_region_map <- function(path) {
  if (!file.exists(path)) {
    warning("Missing R272-R31 map file: ", path)
    return(tibble(N_IND = character(), R31 = character()))
  }

  tryCatch(
    read.csv(path, check.names = FALSE, stringsAsFactors = FALSE) %>%
      transmute(
        N_IND = normalize_link_node(as.character(N_IND)),
        R31 = str_remove(as.character(R31), "^R31-")
      ) %>%
      filter(N_IND != "", R31 != "") %>%
      distinct(N_IND, R31),
    error = function(e) {
      warning("Failed to read R272-R31 map file: ", path, " / ", conditionMessage(e))
      tibble(N_IND = character(), R31 = character())
    }
  )
}

r272_region_map <- read_r272_region_map(r272_map_path)

infer_node_region <- function(node_name) {
  region_pattern <- paste(sort(region_base, decreasing = TRUE), collapse = "|")
  region <- str_extract(node_name, paste0("^(", region_pattern, ")"))
  if_else(is.na(region), node_name, region)
}

node_to_region <- function(node_name) {
  fallback <- infer_node_region(node_name)
  if (nrow(r272_region_map) == 0) {
    return(fallback)
  }

  mapped <- r272_region_map$R31[match(node_name, r272_region_map$N_IND)]
  if_else(is.na(mapped) | mapped == "", fallback, mapped)
}

read_link_flow_symbol <- function(scenario_name, symbol, base_direction) {
  gdx_path <- file.path(psm_primary_dir, scenario_name, paste0(plot_year, ".gdx"))
  if (!file.exists(gdx_path)) {
    warning("Missing link GDX file: ", gdx_path)
    return(tibble())
  }

  rgdx.param(gdx_path, symbol) %>%
    mutate(across(where(is.factor), as.character)) %>%
    filter(str_detect(L, ".*-AC_.*-AC$")) %>%
    group_by(L) %>%
    summarise(Value = sum(.data[[symbol]], na.rm = TRUE) * time_slice_hours / 1000, .groups = "drop") %>%
    filter(is.finite(Value), Value != 0) %>%
    separate(L, sep = "_", into = c("N0", "N1"), remove = FALSE) %>%
    mutate(
      N0 = normalize_link_node(N0),
      N1 = normalize_link_node(N1),
      FromNodeBase = if (base_direction == "backward") N1 else N0,
      ToNodeBase = if (base_direction == "backward") N0 else N1,
      FromNode = if_else(Value >= 0, FromNodeBase, ToNodeBase),
      ToNode = if_else(Value >= 0, ToNodeBase, FromNodeBase),
      FromRegion = node_to_region(FromNode),
      ToRegion = node_to_region(ToNode),
      Value = abs(Value),
      Scenario = scenario_name
    ) %>%
    select(Scenario, FromRegion, ToRegion, Value)
}

read_link_flows <- function(scenario_name) {
  flows <- bind_rows(
    read_link_flow_symbol(scenario_name, "vff_l", "forward"),
    read_link_flow_symbol(scenario_name, "vfb_l", "backward")
  )

  if (nrow(flows) == 0) {
    return(tibble(
      Scenario = character(),
      FromRegion = character(),
      ToRegion = character(),
      Value = numeric()
    ))
  }

  flows %>%
    filter(FromRegion %in% region_base, ToRegion %in% region_base) %>%
    group_by(Scenario, FromRegion, ToRegion) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")
}

make_region_link_rows <- function(df, scenario_name, region_name) {
  if (region_name == "World") {
    bind_rows(
      df %>%
        filter(FromRegion == ToRegion) %>%
        transmute(Scenario, Region = "World", FlowType = "intra", Counterpart = "intra", Value),
      df %>%
        filter(FromRegion != ToRegion) %>%
        transmute(Scenario, Region = "World", FlowType = "exp", Counterpart = "interregional", Value),
      df %>%
        filter(FromRegion != ToRegion) %>%
        transmute(Scenario, Region = "World", FlowType = "imp", Counterpart = "interregional", Value)
    )
  } else {
    bind_rows(
      df %>%
        filter(FromRegion == region_name, ToRegion == region_name) %>%
        transmute(Scenario, Region = region_name, FlowType = "intra", Counterpart = "intra", Value),
      df %>%
        filter(FromRegion == region_name, ToRegion != region_name) %>%
        transmute(Scenario, Region = region_name, FlowType = "exp", Counterpart = ToRegion, Value),
      df %>%
        filter(ToRegion == region_name, FromRegion != region_name) %>%
        transmute(Scenario, Region = region_name, FlowType = "imp", Counterpart = FromRegion, Value)
    )
  }
}

link_edge_data <- bind_rows(lapply(target_scenarios, read_link_flows))
link_flow_data <- bind_rows(lapply(target_scenarios, function(scenario_name) {
  scenario_links <- link_edge_data %>% filter(Scenario == scenario_name)
  bind_rows(lapply(region_list, function(region_name) {
    make_region_link_rows(
      df = scenario_links,
      scenario_name = scenario_name,
      region_name = region_name
    )
  }))
})) %>%
  group_by(Scenario, Region, FlowType, Counterpart) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    FlowType = factor(FlowType, levels = c("intra", "exp", "imp")),
    Region = factor(Region, levels = region_list)
  )

link_counterparts <- sort(unique(c("intra", "interregional", region_base)))
link_counterpart_colors <- setNames(
  grDevices::hcl.colors(length(link_counterparts), palette = "Dark 3"),
  link_counterparts
)

fuel_specs <- tribble(
  ~Fuel, ~SupplyVariables, ~DemandVariables, ~LinkPattern, ~NodeSuffixPattern, ~Color,
  "Hydrogen", list("Sec_Ene_Hyd"), list("Fin_Ene_Hyd"), ".*-H2_.*-H2$", "-H2$", "#8ecae6",
  "Ammonia", list("Sec_Ene_Hyd_Amm"), list("Fin_Ene_Hyd_Amm"), "^R31-[A-Z0-9]+-NH3_R31-[A-Z0-9]+-NH3$", "-NH3$", "#ffb703",
  "Synfuel", list("Sec_Ene_Hyd_Syn"), list("Fin_Ene_Liq_Hyd_syn"), ".*-(SYN|SYF|LFS|EFS)_.*-(SYN|SYF|LFS|EFS)$", "-(SYN|SYF|LFS|EFS)$", "#c77dff"
)
fuel_levels <- fuel_specs$Fuel
fuel_colors <- setNames(fuel_specs$Color, fuel_specs$Fuel)
fuel_flow_types <- c("Supply", "Import")
fuel_flow_colors <- c("Supply" = "#9ecae1", "Import" = "#31a354")

get_region_variable_value <- function(df, variables, model_name, scenario_name, region_name) {
  variables <- unlist(variables, use.names = FALSE)

  df %>%
    filter(
      Model == model_name,
      Scenario == scenario_name,
      Region == region_name,
      Year == plot_year,
      str_to_lower(Variable) %in% str_to_lower(variables)
    ) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    pull(Value) %>%
    { if (length(.) == 0 || !is.finite(.)) 0 else . }
}

read_carrier_flow_symbol <- function(scenario_name, symbol, base_direction, fuel_spec) {
  gdx_path <- file.path(psm_primary_dir, scenario_name, paste0(plot_year, ".gdx"))
  if (!file.exists(gdx_path)) {
    warning("Missing carrier flow GDX file: ", gdx_path)
    return(tibble())
  }

  rgdx.param(gdx_path, symbol) %>%
    mutate(across(where(is.factor), as.character)) %>%
    filter(str_detect(L, fuel_spec$LinkPattern)) %>%
    group_by(L) %>%
    summarise(Value = sum(.data[[symbol]], na.rm = TRUE) * time_slice_hours * gwh_to_ej, .groups = "drop") %>%
    filter(is.finite(Value), Value != 0) %>%
    separate(L, sep = "_", into = c("N0", "N1"), remove = FALSE) %>%
    mutate(
      N0 = normalize_carrier_node(N0, fuel_spec$NodeSuffixPattern),
      N1 = normalize_carrier_node(N1, fuel_spec$NodeSuffixPattern),
      FromNodeBase = if (base_direction == "backward") N1 else N0,
      ToNodeBase = if (base_direction == "backward") N0 else N1,
      FromNode = if_else(Value >= 0, FromNodeBase, ToNodeBase),
      ToNode = if_else(Value >= 0, ToNodeBase, FromNodeBase),
      FromRegion = node_to_region(FromNode),
      ToRegion = node_to_region(ToNode),
      Value = abs(Value),
      Scenario = scenario_name,
      Fuel = fuel_spec$Fuel
    ) %>%
    select(Scenario, Fuel, FromRegion, ToRegion, Value)
}

read_carrier_flows <- function(scenario_name, fuel_spec) {
  flows <- bind_rows(
    read_carrier_flow_symbol(scenario_name, "vff_l", "forward", fuel_spec),
    read_carrier_flow_symbol(scenario_name, "vfb_l", "backward", fuel_spec)
  )

  if (nrow(flows) == 0) {
    return(tibble(Scenario = character(), Fuel = character(), FromRegion = character(), ToRegion = character(), Value = numeric()))
  }

  flows %>%
    filter(FromRegion %in% region_base, ToRegion %in% region_base, FromRegion != ToRegion) %>%
    group_by(Scenario, Fuel, FromRegion, ToRegion) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")
}

carrier_edge_data <- bind_rows(lapply(target_scenarios, function(scenario_name) {
  bind_rows(lapply(seq_len(nrow(fuel_specs)), function(i) {
    read_carrier_flows(scenario_name, fuel_specs[i, ])
  }))
}))

make_region_carrier_trade_rows <- function(df, scenario_name, region_name) {
  bind_rows(lapply(fuel_levels, function(fuel_name) {
    if (region_name == "World") {
      trade <- df %>%
        filter(Scenario == scenario_name, Fuel == fuel_name, FromRegion != ToRegion) %>%
        summarise(Import = sum(Value, na.rm = TRUE), Export = sum(Value, na.rm = TRUE), .groups = "drop")
    } else {
      trade <- df %>%
        filter(Scenario == scenario_name, Fuel == fuel_name) %>%
        summarise(
          Import = sum(Value[ToRegion == region_name & FromRegion != region_name], na.rm = TRUE),
          Export = sum(Value[FromRegion == region_name & ToRegion != region_name], na.rm = TRUE),
          .groups = "drop"
        )
    }

    tibble(
      Scenario = scenario_name,
      Region = region_name,
      Fuel = fuel_name,
      Import = if_else(is.na(trade$Import), 0, trade$Import),
      Export = if_else(is.na(trade$Export), 0, trade$Export)
    )
  }))
}

carrier_trade_data <- bind_rows(lapply(target_scenarios, function(scenario_name) {
  bind_rows(lapply(region_list, function(region_name) {
    make_region_carrier_trade_rows(carrier_edge_data, scenario_name, region_name)
  }))
})) %>%
  mutate(
    Import = if_else(Region == "World", 0, Import),
    Export = if_else(Region == "World", 0, Export)
  )

carrier_balance_data <- bind_rows(lapply(target_scenarios, function(scenario_name) {
  bind_rows(lapply(region_list, function(region_name) {
    bind_rows(lapply(seq_len(nrow(fuel_specs)), function(i) {
      spec <- fuel_specs[i, ]
      tibble(
        Scenario = scenario_name,
        Region = region_name,
        Fuel = spec$Fuel,
        Supply = get_region_variable_value(all_data, spec$SupplyVariables[[1]], "PSM", scenario_name, region_name),
        Demand = get_region_variable_value(all_data, spec$DemandVariables[[1]], "AIM-Technology", scenario_name, region_name)
      )
    }))
  }))
})) %>%
  left_join(carrier_trade_data, by = c("Scenario", "Region", "Fuel")) %>%
  mutate(
    across(c(Supply, Demand, Import, Export), ~replace_na(.x, 0)),
    Fuel = factor(Fuel, levels = fuel_levels),
    Region = factor(Region, levels = region_list)
  )

finene_read_spec <- tribble(
  ~Variable,                ~PlotVariable,           ~Legend,          ~Color,
  "Fin_Ene_SolidsCoa",      "Fin_Ene_SolidsCoa",     "Coal",           "grey70",
  "Fin_Ene_Liq_Oil",        "Fin_Ene_Liq_Oil",       "Liquids-fossil", "sandybrown",
  "Fin_Ene_Gas_Fos",        "Fin_Ene_Gas_Fos",       "Gas-fossil",     "moccasin",
  "Fin_Ene_SolidsBio",      "Fin_Ene_Biomass",       "Biomass",        "#A9D65D",
  "Fin_Ene_Liq_Bio",        "Fin_Ene_Biomass",       "Biomass",        "#A9D65D",
  "Fin_Ene_Solar",          "Fin_Ene_Solar",         "Solar",          "lightsalmon",
  "Fin_Ene_Ele",            "Fin_Ene_Ele",           "Electricity",    "lightsteelblue",
  "Fin_Ene_Heat",           "Fin_Ene_Heat",          "Heat",           "salmon",
  "Fin_Ene_Hyd",            "Fin_Ene_Hyd",           "Hydrogen",       "thistle2",
  "Fin_Ene_Liq_Hyd_syn",    "Fin_Ene_Hyd_syn",       "Synfuel",        "mediumorchid1",
  "Fin_Ene_Gas_Hyd_syn",    "Fin_Ene_Hyd_syn",       "Synfuel",        "mediumorchid1"
)
finene_spec <- finene_read_spec %>%
  distinct(PlotVariable, Legend, Color, .keep_all = TRUE) %>%
  transmute(Variable = PlotVariable, Legend, Color)
finene_colors <- setNames(as.character(finene_spec$Color), finene_spec$Variable)
finene_labels <- setNames(as.character(finene_spec$Legend), finene_spec$Variable)

secele_generation_spec <- secele %>%
  mutate(
    Variable = as.character(Variable),
    PlotVariable = Variable,
    Sign = 1
  )
secele_loss_spec <- tribble(
  ~Variable, ~PlotVariable, ~Legend, ~Color, ~Sign,
  "Sec_Ene_Ele_Sto_Los", "Sec_Ene_Ele_Sto_Los", "Storage loss", "cadetblue3", -1,
  "Sec_Ene_Ele_Cur",     "Sec_Ene_Ele_Cur",     "Curtailment",  "darkkhaki", -1,
  "Sec_Ene_Ele_Tra_Los", "Sec_Ene_Ele_Tra_Los", "Transmission loss", "azure4", -1
)
secele_read_spec <- bind_rows(secele_generation_spec, secele_loss_spec)
secele_spec <- secele_read_spec %>%
  distinct(PlotVariable, Legend, Color, .keep_all = TRUE) %>%
  transmute(Variable = PlotVariable, Legend, Color)
secele_legend_spec <- bind_rows(
  secele_generation_spec %>%
    transmute(Variable = PlotVariable, Legend, Color) %>%
    arrange(desc(row_number())),
  secele_loss_spec %>%
    distinct(PlotVariable, Legend, Color, .keep_all = TRUE) %>%
    transmute(Variable = PlotVariable, Legend, Color)
)
secele_colors <- setNames(as.character(secele_spec$Color), secele_spec$Variable)
secele_labels <- setNames(as.character(secele_spec$Legend), secele_spec$Variable)

emisec_spec <- emisec %>%
  mutate(Variable = as.character(Variable))
emisec_colors <- setNames(as.character(emisec_spec$Color), emisec_spec$Variable)
emisec_labels <- setNames(as.character(emisec_spec$Legend), emisec_spec$Variable)

plot_theme <- theme_bw(base_size = 8.5) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.15, color = "grey88"),
    panel.border = element_rect(linewidth = 0.25, color = "grey55", fill = NA),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 8),
    axis.text = element_text(size = 7.5),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 7),
    legend.key.height = unit(2.6, "mm"),
    legend.key.width = unit(4.5, "mm"),
    plot.title = element_text(size = 8.5, hjust = 0.5, face = "bold"),
    plot.margin = margin(1, 2, 1, 2, unit = "mm")
  )

blank_region_label <- function(region_name) {
  ggplot() +
    annotate("text", x = 0, y = 0.5, label = region_name, hjust = 0, fontface = "bold", size = 5.2) +
    xlim(0, 1) +
    ylim(0, 1) +
    theme_void() +
    theme(plot.margin = margin(1, 1, 1, 1, unit = "mm"))
}

plot_final_energy_region <- function(df, scenario_name, region_name) {
  d <- df %>%
    filter(
      Model == "AIM-Technology",
      Scenario == scenario_name,
      Region == region_name,
      Year %in% time_years,
      Variable %in% finene_read_spec$Variable
    ) %>%
    left_join(finene_read_spec %>% select(Variable, PlotVariable), by = "Variable") %>%
    group_by(Model, Scenario, Region, Year, Variable = PlotVariable) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    select(Year, Variable, Value) %>%
    complete(Year = time_years, Variable = finene_spec$Variable, fill = list(Value = 0)) %>%
    mutate(Variable = factor(Variable, levels = rev(finene_spec$Variable)))

  ggplot(d, aes(x = Year, y = Value, fill = Variable)) +
    geom_area(linewidth = 0.05, color = "white") +
    scale_fill_manual(values = finene_colors, labels = finene_labels, drop = FALSE) +
    scale_x_continuous(breaks = c(2020, 2030, 2040, 2050)) +
    coord_cartesian(xlim = c(2020, 2050)) +
    expand_limits(y = 0) +
    labs(
      x = NULL,
      y = expression(paste("Final energy demand (EJ ", {yr^-1}, ")")),
      title = "Final energy: AIM-Technology"
    ) +
    plot_theme
}

plot_emissions_region <- function(df, scenario_name, region_name) {
  d <- df %>%
    filter(
      Model == "AIM-Technology",
      Scenario == scenario_name,
      Region == region_name,
      Year == plot_year,
      Variable %in% emisec_spec$Variable
    ) %>%
    mutate(Variable = factor(Variable, levels = rev(emisec_spec$Variable)))

  ggplot(d, aes(x = "AIM-Technology", y = Value, fill = Variable)) +
    geom_col(width = 0.58, color = "white", linewidth = 0.08) +
    geom_hline(yintercept = 0, color = "black", linetype = "dotted", linewidth = 0.25) +
    scale_fill_manual(values = emisec_colors, labels = emisec_labels, drop = FALSE) +
    expand_limits(y = 0) +
    labs(
      y = expression(paste("CO"[2], " emissions (Mt CO"[2], " ", {yr^-1}, ")")),
      title = "CO2 emissions by sector: 2050"
    ) +
    plot_theme +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7.5))
}

plot_power_mix_region <- function(df, scenario_name, region_name) {
  d <- df %>%
    filter(
      Scenario == scenario_name,
      Region == region_name,
      Year == plot_year,
      Variable %in% secele_read_spec$Variable
    ) %>%
    left_join(
      secele_read_spec %>% select(Variable, PlotVariable, Sign),
      by = "Variable"
    ) %>%
    mutate(
      Variable = PlotVariable,
      Value = if_else(Sign < 0, -abs(Value), Value),
      Variable = factor(Variable, levels = rev(secele_spec$Variable)),
      Model = factor(Model, levels = c("AIM-Technology", "PSM"))
    ) %>%
    group_by(Scenario, Region, Year, Model, Variable) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      Variable = factor(Variable, levels = rev(secele_spec$Variable)),
      Model = factor(Model, levels = c("AIM-Technology", "PSM"))
    )

  ggplot(d, aes(x = Model, y = Value, fill = Variable)) +
    geom_col(width = 0.58, color = "white", linewidth = 0.08) +
    geom_hline(yintercept = 0, color = "black", linetype = "dotted", linewidth = 0.25) +
    scale_fill_manual(values = secele_colors, labels = secele_labels, drop = FALSE) +
    expand_limits(y = 0) +
    labs(
      y = expression(paste("Power generation (EJ ", {yr^-1}, ")")),
      title = "Power generation mix: 2050"
    ) +
    plot_theme +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7.5))
}

plot_link_flow_region <- function(df, scenario_name, region_name) {
  d <- df %>%
    filter(
      Scenario == scenario_name,
      Region == region_name
    )

  if (nrow(d) == 0) {
    d <- tibble(
      FlowType = factor(c("intra", "exp", "imp"), levels = c("intra", "exp", "imp")),
      Counterpart = "intra",
      Value = 0
    )
  }

  d <- d %>%
    mutate(
      FlowType = factor(FlowType, levels = c("intra", "exp", "imp")),
      Counterpart = factor(Counterpart, levels = names(link_counterpart_colors)),
      FlowX = as.numeric(FlowType)
    )

  legend_breaks <- d %>%
    filter(Value > 0) %>%
    pull(Counterpart) %>%
    as.character() %>%
    unique()

  ggplot(d, aes(x = FlowX, y = Value, fill = Counterpart)) +
    geom_col(width = 0.58, color = "white", linewidth = 0.08) +
    scale_fill_manual(values = link_counterpart_colors, breaks = legend_breaks, drop = FALSE) +
    scale_x_continuous(breaks = c(1, 2, 3), labels = c("intra", "exp", "imp")) +
    coord_cartesian(xlim = c(0.55, 3.45), clip = "off") +
    expand_limits(y = 0) +
    labs(
      y = "Transmission flow (TWh)",
      title = "Transmission flow: 2050"
    ) +
    plot_theme +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7.5),
      legend.position = "right",
      legend.text = element_text(size = 5.8),
      legend.key.height = unit(2.0, "mm"),
      legend.key.width = unit(3.0, "mm"),
      plot.margin = margin(1, 2, 1, 2, unit = "mm")
    ) +
    guides(fill = guide_legend(ncol = 1, byrow = TRUE))
}

plot_carrier_balance_region <- function(df, scenario_name, region_name) {
  d <- df %>%
    filter(
      Scenario == scenario_name,
      Region == region_name
    ) %>%
    mutate(
      Fuel = factor(Fuel, levels = fuel_levels),
      FuelX = as.numeric(Fuel),
      TotalAvailable = Supply + Import,
      ExportYMin = pmax(TotalAvailable - Export, 0),
      ExportYMax = TotalAvailable
    )

  if (nrow(d) == 0) {
    d <- tibble(
      Fuel = factor(fuel_levels, levels = fuel_levels),
      FuelX = seq_along(fuel_levels),
      Supply = 0,
      Import = 0,
      Export = 0,
      Demand = 0,
      TotalAvailable = 0,
      ExportYMin = 0,
      ExportYMax = 0
    )
  }

  bar_data <- d %>%
    select(Fuel, FuelX, Supply, Import) %>%
    pivot_longer(c(Supply, Import), names_to = "FlowType", values_to = "Value") %>%
    mutate(FlowType = factor(FlowType, levels = fuel_flow_types))

  export_data <- d %>%
    filter(Export > 0, ExportYMax > ExportYMin)

  p <- ggplot(bar_data, aes(x = FuelX, y = Value, fill = FlowType)) +
    geom_col(width = 0.58, color = "white", linewidth = 0.08)

  if (nrow(export_data) > 0) {
    p <- p +
      geom_rect(
      data = export_data,
      aes(
        xmin = FuelX - 0.29,
        xmax = FuelX + 0.29,
        ymin = ExportYMin,
        ymax = ExportYMax,
        alpha = "Export"
      ),
      inherit.aes = FALSE,
      fill = "grey20",
      color = NA
      ) +
      scale_alpha_manual(values = c("Export" = 0.28), name = NULL)
  }

  p +
    geom_point(
      data = d,
      aes(x = FuelX, y = Demand, shape = "AIM-Tech demand"),
      inherit.aes = FALSE,
      size = 3,
      stroke = 0.45,
      fill = "white",
      color = "black"
    ) +
    scale_fill_manual(values = fuel_flow_colors, drop = FALSE) +
    scale_shape_manual(values = c("AIM-Tech demand" = 23), name = NULL) +
    scale_x_continuous(breaks = seq_along(fuel_levels), labels = c("H2", "NH3", "Syn")) +
    expand_limits(y = 0) +
    labs(
      y = expression(paste("Carrier balance (EJ ", {yr^-1}, ")")),
      title = "H2/NH3/synfuel balance: 2050"
    ) +
    plot_theme +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7.5),
      legend.position = "right",
      legend.text = element_text(size = 5.8),
      legend.key.height = unit(2.0, "mm"),
      legend.key.width = unit(3.0, "mm"),
      plot.margin = margin(1, 2, 1, 2, unit = "mm")
    ) +
    guides(
      fill = guide_legend(ncol = 1, byrow = TRUE, order = 1),
      alpha = if (nrow(export_data) > 0) guide_legend(order = 2, override.aes = list(fill = "grey20", color = NA)) else "none",
      shape = guide_legend(order = 3, override.aes = list(fill = "white", color = "black"))
    )
}

make_region_row <- function(df, scenario_name, region_name) {
  (
    blank_region_label(region_name) |
    plot_final_energy_region(df, scenario_name, region_name) |
      finene_legend |
      plot_emissions_region(df, scenario_name, region_name) |
      emisec_legend_plot |
      plot_power_mix_region(df, scenario_name, region_name) |
      secele_legend_plot |
      plot_link_flow_region(link_flow_data, scenario_name, region_name) |
      plot_carrier_balance_region(carrier_balance_data, scenario_name, region_name)
  ) +
    plot_layout(widths = c(0.28, 0.55, 0.42, 0.39, 0.50, 0.88, 0.62, 0.82, 0.92))
}

save_plot_page <- function(plot, filename) {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  ggsave(
    filename = filename,
    plot = plot,
    width = 590,
    height = 300,
    units = "mm",
    dpi = 300,
    bg = "white"
  )
}

make_manual_legend <- function(spec, colors, labels, ncol = 1, text_size = 2.0, reverse_order = TRUE) {
  legend_data <- if (reverse_order) {
    spec %>% arrange(desc(row_number()))
  } else {
    spec
  }
  legend_data <- legend_data %>%
    mutate(
      Label = unname(labels[Variable]),
      Color = unname(colors[Variable]),
      idx = row_number() - 1,
      x = (idx %% ncol) * 1.25,
      y = -floor(idx / ncol) * 0.33
    )

  ggplot(legend_data) +
    geom_tile(aes(x = x, y = y, fill = Color), width = 0.13, height = 0.13) +
    geom_text(aes(x = x + 0.10, y = y, label = Label), hjust = 0, size = text_size) +
    scale_fill_identity() +
    coord_cartesian(
      xlim = c(-0.10, (ncol - 1) * 1.25 + 1.20),
      ylim = c(min(legend_data$y) - 0.20, 0.20),
      expand = FALSE
    ) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0, unit = "mm"))
}

finene_legend <- make_manual_legend(finene_spec, finene_colors, finene_labels, ncol = 1, text_size = 2.1)
emisec_legend_plot <- make_manual_legend(emisec_spec, emisec_colors, emisec_labels, ncol = 1, text_size = 1.9)
secele_legend_plot <- make_manual_legend(
  secele_legend_spec,
  secele_colors,
  secele_labels,
  ncol = 1,
  text_size = 1.9,
  reverse_order = FALSE
)

for (scenario_name in target_scenarios) {
  out_dir <- file.path(base_dir, scenario_name, "plot_all")

  for (page_idx in seq_along(region_pages)) {
    page_regions <- region_pages[[page_idx]]
    rows <- lapply(page_regions, function(region_name) {
      make_region_row(all_data, scenario_name, region_name)
    })

    page_body <- patchwork::patchworkGrob(wrap_plots(rows, ncol = 1))
    page_plot <- wrap_elements(full = page_body) +
      plot_annotation(
        title = paste0("Regional summary: ", scenario_name, " (page ", page_idx, " / ", length(region_pages), ")")
      )

    save_plot_page(
      plot = page_plot,
      filename = file.path(
        out_dir,
        paste0("global_", scenario_name, "_regional_summary_page_", page_idx, ".png")
      )
    )
  }
}
