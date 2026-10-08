# PLOT SETTINGS ---------------------------------------------------------------
# Edit only the values in this section when changing runs or plot contents.

# PSM run: <PSM output root>/<SCENARIO>/<REGION>/<DATE>/main
PSM_SCENARIO <- "global_softlink_IntermReport"
PSM_REGION <- "Global"
PSM_DATE <- "2609291626"

timeresol <- "6HOUR"

# AIM-Technology run: <AIMT output root>/<SCENARIO>/<DATE>/main
AIMT_SCENARIO <- "softlink_IntermReport"
AIMT_DATE <- "2609111525"

# Use "on" to include a panel in the regional summary, or "off" to omit it.
target_scenarios <- c("500C-ELE", "500C-H2", "500C-SYF")
plot_year <- 2050

plot_targets <- c(
  final_energy = "on",
  co2_emissions = "on",
  power_generation = "on",
  transmission_flow = "on",
  h2_nh3_synfuel_balance = "on",
  biomass = "off",
  ccs_storage = "off"
)

# PACKAGE AND PROJECT INITIALIZATION ------------------------------------------

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

project_candidates <- unique(c(
  script_dir,
  file.path(script_dir, "master"),
  file.path(script_dir, "AIMPSY_plot", "master"),
  file.path(script_dir, ".."),
  file.path(script_dir, "..", "master"),
  file.path(script_dir, "..", "AIMPSY_plot", "master")
))
project_matches <- project_candidates[
  file.exists(file.path(project_candidates, "code", "dataframe.R"))
]
if (length(project_matches) == 0) {
  stop(
    "Could not locate the AIMPSY_plot/master project containing code/dataframe.R. Checked: ",
    paste(project_candidates, collapse = ", ")
  )
}
project_dir <- normalizePath(project_matches[[1]], winslash = "/", mustWork = TRUE)

source(file.path(project_dir, "code", "dataframe.R"))

# for (gams_sys_dir in c("C:/GAMS/38", "C:/GAMS/win64/26.1")) {
#   if (dir.exists(gams_sys_dir)) {
#     igdx(gams_sys_dir)
#     break
#   }
# }

# DERIVED SETTINGS AND INPUT PATHS --------------------------------------------

required_run_settings <- c(
  PSM_SCENARIO = PSM_SCENARIO,
  PSM_REGION = PSM_REGION,
  PSM_DATE = PSM_DATE,
  AIMT_SCENARIO = AIMT_SCENARIO,
  AIMT_DATE = AIMT_DATE
)
if (any(!nzchar(trimws(required_run_settings)))) {
  stop(
    "Run settings must not be blank: ",
    paste(names(required_run_settings)[!nzchar(trimws(required_run_settings))], collapse = ", ")
  )
}

psm_output_root <- "//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output"
aimt_output_root <- "//SY_3600_01/atfs02/Individual/hsakata/AIMEnduse/output"
psm_main_dir <- file.path(psm_output_root, PSM_SCENARIO, PSM_REGION, PSM_DATE, "main")
aimt_main_dir <- file.path(aimt_output_root, AIMT_SCENARIO, AIMT_DATE, "main")
psm_primary_dir <- file.path(dirname(psm_main_dir), "gams_output", "gdx_primary")
r272_map_path <- "//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/AIMPSY-master/data/set/M_R272.csv"

primary_time_weight_from_timeresol <- function(timeresol) {
  timeresol <- toupper(trimws(timeresol))
  weights <- c(
    "12DAY" = ceiling(365 / 12 * 1000) / 1000,
    "6HOUR" = 6,
    "4HOUR" = 4,
    "3HOUR" = 3,
    "1HOUR" = 1
  )

  if (!timeresol %in% names(weights)) {
    stop(
      "Unsupported timeresol: ", timeresol,
      ". Supported values: ", paste(names(weights), collapse = ", ")
    )
  }

  unname(weights[[timeresol]])
}

primary_time_weight <- primary_time_weight_from_timeresol(timeresol)

target_scenarios <- unique(trimws(as.character(target_scenarios)))
if (length(target_scenarios) == 0 || any(!nzchar(target_scenarios))) {
  stop("target_scenarios must contain at least one non-blank scenario name.")
}
if (length(plot_year) != 1 || !is.numeric(plot_year) || is.na(plot_year)) {
  stop("plot_year must be one numeric year.")
}

required_plot_targets <- c(
  "final_energy",
  "co2_emissions",
  "power_generation",
  "transmission_flow",
  "h2_nh3_synfuel_balance",
  "biomass",
  "ccs_storage"
)
if (is.null(names(plot_targets)) || any(names(plot_targets) == "") || anyDuplicated(names(plot_targets))) {
  stop("plot_targets must be a named vector with one unique name per plot target.")
}
missing_plot_targets <- setdiff(required_plot_targets, names(plot_targets))
unknown_plot_targets <- setdiff(names(plot_targets), required_plot_targets)
if (length(missing_plot_targets) > 0 || length(unknown_plot_targets) > 0) {
  stop(
    "Invalid plot_targets names. Missing: ",
    paste(missing_plot_targets, collapse = ", "),
    "; unknown: ",
    paste(unknown_plot_targets, collapse = ", ")
  )
}
plot_targets <- setNames(
  tolower(trimws(as.character(plot_targets[required_plot_targets]))),
  required_plot_targets
)
invalid_plot_switches <- names(plot_targets)[!plot_targets %in% c("on", "off")]
if (length(invalid_plot_switches) > 0) {
  stop(
    "plot_targets values must be \"on\" or \"off\": ",
    paste(invalid_plot_switches, collapse = ", ")
  )
}
plot_enabled <- setNames(plot_targets == "on", names(plot_targets))
is_plot_enabled <- function(plot_name) {
  isTRUE(unname(plot_enabled[[plot_name]]))
}

time_years <- seq(2020, 2050, by = 5)
gwh_to_ej <- 3.6e-6
twh_to_ej <- 3.6 / 1000
base_dir <- file.path(
  project_dir,
  "output",
  paste0(tolower(PSM_REGION), "_", PSM_DATE, "_", toupper(trimws(timeresol)))
)

# REGION DEFINITIONS AND IAMC DATA PREPARATION --------------------------------

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
    drop_r31_region_rows() %>%
    transmute(
      Scenario = as.character(Sc),
      Region = RawRegion,
      Variable = as.character(Sv),
      Year = as.numeric(as.character(Y5)),
      Value = suppressWarnings(as.numeric(as.character(iamc_gdx))),
      Model = model_name
    ) %>%
    filter(!is.na(Value)) %>%
    group_by(Scenario, Region, Variable, Year, Model) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")
}

drop_r31_region_rows <- function(df) {
  df %>%
    mutate(RawRegion = as.character(Sr)) %>%
    filter(!str_detect(RawRegion, "^R31-"))
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

# NODE-TO-REGION MAPPING -------------------------------------------------------

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

# ELECTRICITY TRANSMISSION FLOW DATA ------------------------------------------

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
    summarise(Value = sum(.data[[symbol]], na.rm = TRUE) * primary_time_weight / 1000, .groups = "drop") %>%
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

# H2, NH3, AND SYNFUEL BALANCE DEFINITIONS ------------------------------------

fuel_specs <- tribble(
  ~Fuel, ~SupplyVariables, ~DemandVariables, ~LinkPattern, ~NodeSuffixPattern,
  "Ammonia",
    list(list("Sec_Ene_Hyd_Amm")),
    list(list("Fin_Ene_Hyd_Amm")),
    "^R31-[A-Z0-9]+-NH3TRD_R31-[A-Z0-9]+-NH3TRD$",
    "-NH3TRD$",
  "Hydrogen",
    list(list("Sec_Ene_Hyd")),
    list(list("Fin_Ene_Hyd")),
    ".*-H2TRD_.*-H2TRD$",
    "-H2TRD$",
  "Synfuel",
    list("Sec_Ene_Hyd_syn"),
    list(c("Fin_Ene_Liq_Hyd_syn", "Fin_Ene_Gas_Hyd_syn")),
    "^R31-[A-Z0-9]+-SYFTRD_R31-[A-Z0-9]+-SYFTRD$",
    "-SYFTRD$"
)
fuel_levels <- fuel_specs$Fuel
conversion_specs <- tribble(
  ~FromCarrier, ~ToCarrier, ~FlowType, ~FillColor, ~CurveColor, ~CurveLayer, ~StackInDestination,
  "Hydrogen", "Ammonia", "H2 to NH3", "#9ecae1", "#3182bd", "lower", FALSE,
  "Ammonia", "Hydrogen", "NH3 to H2", "#de2d26", "#fb6a4a", "upper", TRUE,
  "Hydrogen", "Synfuel", "H2 to Syn", "#bcbddc", "#756bb1", "lower", FALSE,
  "Synfuel", "Hydrogen", "Syn to H2", "#fdae6b", "#e6550d", "upper", TRUE,
  "Ammonia", "Synfuel", "NH3 to Syn", "#fdd0a2", "#fd8d3c", "lower", FALSE,
  "Synfuel", "Ammonia", "Syn to NH3", "#fdae6b", "#e6550d", "upper", FALSE
)
fuel_flow_types <- c("Supply", "Import", conversion_specs$FlowType)
fuel_flow_colors <- c(
  "Supply" = "#9ecae1",
  "Import" = "#31a354",
  setNames(conversion_specs$FillColor, conversion_specs$FlowType)
)

normalize_variable_groups <- function(variables) {
  if (is.list(variables) && !is.data.frame(variables)) {
    lapply(variables, function(x) unlist(x, use.names = FALSE))
  } else {
    list(unlist(variables, use.names = FALSE))
  }
}

get_region_variable_value <- function(df, variables, model_name, scenario_name, region_name) {
  variable_groups <- normalize_variable_groups(variables)

  for (variables in variable_groups) {
    d <- df %>%
      filter(
        Model == model_name,
        Scenario == scenario_name,
        Region == region_name,
        Year == plot_year,
        str_to_lower(Variable) %in% str_to_lower(variables)
      )

    if (nrow(d) > 0) {
      value <- sum(d$Value, na.rm = TRUE)
      return(if (is.finite(value)) value else 0)
    }
  }

  0
}

get_model_consistent_demand_value <- function(df, fuel_spec, model_name, scenario_name, region_name) {
  demand <- get_region_variable_value(
    df,
    fuel_spec$DemandVariables[[1]],
    model_name,
    scenario_name,
    region_name
  )

  if (as.character(fuel_spec$Fuel) == "Hydrogen") {
    ammonia_demand <- get_region_variable_value(
      df,
      "Fin_Ene_Hyd_Amm",
      model_name,
      scenario_name,
      region_name
    )
    demand <- demand - ammonia_demand
  }

  max(demand, 0)
}

report_missing_variable_groups <- function(df, model_name, specs, column_name, value_label) {
  for (i in seq_len(nrow(specs))) {
    spec <- specs[i, ]
    groups <- normalize_variable_groups(spec[[column_name]][[1]])

    for (scenario_name in target_scenarios) {
      has_group <- vapply(groups, function(variables) {
        df %>%
          filter(
            Model == model_name,
            Scenario == scenario_name,
            Year == plot_year,
            str_to_lower(Variable) %in% str_to_lower(variables)
          ) %>%
          nrow() > 0
      }, logical(1))

      if (!any(has_group)) {
        warning(
          "No ",
          value_label,
          " variables found for ",
          spec$Fuel,
          " in ",
          model_name,
          " iamc_gdx / scenario ",
          scenario_name,
          ". Tried: ",
          paste(vapply(groups, paste, collapse = "+", character(1)), collapse = " or ")
        )
      }
    }
  }
}

report_missing_variable_groups(all_data, "PSM", fuel_specs, "SupplyVariables", "supply")
report_missing_variable_groups(all_data, "AIM-Technology", fuel_specs, "DemandVariables", "demand")

for (scenario_name in target_scenarios) {
  if (!any(
    all_data$Model == "AIM-Technology" &
      all_data$Scenario == scenario_name &
      all_data$Year == plot_year &
      str_to_lower(all_data$Variable) == str_to_lower("Fin_Ene_Hyd_Amm")
  )) {
    warning(
      "No Fin_Ene_Hyd_Amm values found in AIM-Technology iamc_gdx / scenario ",
      scenario_name,
      ". Hydrogen direct demand requires Fin_Ene_Hyd - Fin_Ene_Hyd_Amm."
    )
  }
}

# CARRIER TRANSPORT AND CONVERSION FLOW DATA ----------------------------------

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
    summarise(Value = sum(.data[[symbol]], na.rm = TRUE) * primary_time_weight * gwh_to_ej, .groups = "drop") %>%
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

report_missing_carrier_transport <- function(df, fuel_name) {
  for (scenario_name in target_scenarios) {
    has_transport <- df %>%
      filter(Scenario == scenario_name, Fuel == fuel_name, Value > 0) %>%
      nrow() > 0

    if (!has_transport) {
      warning("No regional transport links found for ", fuel_name, " / scenario ", scenario_name, ".")
    }
  }
}

report_missing_carrier_transport(carrier_edge_data, "Synfuel")

get_conversion_carrier <- function(node_name) {
  case_when(
    str_detect(node_name, "-H2$") ~ "Hydrogen",
    str_detect(node_name, "-NH3$") ~ "Ammonia",
    str_detect(node_name, "-SYF$") ~ "Synfuel",
    TRUE ~ NA_character_
  )
}

conversion_link_pattern <- "^(.+-(?:H2|NH3|SYN|SYF|LFS|EFS))_(.+-(?:H2|NH3|SYN|SYF|LFS|EFS))$"

normalize_conversion_node <- function(node_name) {
  node_name %>%
    str_remove("-(H2|NH3|SYN|SYF|LFS|EFS)$") %>%
    str_remove("^R31-")
}

read_conversion_flow_symbol <- function(scenario_name, symbol, base_direction) {
  gdx_path <- file.path(psm_primary_dir, scenario_name, paste0(plot_year, ".gdx"))
  if (!file.exists(gdx_path)) {
    warning("Missing conversion flow GDX file: ", gdx_path)
    return(tibble())
  }

  flows_raw <- rgdx.param(gdx_path, symbol) %>%
    mutate(across(where(is.factor), as.character)) %>%
    filter(str_detect(L, conversion_link_pattern)) %>%
    group_by(L) %>%
    summarise(Value = sum(.data[[symbol]], na.rm = TRUE) * primary_time_weight * gwh_to_ej, .groups = "drop") %>%
    filter(is.finite(Value), Value != 0)

  if (nrow(flows_raw) == 0) {
    return(tibble(Scenario = character(), FromCarrier = character(), ToCarrier = character(), FromRegion = character(), ToRegion = character(), Value = numeric()))
  }

  link_nodes <- str_match(flows_raw$L, conversion_link_pattern)

  flows <- flows_raw %>%
    mutate(
      N0 = link_nodes[, 2],
      N1 = link_nodes[, 3],
      N0Carrier = get_conversion_carrier(N0),
      N1Carrier = get_conversion_carrier(N1),
      N0 = normalize_conversion_node(N0),
      N1 = normalize_conversion_node(N1)
    )

  if (base_direction == "backward") {
    flows <- flows %>%
      mutate(
        FromNodeBase = N1,
        ToNodeBase = N0,
        FromCarrierBase = N1Carrier,
        ToCarrierBase = N0Carrier
      )
  } else {
    flows <- flows %>%
      mutate(
        FromNodeBase = N0,
        ToNodeBase = N1,
        FromCarrierBase = N0Carrier,
        ToCarrierBase = N1Carrier
      )
  }

  flows %>%
    mutate(
      FromNode = if_else(Value >= 0, FromNodeBase, ToNodeBase),
      ToNode = if_else(Value >= 0, ToNodeBase, FromNodeBase),
      FromCarrier = if_else(Value >= 0, FromCarrierBase, ToCarrierBase),
      ToCarrier = if_else(Value >= 0, ToCarrierBase, FromCarrierBase),
      FromRegion = node_to_region(FromNode),
      ToRegion = node_to_region(ToNode),
      Value = abs(Value),
      Scenario = scenario_name
    ) %>%
    select(Scenario, FromCarrier, ToCarrier, FromRegion, ToRegion, Value)
}

read_conversion_flows <- function(scenario_name) {
  flows <- bind_rows(
    read_conversion_flow_symbol(scenario_name, "vff_l", "forward"),
    read_conversion_flow_symbol(scenario_name, "vfb_l", "backward")
  )

  if (nrow(flows) == 0) {
    return(tibble(Scenario = character(), FromCarrier = character(), ToCarrier = character(), FromRegion = character(), ToRegion = character(), Value = numeric()))
  }

  flows %>%
    filter(FromRegion %in% region_base, ToRegion %in% region_base) %>%
    filter(!is.na(FromCarrier), !is.na(ToCarrier), FromCarrier != ToCarrier) %>%
    group_by(Scenario, FromCarrier, ToCarrier, FromRegion, ToRegion) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")
}

conversion_flow_data <- bind_rows(lapply(target_scenarios, read_conversion_flows))

conversion_plot_data <- conversion_flow_data %>%
  inner_join(conversion_specs, by = c("FromCarrier", "ToCarrier")) %>%
  group_by(Scenario, Region = ToRegion, FromCarrier, ToCarrier, FlowType, CurveColor, CurveLayer, StackInDestination) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")

conversion_plot_world <- conversion_plot_data %>%
  group_by(Scenario, FromCarrier, ToCarrier, FlowType, CurveColor, CurveLayer, StackInDestination) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Region = "World")

conversion_plot_data <- bind_rows(conversion_plot_data, conversion_plot_world)

conversion_supply_data <- conversion_plot_data %>%
  filter(StackInDestination) %>%
  transmute(
    Scenario,
    Region,
    Fuel = ToCarrier,
    FlowType,
    Value
  )

incoming_conversion_totals <- conversion_supply_data %>%
  group_by(Scenario, Region, Fuel) %>%
  summarise(IncomingConversion = sum(Value, na.rm = TRUE), .groups = "drop")

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
        Demand = get_model_consistent_demand_value(all_data, spec, "AIM-Technology", scenario_name, region_name)
      )
    }))
  }))
})) %>%
  left_join(carrier_trade_data, by = c("Scenario", "Region", "Fuel")) %>%
  left_join(incoming_conversion_totals, by = c("Scenario", "Region", "Fuel")) %>%
  mutate(
    across(c(Supply, IncomingConversion, Demand, Import, Export), ~replace_na(.x, 0)),
    Scenario = factor(Scenario, levels = target_scenarios),
    Fuel = factor(Fuel, levels = fuel_levels),
    Region = factor(Region, levels = region_list)
  )

# BIOMASS BALANCE DATA ---------------------------------------------------------

biomass_supply_variables <- list(
  c("Prm_Ene_Bio_Ele", "Prm_Ene_Bio_Hyd")
)
biomass_spec <- tibble(
  Fuel = "Biomass",
  LinkPattern = "^R31-[A-Z0-9]+-BMSTRD_R31-[A-Z0-9]+-BMSTRD$",
  NodeSuffixPattern = "-BMSTRD$"
)

read_biomass_flow_symbol <- function(scenario_name, symbol, base_direction) {
  gdx_path <- file.path(psm_primary_dir, scenario_name, paste0(plot_year, ".gdx"))
  if (!file.exists(gdx_path)) {
    warning("Missing biomass flow GDX file: ", gdx_path)
    return(tibble())
  }

  rgdx.param(gdx_path, symbol) %>%
    mutate(across(where(is.factor), as.character)) %>%
    filter(str_detect(L, biomass_spec$LinkPattern)) %>%
    group_by(L) %>%
    summarise(Value = sum(.data[[symbol]], na.rm = TRUE) * primary_time_weight * gwh_to_ej, .groups = "drop") %>%
    filter(is.finite(Value), Value != 0) %>%
    separate(L, sep = "_", into = c("N0", "N1"), remove = FALSE) %>%
    mutate(
      N0 = normalize_carrier_node(N0, biomass_spec$NodeSuffixPattern),
      N1 = normalize_carrier_node(N1, biomass_spec$NodeSuffixPattern),
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

read_biomass_flows <- function(scenario_name) {
  flows <- bind_rows(
    read_biomass_flow_symbol(scenario_name, "vff_l", "forward"),
    read_biomass_flow_symbol(scenario_name, "vfb_l", "backward")
  )

  if (nrow(flows) == 0) {
    return(tibble(Scenario = character(), FromRegion = character(), ToRegion = character(), Value = numeric()))
  }

  flows %>%
    filter(FromRegion %in% region_base, ToRegion %in% region_base, FromRegion != ToRegion) %>%
    group_by(Scenario, FromRegion, ToRegion) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")
}

biomass_edge_data <- bind_rows(lapply(target_scenarios, read_biomass_flows))

make_region_biomass_trade_rows <- function(df, scenario_name, region_name) {
  if (region_name == "World") {
    trade <- df %>%
      filter(Scenario == scenario_name, FromRegion != ToRegion) %>%
      summarise(Import = sum(Value, na.rm = TRUE), Export = sum(Value, na.rm = TRUE), .groups = "drop")
  } else {
    trade <- df %>%
      filter(Scenario == scenario_name) %>%
      summarise(
        Import = sum(Value[ToRegion == region_name & FromRegion != region_name], na.rm = TRUE),
        Export = sum(Value[FromRegion == region_name & ToRegion != region_name], na.rm = TRUE),
        .groups = "drop"
      )
  }

  tibble(
    Scenario = scenario_name,
    Region = region_name,
    Import = if_else(is.na(trade$Import), 0, trade$Import),
    Export = if_else(is.na(trade$Export), 0, trade$Export)
  )
}

biomass_trade_data <- bind_rows(lapply(target_scenarios, function(scenario_name) {
  bind_rows(lapply(region_list, function(region_name) {
    make_region_biomass_trade_rows(biomass_edge_data, scenario_name, region_name)
  }))
})) %>%
  mutate(
    Import = if_else(Region == "World", 0, Import),
    Export = if_else(Region == "World", 0, Export)
  )

biomass_import_source_data <- biomass_edge_data %>%
  group_by(Scenario, Region = ToRegion, SourceRegion = FromRegion) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  filter(Value > 0) %>%
  mutate(
    Scenario = factor(Scenario, levels = target_scenarios),
    Region = factor(Region, levels = region_list),
    SourceRegion = factor(SourceRegion, levels = region_base)
  )

read_biomass_power_use <- function(scenario_name) {
  gdx_path <- file.path(psm_primary_dir, scenario_name, paste0(plot_year, ".gdx"))
  if (!file.exists(gdx_path)) {
    warning("Missing biomass use GDX file: ", gdx_path)
    return(tibble(Scenario = character(), Region = character(), Demand = numeric()))
  }

  use_data <- rgdx.param(gdx_path, "vp_l") %>%
    mutate(across(where(is.factor), as.character)) %>%
    filter(I == "AC", K %in% c("T_CRN", "T_CRNC")) %>%
    transmute(
      Scenario = scenario_name,
      Region = node_to_region(normalize_link_node(N)),
      Demand = suppressWarnings(as.numeric(as.character(vp_l))) * twh_to_ej
    ) %>%
    filter(Region %in% region_base, is.finite(Demand), Demand > 0) %>%
    group_by(Scenario, Region) %>%
    summarise(Demand = sum(Demand, na.rm = TRUE), .groups = "drop")

  if (nrow(use_data) == 0) {
    return(tibble(Scenario = character(), Region = character(), Demand = numeric()))
  }

  world <- use_data %>%
    group_by(Scenario) %>%
    summarise(Demand = sum(Demand, na.rm = TRUE), .groups = "drop") %>%
    mutate(Region = "World")

  bind_rows(use_data, world) %>%
    mutate(
      Scenario = factor(Scenario, levels = target_scenarios),
      Region = factor(Region, levels = region_list)
    )
}

biomass_power_use_data <- bind_rows(lapply(target_scenarios, read_biomass_power_use))

biomass_balance_data <- bind_rows(lapply(target_scenarios, function(scenario_name) {
  bind_rows(lapply(region_list, function(region_name) {
    tibble(
      Scenario = scenario_name,
      Region = region_name,
      Supply = get_region_variable_value(all_data, biomass_supply_variables, "AIM-Technology", scenario_name, region_name)
    )
  }))
})) %>%
  left_join(biomass_power_use_data, by = c("Scenario", "Region")) %>%
  left_join(biomass_trade_data, by = c("Scenario", "Region")) %>%
  mutate(
    across(c(Supply, Demand, Import, Export), ~replace_na(.x, 0)),
    Scenario = factor(Scenario, levels = target_scenarios),
    Region = factor(Region, levels = region_list),
    TotalAvailable = Supply + Import,
    ExportYMin = pmax(TotalAvailable - Export, 0),
    ExportYMax = TotalAvailable
  )

biomass_source_levels <- c("Supply limit", region_base)
biomass_source_colors <- c(
  "Supply limit" = "#7fbf3f",
  setNames(grDevices::hcl.colors(length(region_base), palette = "Dark 3"), region_base)
)

make_biomass_bar_data <- function(balance_row, imports) {
  domestic <- balance_row %>%
    transmute(
      Scenario,
      Region,
      SourceRegion = "Supply limit",
      Value = Supply
    )

  incoming <- imports %>%
    filter(
      Scenario == balance_row$Scenario[[1]],
      Region == balance_row$Region[[1]]
    ) %>%
    transmute(
      Scenario = as.character(Scenario),
      Region = as.character(Region),
      SourceRegion = as.character(SourceRegion),
      Value
    )

  bind_rows(domestic, incoming) %>%
    filter(Value != 0) %>%
    mutate(
      SourceRegion = factor(SourceRegion, levels = biomass_source_levels),
      SourceOrder = match(as.character(SourceRegion), biomass_source_levels)
    ) %>%
    arrange(SourceOrder) %>%
    mutate(
      ymin = lag(cumsum(Value), default = 0),
      ymax = cumsum(Value),
      ymid = (ymin + ymax) / 2
    )
}

# CCS STORAGE POTENTIAL AND USE DATA ------------------------------------------

ccs_storage_kind_levels <- c("Storage use")
ccs_storage_kind_colors <- c("Storage use" = "#238b45")

get_primary_gdx_path <- function(scenario_name) {
  file.path(psm_primary_dir, scenario_name, paste0(plot_year, ".gdx"))
}

add_world_storage_rows <- function(df) {
  world <- df %>%
    filter(Region %in% region_base) %>%
    group_by(Scenario, StorageKind) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    mutate(Region = "World")

  bind_rows(df, world)
}

read_ccs_storage_potential <- function(scenario_name) {
  gdx_path <- get_primary_gdx_path(scenario_name)
  if (!file.exists(gdx_path)) {
    warning("Missing primary GDX file: ", gdx_path)
    return(tibble(Scenario = character(), Region = character(), StorageKind = character(), Value = numeric()))
  }

  potential_data <- rgdx.param(gdx_path, "pmax") %>%
    mutate(across(where(is.factor), as.character)) %>%
    filter(MK == "SCO2") %>%
    transmute(
      Scenario = scenario_name,
      Region = node_to_region(ME),
      StorageKind = "Storage potential",
      Value = suppressWarnings(as.numeric(as.character(pmax)))
    ) %>%
    filter(Region %in% region_base, is.finite(Value), Value > 0) %>%
    group_by(Scenario, Region, StorageKind) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")

  if (nrow(potential_data) == 0) {
    return(tibble(Scenario = character(), Region = character(), StorageKind = character(), Value = numeric()))
  }

  potential_data %>%
    add_world_storage_rows() %>%
    mutate(
      Region = factor(Region, levels = region_list),
      StorageKind = factor(StorageKind, levels = "Storage potential")
    )
}

read_ccs_storage_use <- function(scenario_name) {
  gdx_path <- get_primary_gdx_path(scenario_name)
  if (!file.exists(gdx_path)) {
    warning("Missing primary GDX file: ", gdx_path)
    return(tibble(Scenario = character(), Region = character(), StorageKind = character(), Value = numeric()))
  }

  use_data <- rgdx.param(gdx_path, "vp_l") %>%
    mutate(across(where(is.factor), as.character)) %>%
    filter(I == "CCS", K %in% c("SCO2ON", "SCO2OF")) %>%
    transmute(
      Scenario = scenario_name,
      Region = node_to_region(N),
      StorageKind = "Storage use",
      Value = suppressWarnings(as.numeric(as.character(vp_l)))
    ) %>%
    filter(Region %in% region_base, is.finite(Value), Value > 0) %>%
    group_by(Scenario, Region, StorageKind) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")

  if (nrow(use_data) == 0) {
    return(tibble(Scenario = character(), Region = character(), StorageKind = character(), Value = numeric()))
  }

  use_data %>%
    add_world_storage_rows() %>%
    mutate(
      Region = factor(Region, levels = region_list),
      StorageKind = factor(StorageKind, levels = ccs_storage_kind_levels)
    )
}

ccs_potential_data <- bind_rows(lapply(target_scenarios, read_ccs_storage_potential))
ccs_use_data <- bind_rows(lapply(target_scenarios, read_ccs_storage_use))

ccs_summary_data <- expand_grid(
  Scenario = target_scenarios,
  Region = region_list
) %>%
  left_join(
    ccs_potential_data %>%
      group_by(Scenario, Region) %>%
      summarise(Potential = sum(Value, na.rm = TRUE), .groups = "drop"),
    by = c("Scenario", "Region")
  ) %>%
  left_join(
    ccs_use_data %>%
      group_by(Scenario, Region) %>%
      summarise(Use = sum(Value, na.rm = TRUE), .groups = "drop"),
    by = c("Scenario", "Region")
  ) %>%
  mutate(
    Potential = replace_na(Potential, 0),
    Use = replace_na(Use, 0),
    Scenario = factor(Scenario, levels = target_scenarios),
    Region = factor(Region, levels = region_list)
  )

# PLOT VARIABLE, LABEL, AND COLOR DEFINITIONS ---------------------------------

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

# REGIONAL PANEL THEMES AND PLOT FUNCTIONS ------------------------------------

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

carrier_plot_theme <- theme_bw(base_size = 7.5) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.12, color = "grey88"),
    panel.border = element_rect(linewidth = 0.25, color = "grey55", fill = NA),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 7),
    axis.text = element_text(size = 6.2),
    legend.title = element_blank(),
    legend.text = element_text(size = 5.8),
    legend.key.height = unit(2.0, "mm"),
    legend.key.width = unit(3.0, "mm"),
    plot.title = element_text(size = 8.2, hjust = 0.5, face = "bold"),
    plot.margin = margin(1, 2, 1, 2, unit = "mm")
  )

make_ribbon_polygon_data <- function(ribbon_data, n_points = 28) {
  if (nrow(ribbon_data) == 0) {
    return(tibble(RibbonId = character(), FlowType = character(), x = numeric(), y = numeric()))
  }

  bind_rows(lapply(seq_len(nrow(ribbon_data)), function(i) {
    r <- ribbon_data[i, ]
    t <- seq(0, 1, length.out = n_points)
    x0 <- as.numeric(r$XStart)
    x1 <- as.numeric(r$XEnd)
    dx <- x1 - x0

    bezier <- function(p0, p1, p2, p3) {
      (1 - t)^3 * p0 + 3 * (1 - t)^2 * t * p1 + 3 * (1 - t) * t^2 * p2 + t^3 * p3
    }

    x_top <- bezier(x0, x0 + 0.45 * dx, x1 - 0.45 * dx, x1)
    y_top <- bezier(
      as.numeric(r$YStartMax),
      as.numeric(r$YStartMax),
      as.numeric(r$YEndMax),
      as.numeric(r$YEndMax)
    )
    y_bottom <- bezier(
      as.numeric(r$YStartMin),
      as.numeric(r$YStartMin),
      as.numeric(r$YEndMin),
      as.numeric(r$YEndMin)
    )

    tibble(
      RibbonId = as.character(r$RibbonId),
      FlowType = as.character(r$FlowType),
      x = c(x_top, rev(x_top)),
      y = c(y_top, rev(y_bottom))
    )
  }))
}

plot_carrier_balance_region <- function(df, conversion_df, scenario_name, region_name, show_legend = TRUE) {
  bar_width <- 0.62

  d <- df %>%
    filter(
      Scenario == scenario_name,
      Region == region_name
    ) %>%
    mutate(
      Fuel = factor(Fuel, levels = fuel_levels),
      FuelX = as.numeric(Fuel),
      TotalAvailable = Supply + Import + IncomingConversion,
      ExportYMin = pmax(TotalAvailable - Export, 0),
      ExportYMax = TotalAvailable
    )

  base_bar_data <- d %>%
    select(Fuel, FuelX, Supply, Import) %>%
    pivot_longer(c(Supply, Import), names_to = "FlowType", values_to = "Value") %>%
    mutate(FlowType = factor(FlowType, levels = fuel_flow_types))

  conversion_bar_data <- conversion_df %>%
    filter(Scenario == scenario_name, Region == region_name, StackInDestination) %>%
    mutate(
      Fuel = factor(ToCarrier, levels = fuel_levels),
      FuelX = as.numeric(Fuel),
      FlowType = factor(FlowType, levels = fuel_flow_types)
    ) %>%
    select(Fuel, FuelX, FlowType, Value)

  bar_data <- bind_rows(base_bar_data, conversion_bar_data) %>%
    filter(Value != 0) %>%
    mutate(StackOrder = match(as.character(FlowType), fuel_flow_types)) %>%
    arrange(FuelX, StackOrder) %>%
    group_by(Fuel, FuelX) %>%
    mutate(ymin = lag(cumsum(Value), default = 0), ymax = cumsum(Value)) %>%
    ungroup()

  source_bar_bounds <- d %>%
    transmute(
      FromCarrier = as.character(Fuel),
      FromX = FuelX,
      SourceTotal = TotalAvailable
    )
  target_conversion_bounds <- bar_data %>%
    filter(as.character(FlowType) %in% conversion_specs$FlowType) %>%
    transmute(
      ToCarrier = as.character(Fuel),
      FlowType = as.character(FlowType),
      TargetYMin = ymin,
      TargetYMax = ymax
    )

  conversion_ribbon_data <- conversion_df %>%
    filter(Scenario == scenario_name, Region == region_name, Value > 0) %>%
    mutate(
      FromX = match(FromCarrier, fuel_levels),
      ToX = match(ToCarrier, fuel_levels)
    ) %>%
    filter(!is.na(FromX), !is.na(ToX), FromX != ToX) %>%
    left_join(source_bar_bounds, by = c("FromCarrier", "FromX")) %>%
    left_join(target_conversion_bounds, by = c("ToCarrier", "FlowType")) %>%
    left_join(
      d %>% transmute(ToCarrier = as.character(Fuel), ToX = FuelX, SupplyYMin = 0, SupplyYMax = Supply),
      by = c("ToCarrier", "ToX")
    ) %>%
    filter(!is.na(SourceTotal)) %>%
    mutate(
      TargetYMin = if_else(StackInDestination, TargetYMin, SupplyYMin),
      TargetYMax = if_else(StackInDestination, TargetYMax, SupplyYMax),
      SourceYMin = if_else(CurveLayer == "upper", pmax(SourceTotal - Value, 0), 0),
      SourceYMax = if_else(CurveLayer == "upper", SourceTotal, pmin(Value, SourceTotal)),
      XStart = if_else(FromX <= ToX, FromX + bar_width / 2, FromX - bar_width / 2),
      XEnd = if_else(FromX <= ToX, ToX - bar_width / 2, ToX + bar_width / 2),
      RibbonId = paste(Scenario, Region, FromCarrier, ToCarrier, FlowType, row_number(), sep = "__"),
      YStartMin = SourceYMin,
      YStartMax = SourceYMax,
      YEndMin = TargetYMin,
      YEndMax = TargetYMax
    ) %>%
    filter(!is.na(YEndMin), !is.na(YEndMax), YEndMax > YEndMin)

  ribbon_polygon_data <- make_ribbon_polygon_data(conversion_ribbon_data)
  export_data <- d %>%
    filter(Export > 0, ExportYMax > ExportYMin)

  p <- ggplot()

  if (nrow(ribbon_polygon_data) > 0) {
    p <- p +
      geom_polygon(
        data = ribbon_polygon_data,
        aes(x = x, y = y, group = RibbonId, fill = FlowType),
        inherit.aes = FALSE,
        alpha = 0.38,
        color = NA,
        show.legend = FALSE
      )
  }

  p <- p +
    geom_rect(
      data = bar_data,
      aes(xmin = FuelX - bar_width / 2, xmax = FuelX + bar_width / 2, ymin = ymin, ymax = ymax, fill = FlowType),
      color = "white",
      linewidth = 0.08,
      inherit.aes = FALSE
    )

  if (nrow(export_data) > 0) {
    p <- p +
      geom_rect(
      data = export_data,
      aes(
        xmin = FuelX - 0.31,
        xmax = FuelX + 0.31,
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
      aes(x = FuelX, y = Demand, shape = "AIM-Tech direct demand"),
      inherit.aes = FALSE,
      size = 2.2,
      stroke = 0.4,
      fill = "white",
      color = "black"
    ) +
    scale_fill_manual(values = fuel_flow_colors, drop = FALSE) +
    scale_shape_manual(values = c("AIM-Tech direct demand" = 23), name = NULL) +
    scale_x_continuous(breaks = seq_along(fuel_levels), labels = c("NH3", "H2", "Syn")) +
    expand_limits(y = 0) +
    labs(
      y = expression(paste("Balance (EJ ", {yr^-1}, ")")),
      title = "H2/NH3/synfuel balance: 2050"
    ) +
    carrier_plot_theme +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 6.2),
      legend.position = if (show_legend) "right" else "none"
    ) +
    guides(
      fill = guide_legend(ncol = 1, byrow = TRUE, order = 1),
      alpha = if (nrow(export_data) > 0) guide_legend(order = 2, override.aes = list(fill = "grey20", color = NA)) else "none",
      shape = guide_legend(order = 3, override.aes = list(fill = "white", color = "black"))
    )
}

plot_biomass_balance_region <- function(balance_df, import_df, scenario_name, region_name) {
  bar_width <- 0.62

  d <- balance_df %>%
    filter(
      Scenario == scenario_name,
      Region == region_name
    )

  if (nrow(d) == 0) {
    d <- tibble(
      Scenario = scenario_name,
      Region = region_name,
      Supply = 0,
      Demand = 0,
      Import = 0,
      Export = 0,
      TotalAvailable = 0,
      ExportYMin = 0,
      ExportYMax = 0
    )
  }

  bar_data <- make_biomass_bar_data(d, import_df)
  export_data <- d %>% filter(Export > 0, ExportYMax > ExportYMin)

  p <- ggplot()

  if (nrow(bar_data) > 0) {
    p <- p +
      geom_rect(
        data = bar_data,
        aes(
          xmin = 1 - bar_width / 2,
          xmax = 1 + bar_width / 2,
          ymin = ymin,
          ymax = ymax,
          fill = SourceRegion
        ),
        color = "white",
        linewidth = 0.08,
        inherit.aes = FALSE
      )
  }

  if (nrow(export_data) > 0) {
    p <- p +
      geom_rect(
        data = export_data,
        aes(
          xmin = 1 - bar_width / 2,
          xmax = 1 + bar_width / 2,
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

  p <- p +
    geom_point(
      data = d,
      aes(x = 1, y = Demand, shape = "Power generation use"),
      inherit.aes = FALSE,
      size = 2.2,
      stroke = 0.4,
      fill = "white",
      color = "black"
    ) +
    scale_fill_manual(values = biomass_source_colors, drop = FALSE) +
    scale_shape_manual(values = c("Power generation use" = 23), name = NULL) +
    scale_x_continuous(breaks = 1, labels = "Biomass", limits = c(0.45, 1.55)) +
    expand_limits(y = 0)

  if (region_name != "World") {
    p <- p + coord_cartesian(ylim = c(0, 30))
  }

  p +
    labs(
      y = expression(paste("Biomass availability and use (EJ ", {yr^-1}, ")")),
      title = "Biomass: 2050"
    ) +
    carrier_plot_theme +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 6.2),
      legend.position = "none"
    )
}

plot_ccs_storage_region <- function(summary_df, use_df, scenario_name, region_name) {
  bar_width <- 0.62

  use_bar <- use_df %>%
    filter(Scenario == scenario_name, Region == region_name) %>%
    mutate(
      StorageKind = factor(StorageKind, levels = ccs_storage_kind_levels),
      ymin = lag(cumsum(Value), default = 0),
      ymax = cumsum(Value),
      ymid = (ymin + ymax) / 2
    )

  if (nrow(use_bar) == 0) {
    use_bar <- tibble(
      StorageKind = factor("Storage use", levels = ccs_storage_kind_levels),
      Value = 0,
      ymin = 0,
      ymax = 0,
      ymid = 0
    )
  }

  summary_row <- summary_df %>%
    filter(Scenario == scenario_name, Region == region_name)

  if (nrow(summary_row) == 0) {
    summary_row <- tibble(Scenario = scenario_name, Region = region_name, Potential = 0, Use = 0)
  }

  potential_max <- max(c(summary_row$Potential, 0), na.rm = TRUE)
  use_max <- max(c(summary_row$Use, use_bar$Value, 0), na.rm = TRUE)
  axis_scale <- if (is.finite(potential_max) && is.finite(use_max) && potential_max > 0 && use_max > 0) {
    potential_max / use_max
  } else {
    1
  }
  summary_row <- summary_row %>%
    mutate(PotentialPlot = Potential / axis_scale)

  p <- ggplot()

  if (nrow(use_bar) > 0) {
    p <- p +
      geom_rect(
        data = use_bar,
        aes(
          xmin = 1 - bar_width / 2,
          xmax = 1 + bar_width / 2,
          ymin = ymin,
          ymax = ymax,
          fill = StorageKind
        ),
        color = "white",
        linewidth = 0.08,
        inherit.aes = FALSE
      )
  }

  p +
    geom_point(
      data = summary_row,
      aes(x = 1, y = PotentialPlot, shape = "Storage potential"),
      inherit.aes = FALSE,
      size = 2.8,
      stroke = 0.55,
      fill = "white",
      color = "black"
    ) +
    scale_fill_manual(values = ccs_storage_kind_colors, drop = FALSE) +
    scale_shape_manual(values = c("Storage potential" = 23), name = NULL) +
    scale_x_continuous(breaks = 1, labels = "CCS", limits = c(0.45, 1.55)) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.08)),
      sec.axis = sec_axis(~ . * axis_scale, name = expression(paste("Potential (MtCO"[2], " ", {yr^-1}, ")")))
    ) +
    labs(
      y = expression(paste("Use (MtCO"[2], " ", {yr^-1}, ")")),
      title = "CCS storage: 2050"
    ) +
    carrier_plot_theme +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 6.2),
      axis.title.y.right = element_text(size = 7),
      axis.text.y.right = element_text(size = 6.2),
      legend.position = "none"
    )
}

# SUMMARY PANEL LAYOUT ---------------------------------------------------------

summary_panel_widths <- list(
  region_label = 0.28,
  final_energy = c(0.55, 0.42),
  co2_emissions = c(0.39, 0.50),
  power_generation = c(0.88, 0.62),
  transmission_flow = 0.82,
  h2_nh3_synfuel_balance = 0.92,
  biomass = 0.72,
  ccs_storage = 0.72
)

active_summary_widths <- function() {
  widths <- summary_panel_widths$region_label
  for (plot_name in required_plot_targets) {
    if (is_plot_enabled(plot_name)) {
      widths <- c(widths, summary_panel_widths[[plot_name]])
    }
  }
  widths
}

make_region_row <- function(df, scenario_name, region_name) {
  row_plots <- list(blank_region_label(region_name))

  if (is_plot_enabled("final_energy")) {
    row_plots <- c(
      row_plots,
      list(plot_final_energy_region(df, scenario_name, region_name), finene_legend)
    )
  }
  if (is_plot_enabled("co2_emissions")) {
    row_plots <- c(
      row_plots,
      list(plot_emissions_region(df, scenario_name, region_name), emisec_legend_plot)
    )
  }
  if (is_plot_enabled("power_generation")) {
    row_plots <- c(
      row_plots,
      list(plot_power_mix_region(df, scenario_name, region_name), secele_legend_plot)
    )
  }
  if (is_plot_enabled("transmission_flow")) {
    row_plots <- c(
      row_plots,
      list(plot_link_flow_region(link_flow_data, scenario_name, region_name))
    )
  }
  if (is_plot_enabled("h2_nh3_synfuel_balance")) {
    row_plots <- c(
      row_plots,
      list(
        plot_carrier_balance_region(
          carrier_balance_data,
          conversion_plot_data,
          scenario_name,
          region_name
        )
      )
    )
  }
  if (is_plot_enabled("biomass")) {
    row_plots <- c(
      row_plots,
      list(
        plot_biomass_balance_region(
          biomass_balance_data,
          biomass_import_source_data,
          scenario_name,
          region_name
        )
      )
    )
  }
  if (is_plot_enabled("ccs_storage")) {
    row_plots <- c(
      row_plots,
      list(plot_ccs_storage_region(ccs_summary_data, ccs_use_data, scenario_name, region_name))
    )
  }

  wrap_plots(row_plots, nrow = 1, widths = active_summary_widths())
}

save_plot_page <- function(plot, filename, width_mm) {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  ggsave(
    filename = filename,
    plot = plot,
    width = width_mm,
    height = 300,
    units = "mm",
    dpi = 300,
    bg = "white"
  )
}

# MANUAL LEGEND CONSTRUCTION --------------------------------------------------

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

full_summary_width <- sum(unlist(summary_panel_widths, use.names = FALSE))
summary_output_width_mm <- max(
  80,
  820 * sum(active_summary_widths()) / full_summary_width
)

# GENERATE AND SAVE REGIONAL SUMMARY PLOTS ------------------------------------

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
        paste0(
          tolower(PSM_REGION),
          "_",
          scenario_name,
          "_regional_summary_page_",
          page_idx,
          ".png"
        )
      ),
      width_mm = summary_output_width_mm
    )
  }
}
