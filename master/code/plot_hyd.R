  # H2/NH3/synfuel balance plots -----------------------------------------------

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

  psm_main_dir <- "//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output/global_softlink_Sc/Global/2606171718/main"
  aimt_main_dir <- "//SY_3600_01/atfs02/Individual/hsakata/AIMEnduse/output/softlink_Sc/2606111707/main"
  psm_primary_dir <- file.path(dirname(psm_main_dir), "gams_output", "gdx_primary")
  r272_map_path <- "//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/AIMPSY-master/data/set/M_R272.csv"

  target_scenarios <- c("NPi500", "NPi500-ZF2050")
  plot_year <- 2050
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
  region_base <- setdiff(region_list, "World")
  region_pages <- split(region_list, ceiling(seq_along(region_list) / 16))

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

  aimt_data <- read_iamc_gdx(aimt_main_dir, "AIM-Technology") %>%
    filter(Scenario %in% target_scenarios)
  psm_data <- read_iamc_gdx(psm_main_dir, "PSM") %>%
    filter(Scenario %in% target_scenarios) %>%
    add_world_rows(region_base, replace_existing = TRUE)

  all_data <- bind_rows(aimt_data, psm_data) %>%
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

  fuel_specs <- tribble(
    ~Fuel, ~SupplyVariables, ~DemandVariables, ~LinkPattern, ~NodeSuffixPattern,
    "Hydrogen", list("Sec_Ene_Hyd"), list("Fin_Ene_Hyd"), ".*-H2_.*-H2$", "-H2$",
    "Ammonia", list("Sec_Ene_Hyd_Amm"), list("Fin_Ene_Hyd_Amm"), "^R31-[A-Z0-9]+-NH3_R31-[A-Z0-9]+-NH3$", "-NH3$",
    "Synfuel", list("Sec_Ene_Hyd_Syn"), list("Fin_Ene_Liq_Hyd_syn"), ".*-(SYN|SYF|LFS|EFS)_.*-(SYN|SYF|LFS|EFS)$", "-(SYN|SYF|LFS|EFS)$"
  )
  fuel_levels <- fuel_specs$Fuel
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
      Scenario = factor(Scenario, levels = target_scenarios),
      Fuel = factor(Fuel, levels = fuel_levels),
      Region = factor(Region, levels = region_list)
    )

  plot_theme <- theme_bw(base_size = 7.5) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.12, color = "grey88"),
      panel.border = element_rect(linewidth = 0.25, color = "grey55", fill = NA),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 7),
      axis.text = element_text(size = 6.2),
      legend.title = element_blank(),
      legend.text = element_text(size = 6.4),
      legend.key.height = unit(2.3, "mm"),
      legend.key.width = unit(3.5, "mm"),
      plot.title = element_text(size = 8.2, hjust = 0.5, face = "bold"),
      plot.margin = margin(1, 1, 1, 1, unit = "mm")
    )

  plot_carrier_balance_region <- function(df, scenario_name, region_name, show_legend = FALSE) {
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

    bar_data <- d %>%
      select(Fuel, FuelX, Supply, Import) %>%
      pivot_longer(c(Supply, Import), names_to = "FlowType", values_to = "Value") %>%
      mutate(FlowType = factor(FlowType, levels = fuel_flow_types))

    export_data <- d %>% filter(Export > 0, ExportYMax > ExportYMin)

    p <- ggplot(bar_data, aes(x = FuelX, y = Value, fill = FlowType)) +
      geom_col(width = 0.62, color = "white", linewidth = 0.08)

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
        aes(x = FuelX, y = Demand, shape = "AIM-Tech demand"),
        inherit.aes = FALSE,
        size = 2.2,
        stroke = 0.4,
        fill = "white",
        color = "black"
      ) +
      scale_fill_manual(values = fuel_flow_colors, drop = FALSE) +
      scale_shape_manual(values = c("AIM-Tech demand" = 23), name = NULL) +
      scale_x_continuous(breaks = seq_along(fuel_levels), labels = c("H2", "NH3", "Syn")) +
      expand_limits(y = 0) +
      labs(
        y = expression(paste("Balance (EJ ", {yr^-1}, ")")),
        title = region_name
      ) +
      plot_theme +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 6.2),
        legend.position = if (show_legend) "bottom" else "none"
      ) +
      guides(
        fill = guide_legend(nrow = 1, byrow = TRUE, order = 1),
        alpha = if (nrow(export_data) > 0) guide_legend(order = 2, override.aes = list(fill = "grey20", color = NA)) else "none",
        shape = guide_legend(order = 3, override.aes = list(fill = "white", color = "black"))
      )
  }

  save_plot_page <- function(plot, filename) {
    dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
    ggsave(
      filename = filename,
      plot = plot,
      width = 420,
      height = 300,
      units = "mm",
      dpi = 300,
      bg = "white"
    )
  }

  for (scenario_name in target_scenarios) {
    out_dir <- file.path(base_dir, scenario_name, "plot_hydrogen")

    for (page_idx in seq_along(region_pages)) {
      page_regions <- region_pages[[page_idx]]
      plots <- lapply(seq_along(page_regions), function(i) {
        plot_carrier_balance_region(
          carrier_balance_data,
          scenario_name,
          page_regions[[i]],
          show_legend = page_idx == 1 && i == 1
        )
      })

      page_plot <- wrap_plots(plots, ncol = 4) +
        plot_annotation(
          title = paste0("H2/NH3/synfuel balance: ", scenario_name, ", ", plot_year, " (page ", page_idx, " / ", length(region_pages), ")")
        )

      save_plot_page(
        plot = page_plot,
        filename = file.path(out_dir, paste0(scenario_name, "_hydrogen_balance_page_", page_idx, ".png"))
      )
    }
  }
