  # Biomass balance plots -------------------------------------------------------

  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)
  library(ggplot2)
  library(gdxrrw)
  library(patchwork)

  options(device = function(...) grDevices::pdf(file = NULL, ...))

  for (gams_sys_dir in c("C:/GAMS/38", "C:/GAMS/win64/26.1")) {
    if (dir.exists(gams_sys_dir)) {
      igdx(gams_sys_dir)
      break
    }
  }

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

  psm_main_dir <- "\\\\SY_3600_01\\atfs02\\Individual\\hsakata\\AIMPSY\\output\\global_softlink_Sc\\Global\\2606171718\\main"
  aimt_main_dir <- "\\\\SY_3600_01\\atfs02\\Individual\\hsakata\\AIMEnduse\\output\\softlink_Sc\\2606111707\\main"
  psm_primary_dir <- file.path(dirname(psm_main_dir), "gams_output", "gdx_primary")
  r272_map_path <- "\\\\SY_3600_01\\atfs02\\Individual\\hsakata\\AIMPSY\\AIMPSY-master\\data\\set\\M_R272.csv"

  target_scenarios <- c("NPi500-ZF2050")
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

  biomass_supply_variables <- list(
    c("Prm_Ene_Bio_wo_CCS", "Prm_Ene_Bio_w_CCS"),
    "Prm_Ene_Bio"
  )
  biomass_demand_variables <- list(
    c("Sec_Ene_Ele_Bio_wo_CCS", "Sec_Ene_Ele_Bio_w_CCS"),
    "Sec_Ene_Ele_Bio",
    c("Fin_Ene_SolidsBio", "Fin_Ene_Liq_Bio")
  )

  as_gdx_path <- function(path) {
    normalizePath(path, winslash = "\\", mustWork = FALSE)
  }

  drop_r31_region_rows <- function(df) {
    df %>%
      mutate(RawRegion = as.character(Sr)) %>%
      filter(!str_detect(RawRegion, "^R31-"))
  }

  read_iamc_gdx <- function(main_dir, model_name) {
    gdx_path <- as_gdx_path(file.path(main_dir, "merged_output.gdx"))
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

  report_missing_variable_groups <- function(df, model_name, variable_groups, value_label) {
    for (scenario_name in target_scenarios) {
      has_group <- vapply(variable_groups, function(variables) {
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
          "No biomass ",
          value_label,
          " variables found in ",
          model_name,
          " iamc_gdx / scenario ",
          scenario_name,
          ". Tried: ",
          paste(vapply(variable_groups, paste, collapse = "+", character(1)), collapse = " or ")
        )
      }
    }
  }

  report_missing_variable_groups(all_data, "AIM-Technology", biomass_supply_variables, "supply")
  report_missing_variable_groups(all_data, "AIM-Technology", biomass_demand_variables, "demand")

  biomass_spec <- tibble(
    Fuel = "Biomass",
    LinkPattern = "^R31-[A-Z0-9]+-BMS_R31-[A-Z0-9]+-BMS$",
    NodeSuffixPattern = "-BMS$"
  )

  read_biomass_flow_symbol <- function(scenario_name, symbol, base_direction) {
    gdx_path <- as_gdx_path(file.path(psm_primary_dir, scenario_name, paste0(plot_year, ".gdx")))
    if (!file.exists(gdx_path)) {
      warning("Missing biomass flow GDX file: ", gdx_path)
      return(tibble())
    }

    rgdx.param(gdx_path, symbol) %>%
      mutate(across(where(is.factor), as.character)) %>%
      filter(str_detect(L, biomass_spec$LinkPattern)) %>%
      group_by(L) %>%
      summarise(Value = sum(.data[[symbol]], na.rm = TRUE) * time_slice_hours * gwh_to_ej, .groups = "drop") %>%
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

  for (scenario_name in target_scenarios) {
    has_transport <- biomass_edge_data %>%
      filter(Scenario == scenario_name, Value > 0) %>%
      nrow() > 0

    if (!has_transport) {
      warning("No regional biomass transport links found for scenario ", scenario_name, ".")
    }
  }

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

  biomass_balance_data <- bind_rows(lapply(target_scenarios, function(scenario_name) {
    bind_rows(lapply(region_list, function(region_name) {
      tibble(
        Scenario = scenario_name,
        Region = region_name,
        Supply = get_region_variable_value(all_data, biomass_supply_variables, "AIM-Technology", scenario_name, region_name),
        Demand = get_region_variable_value(all_data, biomass_demand_variables, "AIM-Technology", scenario_name, region_name)
      )
    }))
  })) %>%
    left_join(biomass_trade_data, by = c("Scenario", "Region")) %>%
    mutate(
      across(c(Supply, Demand, Import, Export), ~replace_na(.x, 0)),
      Scenario = factor(Scenario, levels = target_scenarios),
      Region = factor(Region, levels = region_list),
      TotalAvailable = Supply + Import,
      ExportYMin = pmax(TotalAvailable - Export, 0),
      ExportYMax = TotalAvailable
    )

  source_levels <- c("Domestic supply", region_base)
  source_colors <- c(
    "Domestic supply" = "#7fbf3f",
    setNames(grDevices::hcl.colors(length(region_base), palette = "Dark 3"), region_base)
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
      legend.text = element_text(size = 6.1),
      legend.key.height = unit(2.2, "mm"),
      legend.key.width = unit(3.3, "mm"),
      plot.title = element_text(size = 8.2, hjust = 0.5, face = "bold"),
      plot.margin = margin(1, 1, 1, 1, unit = "mm")
    )

  make_biomass_bar_data <- function(balance_row, imports) {
    domestic <- balance_row %>%
      transmute(
        Scenario,
        Region,
        SourceRegion = "Domestic supply",
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
        SourceRegion = factor(SourceRegion, levels = source_levels),
        SourceOrder = match(as.character(SourceRegion), source_levels)
      ) %>%
      arrange(SourceOrder) %>%
      mutate(
        ymin = lag(cumsum(Value), default = 0),
        ymax = cumsum(Value),
        ymid = (ymin + ymax) / 2
      )
  }

  plot_biomass_balance_region <- function(balance_df, import_df, scenario_name, region_name, show_legend = FALSE) {
    bar_width <- 0.62

    d <- balance_df %>%
      filter(
        Scenario == scenario_name,
        Region == region_name
      )

    bar_data <- make_biomass_bar_data(d, import_df)
    export_data <- d %>% filter(Export > 0, ExportYMax > ExportYMin)

    y_max <- max(c(d$TotalAvailable, d$Demand, 0), na.rm = TRUE)
    label_threshold <- if (is.finite(y_max) && y_max > 0) y_max * 0.08 else Inf
    label_data <- bar_data %>%
      filter(as.character(SourceRegion) != "Domestic supply", Value >= label_threshold)

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

    if (nrow(label_data) > 0) {
      p <- p +
        geom_text(
          data = label_data,
          aes(x = 1, y = ymid, label = SourceRegion),
          inherit.aes = FALSE,
          size = 1.8,
          color = "white",
          fontface = "bold"
        )
    }

    export_label_data <- export_data %>%
      mutate(
        ymid = (ExportYMin + ExportYMax) / 2,
        Label = "Export"
      ) %>%
      filter(Export >= label_threshold)

    if (nrow(export_label_data) > 0) {
      p <- p +
        geom_text(
          data = export_label_data,
          aes(x = 1 + bar_width / 2 + 0.03, y = ymid, label = Label),
          inherit.aes = FALSE,
          size = 1.8,
          color = "grey20",
          fontface = "bold",
          hjust = 0
        )
    }

    p <- p +
      geom_point(
        data = d,
        aes(x = 1, y = Demand, shape = "AIM-Tech demand"),
        inherit.aes = FALSE,
        size = 2.2,
        stroke = 0.4,
        fill = "white",
        color = "black"
      ) +
      scale_fill_manual(values = source_colors, drop = FALSE) +
      scale_shape_manual(values = c("AIM-Tech demand" = 23), name = NULL) +
      scale_x_continuous(breaks = 1, labels = "Biomass", limits = c(0.45, 1.55)) +
      expand_limits(y = 0)

    if (region_name != "World") {
      p <- p + coord_cartesian(ylim = c(0, 30))
    }

    p +
      labs(
        y = expression(paste("Biomass balance (EJ ", {yr^-1}, ")")),
        title = region_name
      ) +
      plot_theme +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 6.2),
        legend.position = if (show_legend) "bottom" else "none"
      ) +
      guides(
        fill = guide_legend(nrow = 3, byrow = TRUE, order = 1),
        alpha = if (nrow(export_data) > 0) guide_legend(order = 2, override.aes = list(fill = "grey20", color = NA)) else "none",
        shape = guide_legend(order = 3, override.aes = list(fill = "white", color = "black"))
      )
  }

  make_all_region_biomass_bar_data <- function(balance_df, import_df, scenario_name) {
    domestic <- balance_df %>%
      filter(
        Scenario == scenario_name,
        Region %in% region_base
      ) %>%
      transmute(
        Scenario = as.character(Scenario),
        Region = as.character(Region),
        SourceRegion = "Domestic supply",
        Value = Supply
      )

    incoming <- import_df %>%
      filter(
        Scenario == scenario_name,
        Region %in% region_base
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
        Region = factor(Region, levels = region_base),
        SourceRegion = factor(SourceRegion, levels = source_levels),
        SourceOrder = match(as.character(SourceRegion), source_levels)
      ) %>%
      arrange(Region, SourceOrder) %>%
      group_by(Region) %>%
      mutate(
        ymin = lag(cumsum(Value), default = 0),
        ymax = cumsum(Value),
        ymid = (ymin + ymax) / 2,
        RegionTotal = sum(Value, na.rm = TRUE)
      ) %>%
      ungroup()
  }

  plot_biomass_balance_all_regions <- function(balance_df, import_df, scenario_name) {
    bar_width <- 0.72

    d <- balance_df %>%
      filter(
        Scenario == scenario_name,
        Region %in% region_base
      ) %>%
      mutate(
        Region = factor(Region, levels = region_base),
        RegionX = as.numeric(Region)
      )

    bar_data <- make_all_region_biomass_bar_data(balance_df, import_df, scenario_name) %>%
      mutate(RegionX = as.numeric(Region))

    export_data <- d %>%
      filter(Export > 0, ExportYMax > ExportYMin)

    y_max <- max(c(d$TotalAvailable, d$Demand, 0), na.rm = TRUE)
    label_threshold <- if (is.finite(y_max) && y_max > 0) y_max * 0.035 else Inf
    label_data <- bar_data %>%
      filter(
        as.character(SourceRegion) != "Domestic supply",
        Value >= label_threshold
      )

    export_label_data <- export_data %>%
      mutate(
        ymid = (ExportYMin + ExportYMax) / 2,
        Label = "Export"
      ) %>%
      filter(Export >= label_threshold)

    ggplot() +
      geom_rect(
        data = bar_data,
        aes(
          xmin = RegionX - bar_width / 2,
          xmax = RegionX + bar_width / 2,
          ymin = ymin,
          ymax = ymax,
          fill = SourceRegion
        ),
        color = "white",
        linewidth = 0.08,
        inherit.aes = FALSE
      ) +
      geom_rect(
        data = export_data,
        aes(
          xmin = RegionX - bar_width / 2,
          xmax = RegionX + bar_width / 2,
          ymin = ExportYMin,
          ymax = ExportYMax,
          alpha = "Export"
        ),
        inherit.aes = FALSE,
        fill = "grey20",
        color = NA
      ) +
      geom_text(
        data = label_data,
        aes(x = RegionX, y = ymid, label = SourceRegion),
        inherit.aes = FALSE,
        size = 2.3,
        color = "white",
        fontface = "bold",
        angle = 90
      ) +
      geom_text(
        data = export_label_data,
        aes(x = RegionX + bar_width / 2 + 0.05, y = ymid, label = Label),
        inherit.aes = FALSE,
        size = 2.2,
        color = "grey20",
        fontface = "bold",
        angle = 90,
        hjust = 0.5
      ) +
      geom_point(
        data = d,
        aes(x = RegionX, y = Demand, shape = "AIM-Tech demand"),
        inherit.aes = FALSE,
        size = 2.1,
        stroke = 0.4,
        fill = "white",
        color = "black"
      ) +
      scale_fill_manual(values = source_colors, drop = FALSE) +
      scale_alpha_manual(values = c("Export" = 0.28), name = NULL) +
      scale_shape_manual(values = c("AIM-Tech demand" = 23), name = NULL) +
      scale_x_continuous(
        breaks = seq_along(region_base),
        labels = region_base,
        limits = c(0.35, length(region_base) + 0.75),
        expand = expansion(mult = c(0, 0.01))
      ) +
      expand_limits(y = 0) +
      labs(
        y = expression(paste("Biomass balance (EJ ", {yr^-1}, ")")),
        title = paste0("Biomass balance by import source across regions: ", scenario_name, ", ", plot_year)
      ) +
      theme_bw(base_size = 10) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.14, color = "grey88"),
        panel.border = element_rect(linewidth = 0.28, color = "grey55", fill = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 9.5),
        legend.key.width = unit(5.5, "mm"),
        legend.key.height = unit(4, "mm"),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        plot.margin = margin(2, 2, 2, 2, unit = "mm")
      ) +
      guides(
        fill = guide_legend(nrow = 3, byrow = TRUE, order = 1),
        alpha = guide_legend(order = 2, override.aes = list(fill = "grey20", color = NA)),
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

  save_all_region_plot <- function(plot, filename) {
    dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
    ggsave(
      filename = filename,
      plot = plot,
      width = 640,
      height = 320,
      units = "mm",
      dpi = 300,
      bg = "white"
    )
  }

  for (scenario_name in target_scenarios) {
    out_dir <- file.path(base_dir, scenario_name, "plot_bio")

    for (page_idx in seq_along(region_pages)) {
      page_regions <- region_pages[[page_idx]]
      plots <- lapply(seq_along(page_regions), function(i) {
        plot_biomass_balance_region(
          biomass_balance_data,
          biomass_import_source_data,
          scenario_name,
          page_regions[[i]],
          show_legend = FALSE
        )
      })

      page_plot <- wrap_plots(plots, ncol = 4) +
        plot_annotation(
          title = paste0("Biomass balance by import source: ", scenario_name, ", ", plot_year, " (page ", page_idx, " / ", length(region_pages), ")")
        )

      save_plot_page(
        plot = page_plot,
        filename = file.path(out_dir, paste0(scenario_name, "_biomass_balance_page_", page_idx, ".png"))
      )
    }

    all_regions_plot <- plot_biomass_balance_all_regions(
      biomass_balance_data,
      biomass_import_source_data,
      scenario_name
    )

    save_all_region_plot(
      plot = all_regions_plot,
      filename = file.path(out_dir, paste0(scenario_name, "_biomass_balance_all_regions.png"))
    )
  }
