  # CCS storage potential and use plots ----------------------------------------

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
  psm_primary_dir <- file.path(dirname(psm_main_dir), "gams_output", "gdx_primary")
  r272_map_path <- "\\\\SY_3600_01\\atfs02\\Individual\\hsakata\\AIMPSY\\AIMPSY-master\\data\\set\\M_R272.csv"

  target_scenarios <- c("NPi500-ZF2050")
  plot_year <- 2050
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

  as_gdx_path <- function(path) {
    normalizePath(path, winslash = "\\", mustWork = FALSE)
  }

  normalize_node <- function(node_name) {
    node_name %>%
      str_remove_all(fixed("-AC")) %>%
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
          N_IND = normalize_node(as.character(N_IND)),
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
    node_name <- normalize_node(node_name)
    fallback <- infer_node_region(node_name)
    if (nrow(r272_region_map) == 0) {
      return(fallback)
    }

    mapped <- r272_region_map$R31[match(node_name, r272_region_map$N_IND)]
    if_else(is.na(mapped) | mapped == "", fallback, mapped)
  }

  storage_kind_levels <- c("Storage use")
  storage_kind_colors <- c(
    "Storage use" = "#238b45"
  )

  get_primary_gdx_path <- function(scenario_name) {
    as_gdx_path(file.path(psm_primary_dir, scenario_name, paste0(plot_year, ".gdx")))
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
      stop("Missing primary GDX file: ", gdx_path)
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
      warning("No CCS storage potential rows found in pmax(*,'SCO2') for scenario ", scenario_name, ".")
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
      stop("Missing primary GDX file: ", gdx_path)
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
      warning("No CCS storage use rows found in vp_l for scenario ", scenario_name, ".")
      return(tibble(Scenario = character(), Region = character(), StorageKind = character(), Value = numeric()))
    }

    use_data %>%
      add_world_storage_rows() %>%
      mutate(
        Region = factor(Region, levels = region_list),
        StorageKind = factor(StorageKind, levels = storage_kind_levels)
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
      Utilization = if_else(Potential > 0, Use / Potential, NA_real_),
      Scenario = factor(Scenario, levels = target_scenarios),
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

  stack_bounds <- function(df, x_col = NULL) {
    out <- df %>%
      mutate(StorageOrder = match(as.character(StorageKind), storage_kind_levels)) %>%
      arrange(StorageOrder)

    if (is.null(x_col)) {
      out %>%
        mutate(
          ymin = lag(cumsum(Value), default = 0),
          ymax = cumsum(Value),
          ymid = (ymin + ymax) / 2
        )
    } else {
      out %>%
        group_by(.data[[x_col]]) %>%
        mutate(
          ymin = lag(cumsum(Value), default = 0),
          ymax = cumsum(Value),
          ymid = (ymin + ymax) / 2
        ) %>%
        ungroup()
    }
  }

  plot_ccs_storage_region <- function(potential_df, use_df, summary_df, scenario_name, region_name) {
    bar_width <- 0.62

    use_bar <- use_df %>%
      filter(Scenario == scenario_name, Region == region_name) %>%
      stack_bounds()

    if (nrow(use_bar) == 0) {
      use_bar <- tibble(
        StorageKind = factor("Storage use", levels = storage_kind_levels),
        Value = 0,
        ymin = 0,
        ymax = 0,
        ymid = 0
      )
    }

    summary_row <- summary_df %>%
      filter(Scenario == scenario_name, Region == region_name)

    potential_max <- max(c(summary_row$Potential, 0), na.rm = TRUE)
    use_max <- max(c(summary_row$Use, use_bar$Value, 0), na.rm = TRUE)
    axis_scale <- if (is.finite(potential_max) && is.finite(use_max) && potential_max > 0 && use_max > 0) {
      potential_max / use_max
    } else {
      1
    }
    summary_row <- summary_row %>%
      mutate(PotentialPlot = Potential / axis_scale)

    label_threshold <- if (is.finite(use_max) && use_max > 0) use_max * 0.08 else Inf
    use_labels <- use_bar %>%
      filter(Value >= label_threshold)

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

    if (nrow(use_labels) > 0) {
      p <- p +
        geom_text(
          data = use_labels,
          aes(x = 1, y = ymid, label = "Use"),
          inherit.aes = FALSE,
          size = 1.8,
          color = "white",
          fontface = "bold"
        )
    }

    p +
      geom_point(
        data = summary_row,
        aes(x = 1, y = PotentialPlot, shape = "Storage potential"),
        inherit.aes = FALSE,
        size = 3.2,
        stroke = 0.65,
        fill = "white",
        color = "black"
      ) +
      scale_fill_manual(values = storage_kind_colors, drop = FALSE) +
      scale_shape_manual(values = c("Storage potential" = 23), name = NULL) +
      scale_x_continuous(breaks = 1, labels = "CCS storage", limits = c(0.45, 1.55)) +
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.08)),
        sec.axis = sec_axis(~ . * axis_scale, name = expression(paste("Storage potential (MtCO"[2], " ", {yr^-1}, ")")))
      ) +
      labs(
        y = expression(paste("Storage use (MtCO"[2], " ", {yr^-1}, ")")),
        title = region_name
      ) +
      plot_theme +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 6.2),
        legend.position = "none"
      )
  }

  plot_ccs_storage_all_regions <- function(potential_df, use_df, summary_df, scenario_name) {
    bar_width <- 0.72

    use_bar <- use_df %>%
      filter(Scenario == scenario_name, Region %in% region_base) %>%
      mutate(
        Region = factor(Region, levels = region_base),
        RegionX = as.numeric(Region)
      ) %>%
      stack_bounds("Region")

    summary_plot <- summary_df %>%
      filter(Scenario == scenario_name, Region %in% region_base) %>%
      mutate(
        Region = factor(Region, levels = region_base),
        RegionX = as.numeric(Region)
      )

    potential_max <- max(c(summary_plot$Potential, 0), na.rm = TRUE)
    use_max <- max(c(summary_plot$Use, use_bar$Value, 0), na.rm = TRUE)
    axis_scale <- if (is.finite(potential_max) && is.finite(use_max) && potential_max > 0 && use_max > 0) {
      potential_max / use_max
    } else {
      1
    }
    summary_plot <- summary_plot %>%
      mutate(PotentialPlot = Potential / axis_scale)

    label_threshold <- if (is.finite(use_max) && use_max > 0) use_max * 0.035 else Inf
    use_labels <- use_bar %>%
      filter(Value >= label_threshold)

    ggplot() +
      geom_rect(
        data = use_bar,
        aes(
          xmin = RegionX - bar_width / 2,
          xmax = RegionX + bar_width / 2,
          ymin = ymin,
          ymax = ymax,
          fill = StorageKind
        ),
        color = "white",
        linewidth = 0.08,
        inherit.aes = FALSE
      ) +
      geom_text(
        data = use_labels,
        aes(x = RegionX, y = ymid, label = "Use"),
        inherit.aes = FALSE,
        size = 2.2,
        color = "white",
        fontface = "bold",
        angle = 90
      ) +
      geom_point(
        data = summary_plot,
        aes(x = RegionX, y = PotentialPlot, shape = "Storage potential"),
        inherit.aes = FALSE,
        size = 3.0,
        stroke = 0.65,
        fill = "white",
        color = "black"
      ) +
      scale_fill_manual(values = storage_kind_colors, drop = FALSE) +
      scale_shape_manual(values = c("Storage potential" = 23), name = NULL) +
      scale_x_continuous(
        breaks = seq_along(region_base),
        labels = region_base,
        limits = c(0.35, length(region_base) + 0.75),
        expand = expansion(mult = c(0, 0.01))
      ) +
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.08)),
        sec.axis = sec_axis(~ . * axis_scale, name = expression(paste("Storage potential (MtCO"[2], " ", {yr^-1}, ")")))
      ) +
      labs(
        y = expression(paste("Storage use (MtCO"[2], " ", {yr^-1}, ")")),
        title = paste0("CCS storage potential and use across regions: ", scenario_name, ", ", plot_year)
      ) +
      theme_bw(base_size = 10) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.14, color = "grey88"),
        panel.border = element_rect(linewidth = 0.28, color = "grey55", fill = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.title.y.right = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10),
        axis.text.y.right = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 9.5),
        legend.key.width = unit(5.5, "mm"),
        legend.key.height = unit(4, "mm"),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        plot.margin = margin(2, 2, 2, 2, unit = "mm")
      ) +
      guides(
        fill = guide_legend(nrow = 2, byrow = TRUE, order = 1),
        shape = guide_legend(order = 2, override.aes = list(fill = "white", color = "black"))
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
    out_dir <- file.path(base_dir, scenario_name, "plot_CCS")

    for (page_idx in seq_along(region_pages)) {
      page_regions <- region_pages[[page_idx]]
      plots <- lapply(seq_along(page_regions), function(i) {
        plot_ccs_storage_region(
          ccs_potential_data,
          ccs_use_data,
          ccs_summary_data,
          scenario_name,
          page_regions[[i]]
        )
      })

      page_plot <- wrap_plots(plots, ncol = 4) +
        plot_annotation(
          title = paste0("CCS storage potential and use: ", scenario_name, ", ", plot_year, " (page ", page_idx, " / ", length(region_pages), ")")
        )

      save_plot_page(
        plot = page_plot,
        filename = file.path(out_dir, paste0(scenario_name, "_CCS_storage_page_", page_idx, ".png"))
      )
    }

    all_regions_plot <- plot_ccs_storage_all_regions(
      ccs_potential_data,
      ccs_use_data,
      ccs_summary_data,
      scenario_name
    )

    save_all_region_plot(
      plot = all_regions_plot,
      filename = file.path(out_dir, paste0(scenario_name, "_CCS_storage_all_regions.png"))
    )
  }
