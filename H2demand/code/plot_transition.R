# PLOT SETTINGS ---------------------------------------------------------------

PSM_SCENARIO <- "global_softlink_Sc"
PSM_REGION <- "Global"
PSM_DATE <- "2609041400"

target_scenarios <- c("NPi500","NPi500_H2")
target_years <- seq(2005, 2050, by = 5)

# PACKAGES AND SCRIPT LOCATION ------------------------------------------------

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

# DERIVED SETTINGS AND INPUT PATHS --------------------------------------------

if (length(target_scenarios) != 2L || anyNA(target_scenarios) || any(!nzchar(target_scenarios))) {
  stop("target_scenarios must contain exactly two non-blank scenario names.")
}

comparison_name <- paste(target_scenarios, collapse = "_vs_")
comparison_groups <- comparison_name

source(file.path(project_dir, "code", "dataframe.R"))

psm_output_root <- "//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output"
psm_main_dir <- file.path(
  psm_output_root,
  PSM_SCENARIO,
  PSM_REGION,
  PSM_DATE,
  "main"
)

base_dir <- file.path(
  project_dir,
  "output",
  paste(PSM_SCENARIO, PSM_DATE, comparison_name, sep = "_")
)

region_list <- c(
  "World",
  "JPN", "CHN", "IND", "IDN", "KOR", "THA", "MYS",
  "VNM", "XSE", "XSA", "XEA", "XCS", "XME", "AUS",
  "XOC", "CAN", "USA", "XE15", "XE10", "XE3", "TUR",
  "XEWI", "XEEI", "XENI", "RUS", "MEX", "BRA", "XLM",
  "ZAF", "XAF", "XNF"
)
region_base <- setdiff(region_list, "World")
regions_per_page <- 4
region_pages <- split(region_list, ceiling(seq_along(region_list) / regions_per_page))

model_levels <- target_scenarios
model_labels <- setNames(target_scenarios, target_scenarios)
model_colors <- setNames(c("black", "#e31a1c"), target_scenarios)

drop_r31_region_rows <- function(df) {
  df %>%
    mutate(RawRegion = as.character(Sr)) %>%
    filter(!str_detect(RawRegion, "^R31-"))
}

# IAMC DATA PREPARATION -------------------------------------------------------

read_iamc_gdx <- function(main_dir, source_scenario) {
  gdx_path <- file.path(main_dir, "merged_output.gdx")
  if (!file.exists(gdx_path)) {
    stop("Missing GDX file: ", gdx_path)
  }

  rgdx.param(gdx_path, "iamc_gdx") %>%
    filter(as.character(Sc) == source_scenario) %>%
    drop_r31_region_rows() %>%
    transmute(
      Scenario = comparison_name,
      Region = RawRegion,
      Variable = as.character(Sv),
      Year = as.numeric(as.character(Y5)),
      Value = suppressWarnings(as.numeric(as.character(iamc_gdx))),
      Model = source_scenario
    ) %>%
    filter(!is.na(Value)) %>%
    group_by(Scenario, Region, Variable, Year, Model) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")
}

add_world_rows <- function(df, regions, replace_existing = TRUE) {
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

scenario_data <- bind_rows(lapply(target_scenarios, function(source_scenario) {
  read_iamc_gdx(psm_main_dir, source_scenario)
}))

all_data <- scenario_data %>%
  filter(
    Scenario %in% comparison_groups,
    Year %in% target_years,
    Region %in% region_base
  ) %>%
  add_world_rows(region_base, replace_existing = TRUE) %>%
  filter(Region %in% region_list) %>%
  mutate(
    Scenario = factor(Scenario, levels = comparison_groups),
    Region = factor(Region, levels = region_list),
    Model = factor(Model, levels = model_levels)
  )

# TRANSITION DATA PREPARATION -------------------------------------------------

power_spec <- secele %>%
  mutate(Variable = as.character(Variable))
power_colors <- setNames(as.character(power_spec$Color), power_spec$Variable)
power_labels <- setNames(as.character(power_spec$Legend), power_spec$Variable)

power_transition_data <- all_data %>%
  filter(Variable %in% power_spec$Variable) %>%
  group_by(Scenario, Region, Model, Year, Variable) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Variable = factor(Variable, levels = rev(power_spec$Variable)),
    Region = factor(Region, levels = region_list),
    Model = factor(Model, levels = model_levels)
  )

capacity_spec <- tribble(
  ~Variable,          ~Technology,      ~Color,
  "Cap_Ele_Sto_Bat",  "Battery storage", "#23e893",
  "Cap_Ele_Solar",    "Solar",           "lightsalmon",
  "Cap_Ele_Win",      "Wind",            "lightskyblue3",
  "Cap_Hyd_Ele",      "Electrolyzer",    "orchid"
)
capacity_colors <- setNames(as.character(capacity_spec$Color), capacity_spec$Technology)

capacity_transition_data <- all_data %>%
  filter(Variable %in% capacity_spec$Variable) %>%
  left_join(capacity_spec %>% select(Variable, Technology), by = "Variable") %>%
  group_by(Scenario, Region, Model, Year, Technology) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Technology = factor(Technology, levels = capacity_spec$Technology),
    Region = factor(Region, levels = region_list),
    Model = factor(Model, levels = model_levels)
  )

# REGIONAL PANEL THEMES AND PLOT FUNCTIONS ------------------------------------

plot_theme <- theme_bw(base_size = 9) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.12, color = "grey88"),
    panel.border = element_rect(linewidth = 0.25, color = "grey55", fill = NA),
    strip.background = element_rect(fill = "white", color = "grey75", linewidth = 0.25),
    strip.text = element_text(size = 8, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 9),
    axis.text.x = element_text(size = 7, angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 7),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.key.height = unit(3.2, "mm"),
    legend.key.width = unit(5.0, "mm"),
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
    plot.margin = margin(2, 2, 2, 2, unit = "mm")
  )

compact_theme <- theme_bw(base_size = 8.2) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.12, color = "grey88"),
    panel.border = element_rect(linewidth = 0.25, color = "grey55", fill = NA),
    strip.background = element_rect(fill = "white", color = "grey75", linewidth = 0.25),
    strip.text = element_text(size = 7.2, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 7.4),
    axis.text.x = element_text(size = 6.2, angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 6.2),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 6.6),
    legend.key.height = unit(2.2, "mm"),
    legend.key.width = unit(3.6, "mm"),
    plot.title = element_text(size = 8.2, hjust = 0.5, face = "bold"),
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

complete_power_region_data <- function(df, scenario_name, region_name) {
  template <- expand_grid(
    Scenario = scenario_name,
    Region = region_name,
    Model = model_levels,
    Year = target_years,
    Variable = power_spec$Variable
  )

  template %>%
    left_join(
      df %>%
        filter(Scenario == scenario_name, Region == region_name) %>%
        mutate(
          Scenario = as.character(Scenario),
          Region = as.character(Region),
          Model = as.character(Model),
          Variable = as.character(Variable)
        ),
      by = c("Scenario", "Region", "Model", "Year", "Variable")
    ) %>%
    mutate(
      Value = replace_na(Value, 0),
      Model = factor(Model, levels = model_levels, labels = model_labels),
      Variable = factor(Variable, levels = rev(power_spec$Variable))
    )
}

complete_capacity_region_data <- function(df, scenario_name, region_name) {
  template <- expand_grid(
    Scenario = scenario_name,
    Region = region_name,
    Model = model_levels,
    Year = target_years,
    Technology = capacity_spec$Technology
  )

  template %>%
    left_join(
      df %>%
        filter(Scenario == scenario_name, Region == region_name) %>%
        mutate(
          Scenario = as.character(Scenario),
          Region = as.character(Region),
          Model = as.character(Model),
          Technology = as.character(Technology)
        ),
      by = c("Scenario", "Region", "Model", "Year", "Technology")
    ) %>%
    mutate(
      Value = replace_na(Value, 0),
      Model = factor(Model, levels = model_levels),
      Technology = factor(Technology, levels = capacity_spec$Technology)
    )
}

plot_power_transition_region <- function(df, scenario_name, region_name) {
  d <- complete_power_region_data(df, scenario_name, region_name)

  ggplot(d, aes(x = Year, y = Value, fill = Variable)) +
    geom_area(linewidth = 0.05, color = "white") +
    scale_fill_manual(values = power_colors, labels = power_labels, drop = FALSE) +
    scale_x_continuous(breaks = target_years) +
    expand_limits(y = 0) +
    facet_wrap(~Model, nrow = 1, scales = "fixed", drop = FALSE) +
    labs(
      y = expression(paste("Power generation (EJ ", {yr^-1}, ")")),
      title = "Power generation mix"
    ) +
    compact_theme +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE, reverse = FALSE))
}

plot_capacity_transition_region <- function(df, scenario_name, region_name, technology_name) {
  d <- complete_capacity_region_data(df, scenario_name, region_name) %>%
    filter(Technology == technology_name)

  ggplot(
    d,
    aes(
      x = Year,
      y = Value,
      color = Model,
      linetype = Model,
      shape = Model,
      group = Model
    )
  ) +
    geom_line(linewidth = 0.42) +
    geom_point(size = 1.45, stroke = 0.35) +
    scale_color_manual(values = model_colors, drop = FALSE) +
    scale_x_continuous(breaks = target_years) +
    expand_limits(y = 0) +
    labs(
      y = "Capacity (GW)",
      title = technology_name
    ) +
    compact_theme +
    theme(
      legend.position = "bottom",
      legend.box = "vertical"
    ) +
    guides(
      color = guide_legend(nrow = 1, byrow = TRUE),
      linetype = "none",
      shape = "none"
    )
}

make_region_row <- function(scenario_name, region_name) {
  (
    blank_region_label(region_name) |
      plot_power_transition_region(power_transition_data, scenario_name, region_name) |
      plot_capacity_transition_region(capacity_transition_data, scenario_name, region_name, "Battery storage") |
      plot_capacity_transition_region(capacity_transition_data, scenario_name, region_name, "Solar") |
      plot_capacity_transition_region(capacity_transition_data, scenario_name, region_name, "Wind") |
      plot_capacity_transition_region(capacity_transition_data, scenario_name, region_name, "Electrolyzer")
  ) +
    plot_layout(widths = c(0.20, 1.38, 0.62, 0.62, 0.62, 0.62))
}

plot_transition_page <- function(scenario_name, page_regions, page_idx, total_pages) {
  rows <- lapply(page_regions, function(region_name) {
    make_region_row(scenario_name, region_name)
  })

  page_body <- patchwork::patchworkGrob(wrap_plots(rows, ncol = 1))
  wrap_elements(full = page_body) +
    plot_annotation(
      title = paste0("Power transition summary: ", scenario_name, " (page ", page_idx, " / ", total_pages, ")")
    )
}

save_plot_page <- function(plot, filename) {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  ggsave(
    filename = filename,
    plot = plot,
    width = 760,
    height = 300,
    units = "mm",
    dpi = 300,
    bg = "white"
  )
}

# GENERATE AND SAVE PLOTS -----------------------------------------------------

for (scenario_name in comparison_groups) {
  scenario_data <- power_transition_data %>%
    filter(Scenario == scenario_name)

  if (nrow(scenario_data) == 0) {
    warning("No power generation transition data found for scenario ", scenario_name, ".")
    next
  }

  out_dir <- file.path(base_dir, scenario_name, "plot_transition")

  for (page_idx in seq_along(region_pages)) {
    page_plot <- plot_transition_page(
      scenario_name = scenario_name,
      page_regions = region_pages[[page_idx]],
      page_idx = page_idx,
      total_pages = length(region_pages)
    )

    save_plot_page(
      plot = page_plot,
      filename = file.path(
        out_dir,
        paste0(scenario_name, "_power_transition_summary_page_", page_idx, ".png")
      )
    )
  }
}
