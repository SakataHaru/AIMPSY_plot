# PLOT SETTINGS ---------------------------------------------------------------

AIMT_SCENARIO <- "softlink_IntermReport"
AIMT_DATE <- "2609111525"

target_scenarios <- c("500C-ELE+", "500C-ELE++", "500C-ELE+++")
transition_years <- seq(2020, 2050, by = 5)
car_years <- c(2030, 2050)

discount_rate <- 0.05
discount_base_year <- 2020
discount_end_year <- 2050

regions_per_transition_page <- 5
regions_per_sector_page <- 5


# PACKAGES AND PROJECT PATHS --------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(gdxrrw)
  library(tibble)
  library(patchwork)
  library(scales)
})

script_args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_args[grepl("^--file=", script_args)])
script_dir <- if (length(script_path) > 0) {
  dirname(normalizePath(script_path[[1]], winslash = "/", mustWork = FALSE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

project_dir <- if (file.exists(file.path(script_dir, "dataframe.R"))) {
  normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)
} else if (file.exists(file.path(script_dir, "code", "dataframe.R"))) {
  script_dir
} else {
  stop("Could not locate code/dataframe.R from: ", script_dir)
}

source(file.path(project_dir, "code", "dataframe.R"))


# DERIVED SETTINGS AND INPUT PATHS -------------------------------------------

aimt_output_root <- "//SY_3600_01/atfs02/Individual/hsakata/AIMEnduse/output"
aimt_main_dir <- file.path(aimt_output_root, AIMT_SCENARIO, AIMT_DATE, "main")
gdx_path <- file.path(aimt_main_dir, "merged_output.gdx")
plot_root <- file.path(project_dir, "output", "AIMTech")

if (!file.exists(gdx_path)) {
  stop("Missing AIM-Technology GDX file: ", gdx_path)
}

if (
  length(target_scenarios) == 0 ||
    anyNA(target_scenarios) ||
    any(!nzchar(trimws(target_scenarios))) ||
    anyDuplicated(target_scenarios)
) {
  stop("target_scenarios must contain unique, non-blank scenario names.")
}

if (
  !is.numeric(discount_rate) || length(discount_rate) != 1 ||
    is.na(discount_rate) || discount_rate < 0
) {
  stop("discount_rate must be one non-negative numeric value.")
}

region_list <- c(
  "JPN", "CHN", "IND", "IDN", "KOR", "THA", "MYS", "VNM",
  "XSE", "XSA", "XEA", "XCS", "XME", "AUS", "XOC",
  "CAN", "USA", "XE15", "XE10", "XE3", "TUR", "XEWI",
  "XEEI", "XENI", "RUS", "MEX", "BRA", "XLM", "ZAF",
  "XAF", "XNF"
)
region_list_plot <- c(region_list, "World")

scenario_colors <- setNames(
  c(
    emisec$Color[match("Other\ndemand", emisec$Legend)],
    secele$Color[match("Solar", secele$Legend)],
    secele$Color[match("Hydrogen", secele$Legend)]
  ),
  target_scenarios
)
scenario_shapes <- setNames(c(16, 17, 15), target_scenarios)

dir.create(plot_root, recursive = TRUE, showWarnings = FALSE)
invisible(lapply(
  c("secele", "finene", "finene_sec", "emisec", "totcost", "car"),
  function(folder) dir.create(file.path(plot_root, folder), recursive = TRUE, showWarnings = FALSE)
))


# VARIABLE SPECIFICATIONS AND VALIDATION -------------------------------------

required_specs <- c(
  "secele", "finene", "finind", "finbui", "fintra",
  "emisec", "EVshapss", "EVshafre"
)
missing_specs <- required_specs[!vapply(
  required_specs,
  function(spec_name) exists(spec_name, envir = .GlobalEnv, inherits = FALSE),
  logical(1)
)]
if (length(missing_specs) > 0) {
  stop("Missing variable specifications in dataframe.R: ", paste(missing_specs, collapse = ", "))
}

validate_spec <- function(spec, spec_name) {
  required_columns <- c("Variable", "Legend", "Color")
  missing_columns <- setdiff(required_columns, names(spec))
  if (length(missing_columns) > 0) {
    stop(
      "Specification '", spec_name, "' lacks columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  if (anyDuplicated(spec$Variable)) {
    stop("Specification '", spec_name, "' contains duplicated variables.")
  }
  invisible(TRUE)
}

invisible(Map(
  validate_spec,
  mget(required_specs, envir = .GlobalEnv, inherits = FALSE),
  required_specs
))

cost_variable <- "Pol_Cos_Add_Tot_Ene_Sys_Cos"
all_specs <- mget(required_specs, envir = .GlobalEnv, inherits = FALSE)
needed_variables <- unique(c(
  unlist(lapply(all_specs, function(spec) as.character(spec$Variable)), use.names = FALSE),
  cost_variable
))


# READ AND PREPARE AIM-TECHNOLOGY DATA ---------------------------------------

message("Reading AIM-Technology data: ", gdx_path)
iamc_raw <- rgdx.param(
  normalizePath(gdx_path, winslash = "\\", mustWork = TRUE),
  "iamc_gdx"
) %>%
  transmute(
    Scenario = as.character(Sc),
    Region = as.character(Sr),
    Variable = as.character(Sv),
    Year = suppressWarnings(as.integer(as.character(Y5))),
    Value = suppressWarnings(as.numeric(as.character(iamc_gdx)))
  ) %>%
  filter(
    Scenario %in% target_scenarios,
    Region %in% region_list_plot,
    Variable %in% needed_variables,
    !is.na(Year),
    !is.na(Value),
    Year >= min(c(transition_years, car_years, discount_base_year)),
    Year <= max(c(transition_years, car_years, discount_end_year))
  ) %>%
  group_by(Scenario, Region, Variable, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")

missing_scenarios <- setdiff(target_scenarios, unique(iamc_raw$Scenario))
if (length(missing_scenarios) > 0) {
  stop("Target scenarios missing from iamc_gdx: ", paste(missing_scenarios, collapse = ", "))
}

missing_variables <- setdiff(needed_variables, unique(iamc_raw$Variable))
if (length(missing_variables) > 0) {
  warning(
    "The following configured variables have no non-zero records in the selected data: ",
    paste(missing_variables, collapse = ", ")
  )
}

add_world_total <- function(data) {
  regional <- data %>% filter(Region %in% region_list)

  world_fallback <- regional %>%
    filter(Region %in% region_list) %>%
    group_by(Scenario, Variable, Year) %>%
    summarise(FallbackValue = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    mutate(Region = "World")

  world_reported <- data %>%
    filter(Region == "World") %>%
    select(Scenario, Region, Variable, Year, ReportedValue = Value)

  world <- full_join(
    world_reported,
    world_fallback,
    by = c("Scenario", "Region", "Variable", "Year")
  ) %>%
    transmute(
      Scenario,
      Region,
      Variable,
      Year,
      Value = coalesce(ReportedValue, FallbackValue)
    )

  bind_rows(regional, world)
}

prepare_mix_data <- function(spec, years) {
  spec_clean <- spec %>%
    transmute(
      Variable = as.character(Variable),
      Legend = as.character(Legend),
      Color = as.character(Color)
    )

  iamc_raw %>%
    filter(
      Region %in% region_list_plot,
      Variable %in% spec_clean$Variable,
      Year %in% years
    ) %>%
    add_world_total() %>%
    complete(
      Scenario = target_scenarios,
      Region = region_list_plot,
      Variable = spec_clean$Variable,
      Year = years,
      fill = list(Value = 0)
    ) %>%
    left_join(spec_clean, by = "Variable") %>%
    mutate(
      Scenario = factor(Scenario, levels = target_scenarios),
      Region = factor(Region, levels = region_list_plot),
      Legend = factor(Legend, levels = spec_clean$Legend)
    )
}

secele_data <- prepare_mix_data(secele, transition_years)
finene_data <- prepare_mix_data(finene, transition_years)
emisec_data <- prepare_mix_data(emisec, transition_years)


# PLOT THEMES AND OUTPUT HELPERS ---------------------------------------------

base_theme <- theme_bw(base_size = 9) +
  theme(
    panel.border = element_rect(colour = "grey65", fill = NA, linewidth = 0.25),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.25),
    strip.background = element_rect(fill = "white", colour = "grey65", linewidth = 0.25),
    strip.text = element_text(size = 8, face = "bold"),
    axis.text = element_text(size = 7, colour = "black"),
    axis.title = element_text(size = 9),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 8, colour = "grey30"),
    plot.margin = margin(5, 5, 5, 5)
  )

save_png <- function(plot, filename, width, height) {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    units = "mm",
    dpi = 300,
    bg = "white",
    limitsize = FALSE
  )
  message("Saved: ", filename)
  invisible(filename)
}

split_pages <- function(items, page_size) {
  split(items, ceiling(seq_along(items) / page_size))
}

mix_scale <- function(spec) {
  colors <- setNames(as.character(spec$Color), as.character(spec$Legend))
  scale_fill_manual(
    values = colors,
    breaks = as.character(spec$Legend),
    drop = FALSE
  )
}

row_limits <- function(data) {
  data %>%
    group_by(Region, Scenario, Year) %>%
    summarise(
      Positive = sum(Value[Value > 0], na.rm = TRUE),
      Negative = sum(Value[Value < 0], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(Region) %>%
    summarise(
      Lower = min(0, Negative, na.rm = TRUE),
      Upper = max(0, Positive, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(c(Lower, Upper), values_to = "Limit")
}

make_transition_page <- function(
    data,
    spec,
    regions,
    y_label,
    title,
    legend_at_right = FALSE,
    reverse_stack = FALSE,
    show_legend = TRUE,
    add_annotation = TRUE,
    show_panel_titles = TRUE,
    use_remaining_world_space = TRUE) {
  # Keep the final, incomplete page at the same regional-panel scale as the
  # full pages.  World receives the unused regional slots rather than making
  # the sole remaining R31 region expand to half of the page height.
  regular_regions <- setdiff(regions, "World")
  is_incomplete_world_page <-
    use_remaining_world_space &&
    "World" %in% regions &&
    length(regions) < regions_per_transition_page

  if (is_incomplete_world_page) {
    row_regions <- c(regular_regions, "World")
    row_heights <- c(
      rep(1, length(regular_regions)),
      regions_per_transition_page - length(regular_regions)
    )
    regional_rows <- lapply(seq_along(row_regions), function(row_number) {
      make_transition_page(
        data = data,
        spec = spec,
        regions = row_regions[[row_number]],
        y_label = y_label,
        title = title,
        legend_at_right = legend_at_right,
        reverse_stack = reverse_stack,
        show_legend = row_number == length(row_regions),
        add_annotation = FALSE,
        show_panel_titles = FALSE,
        use_remaining_world_space = FALSE
      )
    })

    return(
      wrap_plots(regional_rows, ncol = 1, heights = row_heights) +
        plot_annotation(
          title = title,
          subtitle = paste(target_scenarios, collapse = "  |  ")
        )
    )
  }

  page_data <- data %>%
    filter(as.character(Region) %in% regions) %>%
    mutate(Region = factor(as.character(Region), levels = regions))
  limits <- row_limits(page_data) %>%
    mutate(Region = factor(as.character(Region), levels = regions))

  left <- ggplot(page_data, aes(Year, Value, fill = Legend, group = Legend)) +
    geom_area(
      position = position_stack(reverse = reverse_stack),
      linewidth = 0.12,
      colour = "white"
    ) +
    geom_blank(data = limits, aes(x = min(transition_years), y = Limit), inherit.aes = FALSE) +
    facet_grid(
      rows = vars(Region), cols = vars(Scenario),
      scales = "free_y", switch = "y", drop = FALSE
    ) +
    scale_x_continuous(
      breaks = c(2020, 2030, 2040, 2050),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    mix_scale(spec) +
    labs(
      x = NULL,
      y = y_label,
      title = if (show_panel_titles) "2020-2050 transition" else NULL
    ) +
    base_theme +
    theme(
      strip.placement = "outside",
      strip.text.y.left = element_text(angle = 0, size = 8),
      legend.position = if (show_legend && !legend_at_right) "bottom" else "none"
    )

  comparison_data <- page_data %>% filter(Year == max(transition_years))
  right <- ggplot(comparison_data, aes(Scenario, Value, fill = Legend)) +
    geom_col(
      position = position_stack(reverse = reverse_stack),
      width = 0.78,
      linewidth = 0.12,
      colour = "white"
    ) +
    geom_blank(
      data = limits,
      aes(x = factor(target_scenarios[[1]], levels = target_scenarios), y = Limit),
      inherit.aes = FALSE
    ) +
    facet_grid(rows = vars(Region), scales = "free_y", drop = FALSE) +
    mix_scale(spec) +
    guides(fill = guide_legend(ncol = 1, byrow = TRUE, reverse = reverse_stack)) +
    labs(
      x = NULL,
      y = NULL,
      title = if (show_panel_titles) "2050 comparison" else NULL
    ) +
    base_theme +
    theme(
      strip.text.y = element_blank(),
      strip.background.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(angle = 35, hjust = 1, size = 6.5),
      legend.position = if (show_legend && legend_at_right) "right" else "none",
      legend.direction = "vertical",
      legend.box = "vertical"
    )

  panel <- left + right + plot_layout(widths = c(3.15, 1))
  if (add_annotation) {
    panel + plot_annotation(
      title = title,
      subtitle = paste(target_scenarios, collapse = "  |  ")
    )
  } else {
    panel
  }
}

save_transition_pages <- function(
    data,
    spec,
    folder,
    file_stem,
    y_label,
    title,
    width = 440,
    height = 330,
    legend_at_right = FALSE,
    reverse_stack = FALSE) {
  pages <- split_pages(region_list_plot, regions_per_transition_page)
  for (page_number in seq_along(pages)) {
    p <- make_transition_page(
      data = data,
      spec = spec,
      regions = pages[[page_number]],
      y_label = y_label,
      title = title,
      legend_at_right = legend_at_right,
      reverse_stack = reverse_stack
    )
    save_png(
      p,
      file.path(
        plot_root,
        folder,
        sprintf("%s_page_%02d.png", file_stem, page_number)
      ),
      width = width,
      height = height
    )
  }
}


# 1. SECONDARY ELECTRICITY ----------------------------------------------------

save_transition_pages(
  data = secele_data,
  spec = secele,
  folder = "secele",
  file_stem = "AIMTech_secele_scenario_comparison",
  y_label = "Power generation (EJ/yr)",
  title = "AIM-Technology power generation mix",
  width = 480,
  height = 440,
  legend_at_right = TRUE,
  reverse_stack = TRUE
)


# 2. FINAL ENERGY -------------------------------------------------------------

save_transition_pages(
  data = finene_data,
  spec = finene,
  folder = "finene",
  file_stem = "AIMTech_finene_scenario_comparison",
  y_label = "Final energy demand (EJ/yr)",
  title = "AIM-Technology final energy mix",
  width = 480,
  height = 440,
  legend_at_right = TRUE,
  reverse_stack = TRUE
)


# 3. SECTORAL FINAL ENERGY IN 2050 -------------------------------------------

sector_specs <- list(
  Industry = finind,
  Buildings = finbui,
  Transport = fintra
)

sector_data <- bind_rows(lapply(names(sector_specs), function(sector_name) {
  prepare_mix_data(sector_specs[[sector_name]], max(transition_years)) %>%
    mutate(Sector = sector_name)
})) %>%
  mutate(Sector = factor(Sector, levels = names(sector_specs)))

sector_legend_order <- unique(c(
  as.character(finind$Legend),
  as.character(finbui$Legend),
  as.character(fintra$Legend)
))
sector_colors <- bind_rows(sector_specs) %>%
  transmute(Legend = as.character(Legend), Color = as.character(Color)) %>%
  distinct(Legend, .keep_all = TRUE) %>%
  { setNames(.$Color, .$Legend) }

make_sector_page <- function(
    page_regions,
    show_legend = TRUE,
    add_annotation = TRUE) {
  page_data <- sector_data %>%
    filter(as.character(Region) %in% page_regions) %>%
    mutate(
      Region = factor(as.character(Region), levels = page_regions),
      Legend = factor(as.character(Legend), levels = sector_legend_order)
    )

  p <- ggplot(page_data, aes(Scenario, Value, fill = Legend)) +
    geom_col(
      position = position_stack(reverse = TRUE),
      width = 0.78,
      linewidth = 0.12,
      colour = "white"
    ) +
    facet_grid(
      rows = vars(Region), cols = vars(Sector),
      scales = "free_y", switch = "y", drop = FALSE
    ) +
    scale_fill_manual(
      values = sector_colors,
      breaks = sector_legend_order,
      drop = FALSE
    ) +
    guides(fill = guide_legend(ncol = 1, byrow = TRUE, reverse = TRUE)) +
    labs(
      x = NULL,
      y = "Final energy demand (EJ/yr)",
      title = if (add_annotation) {
        "AIM-Technology sectoral final energy demand in 2050"
      } else {
        NULL
      },
      subtitle = if (add_annotation) {
        paste(target_scenarios, collapse = "  |  ")
      } else {
        NULL
      }
    ) +
    base_theme +
    theme(
      strip.placement = "outside",
      strip.text.y.left = element_text(angle = 0, size = 8),
      axis.text.x = element_text(angle = 35, hjust = 1, size = 6.5),
      legend.position = if (show_legend) "right" else "none",
      legend.direction = "vertical",
      legend.box = "vertical"
    )

  p
}

sector_pages <- split_pages(region_list_plot, regions_per_sector_page)
for (page_number in seq_along(sector_pages)) {
  page_regions <- sector_pages[[page_number]]
  regular_regions <- setdiff(page_regions, "World")
  is_incomplete_world_page <-
    "World" %in% page_regions &&
    length(page_regions) < regions_per_sector_page

  if (is_incomplete_world_page) {
    row_regions <- c(regular_regions, "World")
    row_heights <- c(
      rep(1, length(regular_regions)),
      regions_per_sector_page - length(regular_regions)
    )
    regional_rows <- lapply(seq_along(row_regions), function(row_number) {
      make_sector_page(
        page_regions = row_regions[[row_number]],
        show_legend = row_number == length(row_regions),
        add_annotation = FALSE
      )
    })
    p <- wrap_plots(regional_rows, ncol = 1, heights = row_heights) +
      plot_annotation(
        title = "AIM-Technology sectoral final energy demand in 2050",
        subtitle = paste(target_scenarios, collapse = "  |  ")
      )
  } else {
    p <- make_sector_page(page_regions = page_regions)
  }

  save_png(
    p,
    file.path(
      plot_root,
      "finene_sec",
      sprintf("AIMTech_finene_sector_2050_page_%02d.png", page_number)
    ),
    width = 450,
    height = 380
  )
}


# 4. SECTORAL CO2 EMISSIONS ---------------------------------------------------

save_transition_pages(
  data = emisec_data,
  spec = emisec,
  folder = "emisec",
  file_stem = "AIMTech_emisec_scenario_comparison",
  y_label = expression(paste("CO"[2], " emissions (MtCO"[2], "/yr)")),
  title = expression(paste("AIM-Technology sectoral CO"[2], " emissions"))
)


# 5. DISCOUNTED CUMULATIVE SYSTEM COST ---------------------------------------

cost_model_years <- sort(unique(transition_years[
  transition_years >= discount_base_year & transition_years <= discount_end_year
]))
cost_annual_years <- seq(discount_base_year, discount_end_year)

cost_model_data <- iamc_raw %>%
  filter(
    Region %in% region_list_plot,
    Variable == cost_variable,
    Year %in% cost_model_years
  ) %>%
  add_world_total() %>%
  select(Scenario, Region, Year, Value) %>%
  complete(
    Scenario = target_scenarios,
    Region = region_list_plot,
    Year = cost_model_years,
    fill = list(Value = 0)
  )

cost_cumulative <- cost_model_data %>%
  group_by(Scenario, Region) %>%
  group_modify(function(.x, .y) {
    tibble(
      Year = cost_annual_years,
      Value = approx(
        x = .x$Year,
        y = .x$Value,
        xout = cost_annual_years,
        method = "linear",
        rule = 2
      )$y
    )
  }) %>%
  ungroup() %>%
  mutate(
    DiscountFactor = (1 + discount_rate)^(Year - discount_base_year),
    PresentValue = Value / DiscountFactor
  ) %>%
  group_by(Scenario, Region) %>%
  summarise(CumulativeCost = sum(PresentValue, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Scenario = factor(Scenario, levels = target_scenarios),
    Region = factor(Region, levels = region_list_plot)
  )

cost_subtitle <- paste0(
  cost_variable,
  "; annual linear interpolation; ",
  discount_base_year, "-", discount_end_year,
  "; ", percent(discount_rate, accuracy = 1), " discount rate"
)

p_cost_region <- ggplot(
  cost_cumulative %>% filter(as.character(Region) %in% region_list),
  aes(Region, CumulativeCost, colour = Scenario, shape = Scenario, group = Scenario)
) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.3) +
  geom_point(position = position_dodge(width = 0.58), size = 2.5, stroke = 0.75) +
  scale_colour_manual(values = scenario_colors, drop = FALSE) +
  scale_shape_manual(values = scenario_shapes, drop = FALSE) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  labs(
    x = NULL,
    y = "Discounted cumulative additional system cost (billion US$2005)",
    title = "Regional cumulative total energy system cost through 2050",
    subtitle = cost_subtitle
  ) +
  base_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

save_png(
  p_cost_region,
  file.path(plot_root, "totcost", "AIMTech_totcost_31regions.png"),
  width = 380,
  height = 190
)

cost_world <- cost_cumulative %>%
  filter(as.character(Region) == "World") %>%
  mutate(Scenario = factor(Scenario, levels = target_scenarios))

p_cost_world <- ggplot(
  cost_world,
  aes("World", CumulativeCost, colour = Scenario, shape = Scenario)
) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.3) +
  geom_point(position = position_dodge(width = 0.45), size = 4, stroke = 0.9) +
  scale_colour_manual(values = scenario_colors, drop = FALSE) +
  scale_shape_manual(values = scenario_shapes, drop = FALSE) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  labs(
    x = NULL,
    y = "Discounted cumulative additional system cost (billion US$2005)",
    title = "World cumulative total energy system cost through 2050",
    subtitle = paste0(cost_subtitle, "; reported World values preferred")
  ) +
  base_theme +
  theme(legend.position = "bottom")

save_png(
  p_cost_world,
  file.path(plot_root, "totcost", "AIMTech_totcost_world.png"),
  width = 190,
  height = 160
)


# 6. WORLD ROAD-VEHICLE SHARES -----------------------------------------------

prepare_world_share_data <- function(spec) {
  spec_clean <- spec %>%
    transmute(
      Variable = as.character(Variable),
      Legend = as.character(Legend),
      Color = as.character(Color)
    )

  data <- iamc_raw %>%
    filter(
      Region == "World",
      Variable %in% spec_clean$Variable,
      Year %in% car_years
    ) %>%
    select(Scenario, Year, Variable, Value) %>%
    complete(
      Scenario = target_scenarios,
      Year = car_years,
      Variable = spec_clean$Variable,
      fill = list(Value = 0)
    ) %>%
    left_join(spec_clean, by = "Variable")

  if (sum(abs(data$Value), na.rm = TRUE) == 0) {
    stop(
      "No World vehicle-share values were found for variables: ",
      paste(spec_clean$Variable, collapse = ", ")
    )
  }

  share_multiplier <- if (max(abs(data$Value), na.rm = TRUE) <= 1.5) 100 else 1

  data %>%
    mutate(
      Value = Value * share_multiplier,
      Scenario = factor(Scenario, levels = target_scenarios),
      Year = factor(Year, levels = car_years),
      Legend = factor(Legend, levels = spec_clean$Legend)
    )
}

save_vehicle_share_plot <- function(spec, plot_title, filename) {
  data <- prepare_world_share_data(spec)

  p <- ggplot(data, aes(Scenario, Value, fill = Legend)) +
    geom_col(width = 0.74, linewidth = 0.15, colour = "white") +
    facet_wrap(vars(Year), nrow = 1, labeller = label_both) +
    mix_scale(spec) +
    scale_y_continuous(
      breaks = seq(0, 100, by = 20),
      labels = label_percent(scale = 1),
      expand = expansion(mult = c(0, 0.02))
    ) +
    coord_cartesian(ylim = c(0, 100)) +
    labs(
      x = NULL,
      y = "Vehicle stock share",
      title = plot_title,
      subtitle = "World; 2030 and 2050"
    ) +
    base_theme +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),
      legend.position = "bottom"
    )

  save_png(
    p,
    file.path(plot_root, "car", filename),
    width = 250,
    height = 160
  )
}

save_vehicle_share_plot(
  EVshapss,
  "Passenger road-vehicle technology shares",
  "AIMTech_car_passenger_world_2030_2050.png"
)

save_vehicle_share_plot(
  EVshafre,
  "Freight road-vehicle technology shares",
  "AIMTech_car_freight_world_2030_2050.png"
)

message("Completed all AIM-Technology scenario-comparison plots in: ", plot_root)
