# PLOT SETTINGS ---------------------------------------------------------------
# Edit only this section when changing model runs or output behavior.
# Structure and implementation rules: ../PLOTTING_GUIDE.md

# AIM-Technology run: <root>/<scenario>/<run-date>/main
AIMT_SCENARIO <- "softlink_Sc"
AIMT_DATE <- "2606111707"
AIMT_OUTPUT_ROOT <- "C:/@Research_Workplace/300_AIMPSY-with-AI/AIMPSY_plot/IntermReport/data/AIM-Tech"
  #"//SY_3600_01/atfs02/Individual/hsakata/AIMEnduse/output"

# PSM run: <root>/<scenario>/<region>/<run-date>/main
PSM_SCENARIO <- "global_softlink_Sc"
PSM_REGION <- "Global"
PSM_DATE <- "2606171718"
PSM_OUTPUT_ROOT <- "C:/@Research_Workplace/300_AIMPSY-with-AI/AIMPSY_plot/IntermReport/data/AIM-PSY"
  #"//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output"

# Primary-GDX timeslice duration used to annualize vff_l and vfb_l.
time_resol <- "6HOUR"

# PSM node-to-R31 mapping used to identify interregional links.
PSM_REGION_MAP_FILE <- "C:/@Research_Workplace/300_AIMPSY-with-AI/AIMPSY_plot/IntermReport/data/M_R272.csv"

# Local GAMS installation used by gdxrrw.
GAMS_SYSTEM_DIR <- "C:/GAMS/win64/26.1"

# Relative paths are resolved from IntermReport/code.
OUTPUT_DIRECTORY <- "../output"

# Confirmed scenario names from the existing IntermReport code. Add further
# scenarios only after checking the input GDX (for example, the future SYF case).
target_scenarios <- c(
  ELE = "NPi500",
  H2 = "NPi500-LimCCS_bio",
  SYF = "NPi500-ZF2050"
)

BUILD_FIGURES <- TRUE
SAVE_INDIVIDUAL_PANELS <- TRUE
SAVE_FINAL_FIGURES <- TRUE
BUILD_FIG2 <- TRUE

# Fig. 3 representative scenario. Use one of 1.5C-ELE / 1.5C-H2 / 1.5C-SYF.
FIG3_SCENARIO <- "1.5C-H2"
BUILD_FIG3 <- TRUE
FIG3_LAYOUT <- "vertical" # "vertical" or "horizontal"
FIG3_NODE_THRESHOLD <- 0.1
FIG3_POWER_FLOW_THRESHOLD <- 0.01
FIG3_H2_FLOW_THRESHOLD <- 0.01
FIG3_NH3_FLOW_THRESHOLD <- 0.01
FIG3_SYF_FLOW_THRESHOLD <- 0.01


# PACKAGES AND SCRIPT LOCATION ------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(gdxrrw)
  library(cowplot)
  library(sf)
  library(rnaturalearth)
  library(tibble)
})

script_args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_args[grepl("^--file=", script_args)])
script_dir <- if (length(script_path) > 0L) {
  dirname(normalizePath(script_path[[1]], winslash = "/", mustWork = FALSE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

code_candidates <- unique(c(
  script_dir,
  file.path(script_dir, "code"),
  file.path(script_dir, "IntermReport", "code"),
  file.path(script_dir, "..", "code")
))
code_matches <- code_candidates[
  file.exists(file.path(code_candidates, "data_import.R")) &
    file.exists(file.path(code_candidates, "plot_common.R"))
]
if (length(code_matches) == 0L) {
  stop(
    "Could not locate the IntermReport/code directory. Checked: ",
    paste(code_candidates, collapse = ", ")
  )
}

code_dir <- normalizePath(code_matches[[1]], winslash = "/", mustWork = TRUE)
project_dir <- normalizePath(file.path(code_dir, ".."), winslash = "/", mustWork = TRUE)


# SHARED FUNCTIONS AND PANEL DEFINITIONS --------------------------------------

source(file.path(code_dir, "data_import.R"))
source(file.path(code_dir, "plot_common.R"))

panel_files <- c(
  "fig1/fig1_a.R",
  "fig1/fig1_b.R",
  "fig1/fig1_c.R",
  "fig1/fig1_d.R",
  "fig2/fig2_a.R",
  "fig2/fig2_b.R",
  "fig2/fig2_c.R",
  "fig2/fig2_d.R",
  "fig3/fig3_common.R",
  "fig3/fig3_a.R",
  "fig3/fig3_b.R"
)
invisible(lapply(file.path(code_dir, panel_files), source))

# dataframe.R is retained as the single legacy source for Variable/Legend/Color
# tables. The loader isolates those tables in a named list instead of creating
# every legacy object in the global environment.
VARIABLE_SPECS <- load_variable_specs(file.path(code_dir, "dataframe.R"))


# DERIVED SETTINGS AND INPUT PATHS --------------------------------------------

required_run_settings <- c(
  AIMT_SCENARIO = AIMT_SCENARIO,
  AIMT_DATE = AIMT_DATE,
  AIMT_OUTPUT_ROOT = AIMT_OUTPUT_ROOT,
  PSM_SCENARIO = PSM_SCENARIO,
  PSM_REGION = PSM_REGION,
  PSM_DATE = PSM_DATE,
  PSM_OUTPUT_ROOT = PSM_OUTPUT_ROOT,
  time_resol = time_resol,
  PSM_REGION_MAP_FILE = PSM_REGION_MAP_FILE,
  GAMS_SYSTEM_DIR = GAMS_SYSTEM_DIR,
  OUTPUT_DIRECTORY = OUTPUT_DIRECTORY
)
if (anyNA(required_run_settings) || any(!nzchar(trimws(required_run_settings)))) {
  invalid_names <- names(required_run_settings)[
    is.na(required_run_settings) | !nzchar(trimws(required_run_settings))
  ]
  stop("Run settings must not be blank: ", paste(invalid_names, collapse = ", "))
}

target_scenarios <- trimws(target_scenarios)
if (length(target_scenarios) == 0L || any(!nzchar(target_scenarios)) ||
    is.null(names(target_scenarios)) || any(!nzchar(names(target_scenarios))) ||
    anyDuplicated(names(target_scenarios)) || anyDuplicated(target_scenarios)) {
  stop("target_scenarios must be a uniquely named scenario mapping.")
}
fig3_scenario_key <- sub("^1[.]5C-", "", trimws(FIG3_SCENARIO), ignore.case = TRUE)
if (!fig3_scenario_key %in% names(target_scenarios)) {
  stop("FIG3_SCENARIO must identify one of: ", paste0("1.5C-", names(target_scenarios), collapse = ", "))
}
fig3_model_scenario <- unname(target_scenarios[[fig3_scenario_key]])
if (!FIG3_LAYOUT %in% c("vertical", "horizontal")) {
  stop("FIG3_LAYOUT must be 'vertical' or 'horizontal'.")
}

aimt_main_dir <- file.path(
  AIMT_OUTPUT_ROOT,
  AIMT_SCENARIO,
  AIMT_DATE,
  "main"
)
psm_run_dir <- file.path(
  PSM_OUTPUT_ROOT,
  PSM_SCENARIO,
  PSM_REGION,
  PSM_DATE
)
psm_main_dir <- file.path(psm_run_dir, "main")
psm_primary_dir <- file.path(psm_run_dir, "gams_output", "gdx_primary")

output_dir <- if (grepl("^(?:[A-Za-z]:|//|\\\\)", OUTPUT_DIRECTORY)) {
  normalizePath(OUTPUT_DIRECTORY, winslash = "/", mustWork = FALSE)
} else {
  normalizePath(file.path(code_dir, OUTPUT_DIRECTORY), winslash = "/", mustWork = FALSE)
}


# FIGURE ASSEMBLY AND SAVING --------------------------------------------------

build_fig1 <- function(
  data,
  output_dir,
  save_individual_panels = FALSE,
  save_final_figure = FALSE
) {
  fig1_output_dir <- file.path(output_dir, "fig1")
  dir.create(fig1_output_dir, recursive = TRUE, showWarnings = FALSE)

  fig1_a <- make_fig1_a(data)
  fig1_b <- make_fig1_b(data)
  fig1_c <- make_fig1_c(data)
  fig1_d <- make_fig1_d(data)

  # LEGENDS 
  legend_a <- cowplot::get_plot_component(
    fig1_a +
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1, reverse = TRUE)) +
      ggplot2::theme(
        legend.position = "right",
        legend.key.size = grid::unit(3.5, "mm"),
        legend.spacing = grid::unit(0, "mm"),
        legend.text = ggplot2::element_text(size = 7)
      ),
    "guide-box-right"
  )
  legend_c <- cowplot::get_plot_component(
    fig1_c +
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1)) +
      ggplot2::theme(
        legend.position = "right",
        legend.key.size = grid::unit(3.5, "mm"),
        legend.spacing = grid::unit(0, "mm"),
        legend.text = ggplot2::element_text(size = 7)
      ),
    "guide-box-right"
  )
  legend_d <- cowplot::get_plot_component(
    fig1_d +
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1)) +
      ggplot2::theme(
        legend.position = "right",
        legend.direction = "vertical",
        legend.key.size = grid::unit(3.5, "mm"),
        legend.spacing = grid::unit(0, "mm"),
        legend.text = ggplot2::element_text(size = 7)
      ),
    "guide-box-right"
  )

  # FIGURE 1 LAYOUT 
  # |              a | final-energy legend |
  # |        b (60%) | c | emissions legend |
  # |              d | electricity legend  |
  row_a <- cowplot::plot_grid(
    fig1_a + ggplot2::theme(legend.position = "none"),
    legend_a,
    nrow = 1,
    rel_widths = c(0.82, 0.18),
    labels = c("a", ""),
    label_size = 12
  )
  row_bc <- cowplot::plot_grid(
    fig1_b + ggplot2::theme(legend.position = "none"),
    fig1_c + ggplot2::theme(legend.position = "none"),
    legend_c,
    nrow = 1,
    rel_widths = c(0.60, 0.28, 0.12),
    labels = c("b", "c", ""),
    label_size = 12
  )
  row_d <- cowplot::plot_grid(
    fig1_d + ggplot2::theme(legend.position = "none"),
    legend_d,
    nrow = 1,
    rel_widths = c(0.82, 0.18),
    labels = c("d", ""),
    label_size = 12
  )
  fig1 <- cowplot::plot_grid(
    row_a,
    row_bc,
    row_d,
    ncol = 1,
    rel_heights = c(1, 1, 1)
  )

  panels <- list(a = fig1_a, b = fig1_b, c = fig1_c, d = fig1_d)

  if (isTRUE(save_individual_panels)) {
    preview_a <- cowplot::plot_grid(
      fig1_a + ggplot2::theme(legend.position = "none"), legend_a,
      nrow = 1, rel_widths = c(0.82, 0.18)
    )
    preview_c <- cowplot::plot_grid(
      fig1_c + ggplot2::theme(legend.position = "none"), legend_c,
      nrow = 1, rel_widths = c(0.70, 0.30)
    )
    preview_d <- cowplot::plot_grid(
      fig1_d + ggplot2::theme(legend.position = "none"), legend_d,
      nrow = 1, rel_widths = c(0.82, 0.18)
    )
    save_png_mm(preview_a, file.path(fig1_output_dir, "fig1_a.png"), 174, 62.5)
    save_png_mm(fig1_b, file.path(fig1_output_dir, "fig1_b.png"), 104.4, 62.5)
    save_png_mm(preview_c, file.path(fig1_output_dir, "fig1_c.png"), 69.6, 62.5)
    save_png_mm(preview_d, file.path(fig1_output_dir, "fig1_d.png"), 174, 62.5)
  }

  if (isTRUE(save_final_figure)) {
    save_png_mm(fig1, file.path(fig1_output_dir, "fig1.png"), 174, 187.5)
  }

  invisible(list(panels = panels, figure = fig1))
}

# FIGURE 2 -----------------------------------------------------------
build_fig2 <- function(
  data,
  primary_2050,
  output_dir,
  save_individual_panels = FALSE,
  save_final_figure = FALSE
) {
  fig2_output_dir <- file.path(output_dir, "fig2")
  dir.create(fig2_output_dir, recursive = TRUE, showWarnings = FALSE)

  fig2_a <- make_fig2_a(data)
  fig2_b <- make_fig2_b(primary_2050)
  fig2_c <- make_fig2_c(data, primary_2050)
  fig2_d <- make_fig2_d(primary_2050)
  panels <- list(a = fig2_a, b = fig2_b, c = fig2_c, d = fig2_d)

  # LEGEND 
  scenario_legend <- cowplot::get_plot_component(
    fig2_a +
      ggplot2::guides(
        color = ggplot2::guide_legend(nrow = 1),
        linetype = ggplot2::guide_legend(nrow = 1)
      ) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = grid::unit(3.5, "mm"),
        legend.spacing = grid::unit(0, "mm"),
        legend.margin = ggplot2::margin(0, 0, 0, 0)
      ),
    "guide-box-bottom"
  )

  # FIGURE 2 LAYOUT 
  # |       a       | b |
  # |     scenario legend |
  # |     c     |   d   |
  top_row <- cowplot::plot_grid(
    fig2_a + ggplot2::theme(legend.position = "none"),
    fig2_b + ggplot2::theme(legend.position = "none"),
    nrow = 1,
    rel_widths = c(4, 1),
    labels = c("a", "b"),
    label_size = 12
  )
  bottom_row <- cowplot::plot_grid(
    fig2_c + ggplot2::theme(legend.position = "none"),
    fig2_d + ggplot2::theme(legend.position = "none"),
    nrow = 1,
    rel_widths = c(3, 2),
    labels = c("c", "d"),
    label_size = 12
  )
  fig2 <- cowplot::plot_grid(
    top_row,
    scenario_legend,
    bottom_row,
    ncol = 1,
    rel_heights = c(1, 0.13, 1)
  )

  if (isTRUE(save_individual_panels)) {
    save_png_mm(fig2_a, file.path(fig2_output_dir, "fig2_a.png"), 144, 67.5)
    save_png_mm(fig2_b, file.path(fig2_output_dir, "fig2_b.png"), 36, 67.5)
    save_png_mm(fig2_c, file.path(fig2_output_dir, "fig2_c.png"), 108, 67.5)
    save_png_mm(fig2_d, file.path(fig2_output_dir, "fig2_d.png"), 72, 67.5)
  }

  if (isTRUE(save_final_figure)) {
    save_png_mm(fig2, file.path(fig2_output_dir, "fig2.png"), 174, 174)
  }

  invisible(list(panels = panels, figure = fig2))
}

# FIGURE 3 -----------------------------------------------------------
build_fig3 <- function(
  data, primary_2050, geodata, scenario, output_dir,
  save_individual_panels = FALSE, save_final_figure = FALSE
) {
  fig3_output_dir <- file.path(output_dir, "fig3")
  dir.create(fig3_output_dir, recursive = TRUE, showWarnings = FALSE)
  fig3_a <- make_fig3_a(
    data, primary_2050, geodata, scenario,
    FIG3_NODE_THRESHOLD, FIG3_POWER_FLOW_THRESHOLD
  )
  fig3_b <- make_fig3_b(
    data, primary_2050, geodata, scenario,
    FIG3_NODE_THRESHOLD, FIG3_H2_FLOW_THRESHOLD,
    FIG3_NH3_FLOW_THRESHOLD, FIG3_SYF_FLOW_THRESHOLD
  )
  # Size restrictions are intentionally not applied to the first Fig. 3 map.
  # Vertical is the readable default; horizontal remains available for review.
  if (FIG3_LAYOUT == "vertical") {
    fig3 <- cowplot::plot_grid(
      fig3_a, fig3_b, ncol = 1,
      labels = c("a", "b"), label_size = 14, rel_heights = c(1, 1)
    )
    final_size_mm <- c(width = 300, height = 270)
  } else {
    fig3 <- cowplot::plot_grid(
      fig3_a, fig3_b, nrow = 1,
      labels = c("a", "b"), label_size = 14, rel_widths = c(1, 1)
    )
    final_size_mm <- c(width = 400, height = 135)
  }
  if (isTRUE(save_individual_panels)) {
    save_png_mm(fig3_a, file.path(fig3_output_dir, "fig3_a.png"), 300, 135)
    save_png_mm(fig3_b, file.path(fig3_output_dir, "fig3_b.png"), 300, 135)
  }
  if (isTRUE(save_final_figure)) {
    save_png_mm(
      fig3, file.path(fig3_output_dir, "fig3.png"),
      final_size_mm[["width"]], final_size_mm[["height"]]
    )
  }
  invisible(list(panels = list(a = fig3_a, b = fig3_b), figure = fig3))
}


# RUN -------------------------------------------------------------------------

if (isTRUE(BUILD_FIGURES)) {
  # Input contract for this scaffold:
  # - AIM-Technology: <scenario>/<run-date>/main/merged_output.gdx, iamc_gdx
  # - PSM: <scenario>/<region>/<run-date>/main/merged_output.gdx, iamc_gdx
  # - PSM storage charge/discharge is not read in Fig. 1; the current IAMC
  #   mapping does not export it. Storage loss is read from iamc_gdx instead.
  data <- import_model_data(
    aimt_main_dir = aimt_main_dir,
    psm_main_dir = psm_main_dir,
    psm_primary_dir = psm_primary_dir,
    scenario_map = target_scenarios,
    variable_specs = VARIABLE_SPECS,
    gams_system_dir = GAMS_SYSTEM_DIR
  )

  figure_results <- list(
    fig1 = build_fig1(
      data = data,
      output_dir = output_dir,
      save_individual_panels = SAVE_INDIVIDUAL_PANELS,
      save_final_figure = SAVE_FINAL_FIGURES
    )
  )

  primary_2050 <- NULL
  if (isTRUE(BUILD_FIG2) || isTRUE(BUILD_FIG3)) {
    # Read the year-specific primary symbols once, then reuse the aggregated
    # object for all panel previews and the combined Fig. 2 / Fig. 3.
    primary_scenarios <- if (isTRUE(BUILD_FIG2)) {
      target_scenarios
    } else {
      target_scenarios[fig3_scenario_key]
    }
    primary_2050 <- load_psm_primary(
      psm_primary_dir = psm_primary_dir,
      scenario_map = primary_scenarios,
      year = 2050,
      time_resol = time_resol,
      gams_system_dir = GAMS_SYSTEM_DIR,
      region_map_path = PSM_REGION_MAP_FILE
    )
  }
  if (isTRUE(BUILD_FIG2)) {
    figure_results$fig2 <- build_fig2(
      data = data,
      primary_2050 = primary_2050,
      output_dir = output_dir,
      save_individual_panels = SAVE_INDIVIDUAL_PANELS,
      save_final_figure = SAVE_FINAL_FIGURES
    )
  }
  if (isTRUE(BUILD_FIG3)) {
    fig3_geodata <- load_fig3_geodata(file.path(project_dir, "data"))
    figure_results$fig3 <- build_fig3(
      data = data, primary_2050 = primary_2050, geodata = fig3_geodata,
      scenario = fig3_model_scenario, output_dir = output_dir,
      save_individual_panels = SAVE_INDIVIDUAL_PANELS,
      save_final_figure = SAVE_FINAL_FIGURES
    )
  }
}
