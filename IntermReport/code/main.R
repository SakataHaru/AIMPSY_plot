# PLOT SETTINGS ---------------------------------------------------------------
# Edit only this section when changing model runs or output behavior.

# AIM-Technology run: <root>/<scenario>/<run-date>/main
AIMT_SCENARIO <- "softlink_Sc"
AIMT_DATE <- "2606111707"
AIMT_OUTPUT_ROOT <- "C:/@Research_Workplace/300_AIMPSY-with-AI/AIMPSY_plot/IntermReport/data"
  #"//SY_3600_01/atfs02/Individual/hsakata/AIMEnduse/output"

# PSM run: <root>/<scenario>/<region>/<run-date>/main
PSM_SCENARIO <- "softlink_Sc"
PSM_REGION <- "Global"
PSM_DATE <- "2606171718"
PSM_OUTPUT_ROOT <- "//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output"

# Relative paths are resolved from IntermReport/code.
OUTPUT_DIRECTORY <- "../output"

# Confirmed scenario names from the existing IntermReport code. Add further
# scenarios only after checking the input GDX (for example, the future SYF case).
target_scenarios <- c("NPi500", "NPi500-LimCCS_bio", "NPi500-ZF2050")

# Keep FALSE while the panel files contain templates. Setting this to TRUE reads
# both GDX files, creates the placeholder panels, and enables the save switches.
BUILD_FIGURES <- FALSE
SAVE_INDIVIDUAL_PANELS <- FALSE
SAVE_FINAL_FIGURES <- FALSE


# PACKAGES AND SCRIPT LOCATION ------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(gdxrrw)
  library(patchwork)
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
  "fig1/fig1_e.R",
  "fig2/fig2_a.R",
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
  OUTPUT_DIRECTORY = OUTPUT_DIRECTORY
)
if (anyNA(required_run_settings) || any(!nzchar(trimws(required_run_settings)))) {
  invalid_names <- names(required_run_settings)[
    is.na(required_run_settings) | !nzchar(trimws(required_run_settings))
  ]
  stop("Run settings must not be blank: ", paste(invalid_names, collapse = ", "))
}

target_scenarios <- unique(trimws(as.character(target_scenarios)))
if (length(target_scenarios) == 0L || any(!nzchar(target_scenarios))) {
  stop("target_scenarios must contain at least one non-blank scenario name.")
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

build_all_figures <- function(
  data,
  output_dir,
  save_individual_panels = FALSE,
  save_final_figures = FALSE
) {
  ensure_figure_output_dirs(output_dir)

  fig1_a <- make_fig1_a(data)
  fig1_b <- make_fig1_b(data)
  fig1_c <- make_fig1_c(data)
  fig1_d <- make_fig1_d(data)
  fig1_e <- make_fig1_e(data)
  fig1 <- assemble_figure(
    panel_plots = list(a = fig1_a, b = fig1_b, c = fig1_c, d = fig1_d, e = fig1_e),
    figure = "fig1"
  )

  # TODO: Add fig2_b, fig2_c, ... after the Fig. 2 panel composition is fixed.
  fig2_a <- make_fig2_a(data)
  fig2 <- assemble_figure(panel_plots = list(a = fig2_a), figure = "fig2")

  fig3_a <- make_fig3_a(data)
  fig3_b <- make_fig3_b(data)
  fig3 <- assemble_figure(
    panel_plots = list(a = fig3_a, b = fig3_b),
    figure = "fig3"
  )

  panels <- list(
    fig1 = list(a = fig1_a, b = fig1_b, c = fig1_c, d = fig1_d, e = fig1_e),
    fig2 = list(a = fig2_a),
    fig3 = list(a = fig3_a, b = fig3_b)
  )
  figures <- list(fig1 = fig1, fig2 = fig2, fig3 = fig3)

  if (isTRUE(save_individual_panels)) {
    for (figure_name in names(panels)) {
      for (panel_name in names(panels[[figure_name]])) {
        save_panel(
          plot = panels[[figure_name]][[panel_name]],
          figure = figure_name,
          panel = panel_name,
          path = file.path(
            output_dir,
            figure_name,
            paste0(figure_name, "_", panel_name, ".png")
          )
        )
      }
    }
  }

  if (isTRUE(save_final_figures)) {
    for (figure_name in names(figures)) {
      save_figure(
        plot = figures[[figure_name]],
        figure = figure_name,
        path = file.path(output_dir, figure_name, paste0(figure_name, ".png"))
      )
    }
  }

  invisible(list(panels = panels, figures = figures))
}


# RUN -------------------------------------------------------------------------

if (isTRUE(BUILD_FIGURES)) {
  # Input contract for this scaffold:
  # - AIM-Technology: <scenario>/<run-date>/main/merged_output.gdx, iamc_gdx
  # - PSM: <scenario>/<region>/<run-date>/main/merged_output.gdx, iamc_gdx
  # - No primary GDX symbol is read until a panel declares its exact requirement.
  data <- import_model_data(
    aimt_main_dir = aimt_main_dir,
    psm_main_dir = psm_main_dir,
    psm_primary_dir = psm_primary_dir
  )

  figure_results <- build_all_figures(
    data = data,
    output_dir = output_dir,
    save_individual_panels = SAVE_INDIVIDUAL_PANELS,
    save_final_figures = SAVE_FINAL_FIGURES
  )
}
