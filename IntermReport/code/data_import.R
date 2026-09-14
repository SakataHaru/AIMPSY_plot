# MODEL DATA IMPORT ------------------------------------------------------------
# Keep Figure-specific filtering, grouping, and aggregation in panel functions.

read_iamc_gdx <- function(main_dir, model_name) {
  gdx_path <- file.path(main_dir, "merged_output.gdx")
  if (!file.exists(gdx_path)) {
    stop("Missing ", model_name, " GDX file: ", gdx_path)
  }

  raw_data <- gdxrrw::rgdx.param(
    normalizePath(gdx_path, winslash = "\\", mustWork = TRUE),
    "iamc_gdx"
  )

  required_columns <- c("Sc", "Sr", "Sv", "Y5", "iamc_gdx")
  missing_columns <- setdiff(required_columns, names(raw_data))
  if (length(missing_columns) > 0L) {
    stop(
      model_name,
      " iamc_gdx lacks expected dimensions/field: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  dplyr::transmute(
    raw_data,
    Scenario = as.character(.data$Sc),
    Region = as.character(.data$Sr),
    Variable = as.character(.data$Sv),
    Year = suppressWarnings(as.integer(as.character(.data$Y5))),
    Value = suppressWarnings(as.numeric(as.character(.data$iamc_gdx))),
    Model = model_name
  )
}

import_model_data <- function(aimt_main_dir, psm_main_dir, psm_primary_dir) {
  message("Reading AIM-Technology IAMC data: ", file.path(aimt_main_dir, "merged_output.gdx"))
  aimt_iamc <- read_iamc_gdx(aimt_main_dir, "AIM-Technology")

  message("Reading PSM IAMC data: ", file.path(psm_main_dir, "merged_output.gdx"))
  psm_iamc <- read_iamc_gdx(psm_main_dir, "PSM")

  list(
    aimt_iamc = aimt_iamc,
    psm_iamc = psm_iamc,
    metadata = list(
      aimt_main_dir = aimt_main_dir,
      aimt_iamc_gdx = file.path(aimt_main_dir, "merged_output.gdx"),
      psm_main_dir = psm_main_dir,
      psm_iamc_gdx = file.path(psm_main_dir, "merged_output.gdx"),
      psm_primary_dir = psm_primary_dir,
      iamc_symbol = "iamc_gdx"
    )
  )
}
