# COMMON PLOT STYLE ------------------------------------------------------------

MyTheme <- ggplot2::theme_bw() +
  ggplot2::theme(
    panel.border = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_line(color = NA),
    axis.line = ggplot2::element_line(colour = "black"),
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill = "white", colour = "white"),
    strip.text.x = ggplot2::element_text(
      size = 8,
      colour = "black",
      angle = 0,
      face = "bold"
    ),
    strip.text.y = ggplot2::element_text(
      size = 8,
      colour = "black",
      angle = 270,
      face = "bold"
    ),
    axis.text.x = ggplot2::element_text(
      size = 8,
      angle = 45,
      vjust = 0.9,
      hjust = 1,
      margin = ggplot2::margin(t = 0.1, r = 0, b = 0, l = 0, unit = "cm")
    ),
    axis.text.y = ggplot2::element_text(
      size = 8,
      margin = ggplot2::margin(t = 0, r = 0.3, b = 0, l = 0, unit = "cm")
    ),
    axis.title.x = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 9),
    legend.title = ggplot2::element_text(size = 8),
    axis.ticks.length = grid::unit(0.15, "cm"),
    plot.tag = ggplot2::element_text(face = "bold")
  )


# COMMON COLOR DEFINITIONS -----------------------------------------------------

# No existing IntermReport definition maps ELE/H2/SYF to the requested color
# families. These readable placeholders should be replaced with approved journal
# colors after the palette is fixed.
SCENARIO_COLORS <- c(
  ELE = "#E6AB02", # yellow placeholder
  H2 = "#D73027",  # red placeholder
  SYF = "#2C7BB6"  # blue placeholder
)

CARRIER_LEVELS <- c(
  "Coal", "Liquids-fossil", "Gas-fossil", "Biomass", "Solar",
  "Electricity", "Heat", "Hydrogen", "Ammonia", "Synfuel"
)
CARRIER_COLORS <- c(
  "Coal" = "grey70",
  "Liquids-fossil" = "sandybrown",
  "Gas-fossil" = "moccasin",
  "Biomass" = "#A9D65D",
  "Solar" = "lightsalmon",
  "Electricity" = "lightsteelblue",
  "Heat" = "salmon",
  "Hydrogen" = "thistle2",
  "Ammonia" = "plum3",
  "Synfuel" = "mediumorchid1"
)

# Fig. 1-c separates power-sector emissions from the former aggregate
# energy-supply category. Other supply retains the legacy emisec color.
EMISSION_POWER_COLOR <- "gold"

FIG2_FLOW_COLORS <- c(
  "Hydrogen" = unname(CARRIER_COLORS[["Hydrogen"]]),
  "Ammonia" = "darkslateblue",
  "Synfuel" = unname(CARRIER_COLORS[["Synfuel"]]),
  "Other" = "grey65",
  "Loss" = "grey85"
)

# Conversion efficiencies in the model-side link definition for the current
# PSM interface. Values are applied to annual link activity (input basis).
FIG2_CONVERSION_EFFICIENCY <- c(
  electrolysis = 0.70,
  haber_bosch = 5.2 / (0.18 * 33.4),
  nh3_cracking = 0.99 * 0.85,
  fischer_tropsch = 0.73,
  methanation = 0.77
)

# This is the existing ALLPLOT_ENERGY classification with ammonia separated
# from Fin_Ene_Hyd. The duplicated ammonia row subtracts it from direct H2.
FINAL_ENERGY_SPEC <- tibble::tribble(
  ~Variable, ~PlotVariable, ~Legend, ~Color, ~Multiplier,
  "Fin_Ene_SolidsCoa",   "Fin_Ene_SolidsCoa", "Coal",            "grey70",        1,
  "Fin_Ene_Liq_Oil",     "Fin_Ene_Liq_Oil",   "Liquids-fossil",  "sandybrown",     1,
  "Fin_Ene_Gas_Fos",     "Fin_Ene_Gas_Fos",   "Gas-fossil",      "moccasin",       1,
  "Fin_Ene_SolidsBio",   "Fin_Ene_Biomass",   "Biomass",         "#A9D65D",        1,
  "Fin_Ene_Liq_Bio",     "Fin_Ene_Biomass",   "Biomass",         "#A9D65D",        1,
  "Fin_Ene_Solar",       "Fin_Ene_Solar",     "Solar",           "lightsalmon",    1,
  "Fin_Ene_Ele",         "Fin_Ene_Ele",       "Electricity",     "lightsteelblue", 1,
  "Fin_Ene_Heat",        "Fin_Ene_Heat",      "Heat",            "salmon",         1,
  "Fin_Ene_Hyd",         "Fin_Ene_Hyd",       "Hydrogen",        "thistle2",       1,
  "Fin_Ene_Hyd_Amm",     "Fin_Ene_Hyd",       "Hydrogen",        "thistle2",      -1,
  "Fin_Ene_Hyd_Amm",     "Fin_Ene_Hyd_Amm",   "Ammonia",         "plum3",          1,
  "Fin_Ene_Liq_Hyd_syn", "Fin_Ene_Hyd_syn",   "Synfuel",         "mediumorchid1",  1,
  "Fin_Ene_Gas_Hyd_syn", "Fin_Ene_Hyd_syn",   "Synfuel",         "mediumorchid1",  1
)

PSM_WORLD_REGIONS <- c(
  "JPN", "CHN", "IND", "IDN", "KOR", "THA", "MYS", "VNM",
  "XSE", "XSA", "XEA", "XCS", "XME", "AUS", "XOC", "CAN",
  "USA", "XE15", "XE10", "XE3", "TUR", "XEWI", "XEEI", "XENI",
  "RUS", "MEX", "BRA", "XLM", "ZAF", "XAF", "XNF"
)


# LEGACY VARIABLE AND COLOR SPECIFICATIONS ------------------------------------

load_variable_specs <- function(path) {
  if (!file.exists(path)) {
    stop("Missing variable/color specification file: ", path)
  }

  spec_environment <- new.env(parent = baseenv())
  assign("tribble", tibble::tribble, envir = spec_environment)
  sys.source(path, envir = spec_environment)

  object_names <- ls(spec_environment, all.names = TRUE)
  is_variable_spec <- vapply(
    object_names,
    function(object_name) {
      object <- get(object_name, envir = spec_environment, inherits = FALSE)
      is.data.frame(object) &&
        all(c("Variable", "Legend", "Color") %in% names(object))
    },
    logical(1)
  )

  spec_names <- object_names[is_variable_spec]
  specs <- mget(spec_names, envir = spec_environment, inherits = FALSE)
  lapply(
    specs,
    function(spec) {
      dplyr::mutate(
        spec,
        Variable = as.character(.data$Variable),
        Legend = as.character(.data$Legend),
        Color = as.character(.data$Color)
      )
    }
  )
}


# PANEL AND FIGURE HELPERS -----------------------------------------------------

assert_panel_data <- function(data) {
  if (!is.list(data) || !all(c("aimt_iamc", "psm_iamc", "variable_specs", "metadata") %in% names(data))) {
    stop("data must be the named list returned by import_model_data().")
  }
  invisible(TRUE)
}

assert_primary_data <- function(primary) {
  required <- c("psm_vfs_l", "psm_vff_l", "psm_vfb_l", "psm_region_map", "metadata")
  if (!is.list(primary) || !all(required %in% names(primary))) {
    stop("primary must be the named list returned by load_psm_primary().")
  }
  invisible(TRUE)
}

primary_scenario_crosswalk <- function(primary) {
  scenario_map <- primary$metadata$scenario_map
  tibble::tibble(
    Scenario = unname(scenario_map),
    ScenarioKey = factor(names(scenario_map), levels = names(scenario_map))
  )
}

split_primary_links <- function(dataset) {
  link_parts <- strsplit(as.character(dataset$Link), "_", fixed = TRUE)
  invalid <- lengths(link_parts) != 2L
  if (any(invalid)) {
    stop("Primary link labels must contain exactly one underscore. Examples: ",
         paste(utils::head(unique(dataset$Link[invalid]), 3), collapse = ", "))
  }
  dplyr::mutate(
    dataset,
    Node0 = vapply(link_parts, `[[`, character(1), 1L),
    Node1 = vapply(link_parts, `[[`, character(1), 2L)
  )
}

primary_node_region <- function(nodes, region_map) {
  node_core <- sub("-(AC|H2|H2TRD|NH3|NH3TRD|SYF|SYFTRD|SYM|SYMTRD)$", "", nodes)
  explicit_r31 <- grepl("^R31-", node_core)
  region <- region_map$Region[match(node_core, region_map$Node)]
  region[explicit_r31] <- sub("^R31-", "", node_core[explicit_r31])
  region
}

classify_transport_link <- function(link) {
  dplyr::case_when(
    grepl(".*-(H2|H2TRD)_.*-(H2|H2TRD)$", link) ~ "Hydrogen",
    grepl("^R31-[A-Z0-9]+-(NH3|NH3TRD)_R31-[A-Z0-9]+-(NH3|NH3TRD)$", link) ~ "Ammonia",
    grepl("^R31-[A-Z0-9]+-(SYF|SYFTRD)_R31-[A-Z0-9]+-(SYF|SYFTRD)$", link) ~ "Synfuel",
    TRUE ~ NA_character_
  )
}

scenario_crosswalk <- function(data) {
  scenario_map <- data$metadata$scenario_map
  tibble::tibble(
    Scenario = unname(scenario_map),
    ScenarioKey = factor(names(scenario_map), levels = names(scenario_map))
  )
}

assert_variables_present <- function(dataset, variables, context) {
  missing_variables <- setdiff(variables, unique(dataset$Variable))
  if (length(missing_variables) > 0L) {
    stop(context, " is missing variables: ", paste(missing_variables, collapse = ", "))
  }
  invisible(TRUE)
}

make_panel_placeholder <- function(label) {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 0.5,
      y = 0.5,
      label = paste0(label, "\nTODO: implement panel")
    ) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::labs(x = NULL, y = NULL) +
    MyTheme +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}

save_png_mm <- function(plot, path, width_mm, height_mm, dpi = 300, bg = "white") {
  if (tolower(tools::file_ext(path)) != "png") {
    stop("save_png_mm() only writes PNG files: ", path)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary_path <- tempfile("figure_", tmpdir = dirname(path), fileext = ".png")
  on.exit(unlink(temporary_path), add = TRUE)
  ggplot2::ggsave(
    filename = temporary_path,
    plot = plot,
    width = width_mm,
    height = height_mm,
    units = "mm",
    dpi = dpi,
    bg = bg
  )
  if (!file.copy(temporary_path, path, overwrite = TRUE)) {
    stop("Could not update PNG output: ", path)
  }
  invisible(path)
}
