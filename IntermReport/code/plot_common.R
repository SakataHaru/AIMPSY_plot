# COMMON PLOT STYLE ------------------------------------------------------------

BASE_FONT_SIZE_PT <- 8
AXIS_TEXT_SIZE_PT <- 7
AXIS_TITLE_SIZE_PT <- 8
LEGEND_TEXT_SIZE_PT <- 7
LEGEND_TITLE_SIZE_PT <- 8
PLOT_TAG_SIZE_PT <- 9
PANEL_MARGIN_MM <- 1.5

theme_paper <- function(base_size = BASE_FONT_SIZE_PT) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = AXIS_TEXT_SIZE_PT),
      axis.title = ggplot2::element_text(size = AXIS_TITLE_SIZE_PT),
      legend.text = ggplot2::element_text(size = LEGEND_TEXT_SIZE_PT),
      legend.title = ggplot2::element_text(size = LEGEND_TITLE_SIZE_PT),
      legend.position = "right",
      plot.tag = ggplot2::element_text(size = PLOT_TAG_SIZE_PT, face = "bold"),
      plot.margin = ggplot2::margin(
        PANEL_MARGIN_MM,
        PANEL_MARGIN_MM,
        PANEL_MARGIN_MM,
        PANEL_MARGIN_MM,
        unit = "mm"
      )
    )
}


# COMMON COLOR DEFINITIONS -----------------------------------------------------

# No existing IntermReport definition maps ELE/H2/SYF to the requested color
# families. These readable placeholders should be replaced with approved journal
# colors after the palette is fixed.
SCENARIO_COLORS <- c(
  ELE = "#E6AB02", # yellow placeholder
  H2 = "#D73027",  # red placeholder
  SYF = "#2C7BB6"  # blue placeholder
)

# TODO: Add named colors here only after technology/carrier labels are confirmed.
TECHNOLOGY_COLORS <- character(0)
CARRIER_COLORS <- character(0)


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


# PHYSICAL PANEL AND FIGURE SIZES ---------------------------------------------
# All values are final allocated dimensions in mm. Replace placeholders panel by
# panel after the journal layout is fixed. Full-width panels include column gaps.

FIGURE_WIDTH_MM <- 180
PANEL_GAP_MM <- 4

FIG_SIZE <- list(
  fig1 = list(
    a = c(width_mm = 88, height_mm = 55),
    b = c(width_mm = 88, height_mm = 55),
    c = c(width_mm = 88, height_mm = 55),
    d = c(width_mm = 88, height_mm = 55),
    e = c(width_mm = 180, height_mm = 55)
  ),
  # TODO: Add panels and revise the placeholder height when Fig. 2 is designed.
  fig2 = list(
    a = c(width_mm = 180, height_mm = 100)
  ),
  fig3 = list(
    a = c(width_mm = 88, height_mm = 80),
    b = c(width_mm = 88, height_mm = 80)
  )
)

# The # cells are physical gaps, not plots. A full-width panel spans the gap and
# therefore starts with the full allocated width instead of being stretched later.
FIG_LAYOUT <- list(
  fig1 = list(
    design = "A#B\n###\nC#D\n###\nEEE",
    widths_mm = c(88, PANEL_GAP_MM, 88),
    heights_mm = c(55, PANEL_GAP_MM, 55, PANEL_GAP_MM, 55),
    key_to_panel = c(A = "a", B = "b", C = "c", D = "d", E = "e")
  ),
  fig2 = list(
    design = "A",
    widths_mm = FIG_SIZE$fig2$a[["width_mm"]],
    heights_mm = FIG_SIZE$fig2$a[["height_mm"]],
    key_to_panel = c(A = "a")
  ),
  fig3 = list(
    design = "A#B",
    widths_mm = c(88, PANEL_GAP_MM, 88),
    heights_mm = 80,
    key_to_panel = c(A = "a", B = "b")
  )
)

get_panel_size_mm <- function(figure, panel) {
  if (!figure %in% names(FIG_SIZE)) {
    stop("Unknown figure: ", figure)
  }
  if (!panel %in% names(FIG_SIZE[[figure]])) {
    stop("Unknown panel '", panel, "' for ", figure)
  }

  size <- FIG_SIZE[[figure]][[panel]]
  if (!identical(names(size), c("width_mm", "height_mm")) ||
      any(!is.finite(size)) || any(size <= 0)) {
    stop("Invalid physical size for ", figure, " panel ", panel)
  }
  size
}

get_figure_size_mm <- function(figure) {
  if (!figure %in% names(FIG_LAYOUT)) {
    stop("Unknown figure: ", figure)
  }

  layout <- FIG_LAYOUT[[figure]]
  c(
    width_mm = sum(layout$widths_mm),
    height_mm = sum(layout$heights_mm)
  )
}

validate_figure_layout <- function(figure) {
  layout <- FIG_LAYOUT[[figure]]
  design_rows <- strsplit(layout$design, "\n", fixed = TRUE)[[1]]
  design_matrix <- do.call(rbind, strsplit(design_rows, "", fixed = TRUE))

  if (ncol(design_matrix) != length(layout$widths_mm) ||
      nrow(design_matrix) != length(layout$heights_mm)) {
    stop("Layout track counts do not match physical dimensions for ", figure)
  }

  for (layout_key in names(layout$key_to_panel)) {
    panel_name <- unname(layout$key_to_panel[[layout_key]])
    cells <- which(design_matrix == layout_key, arr.ind = TRUE)
    if (nrow(cells) == 0L) {
      stop("Layout key '", layout_key, "' is absent from ", figure)
    }

    allocated <- c(
      width_mm = sum(layout$widths_mm[unique(cells[, "col"])]),
      height_mm = sum(layout$heights_mm[unique(cells[, "row"])])
    )
    configured <- get_panel_size_mm(figure, panel_name)
    if (!isTRUE(all.equal(allocated, configured, tolerance = 1e-8))) {
      stop(
        "FIG_SIZE and FIG_LAYOUT disagree for ", figure, " panel ", panel_name,
        ": configured ", paste(configured, collapse = " x "),
        " mm; allocated ", paste(allocated, collapse = " x "), " mm"
      )
    }
  }

  figure_size <- get_figure_size_mm(figure)
  if (!isTRUE(all.equal(unname(figure_size[["width_mm"]]), FIGURE_WIDTH_MM))) {
    stop(figure, " width does not equal FIGURE_WIDTH_MM")
  }

  invisible(TRUE)
}


# PANEL AND FIGURE HELPERS -----------------------------------------------------

assert_panel_data <- function(data) {
  if (!is.list(data) || !all(c("aimt_iamc", "psm_iamc", "metadata") %in% names(data))) {
    stop("data must be the named list returned by import_model_data().")
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
    theme_paper() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}

assemble_figure <- function(panel_plots, figure) {
  validate_figure_layout(figure)
  layout <- FIG_LAYOUT[[figure]]
  expected_panels <- unname(layout$key_to_panel)
  missing_panels <- setdiff(expected_panels, names(panel_plots))
  extra_panels <- setdiff(names(panel_plots), expected_panels)
  if (length(missing_panels) > 0L || length(extra_panels) > 0L) {
    stop(
      "Panel list for ", figure, " does not match its layout. Missing: ",
      paste(missing_panels, collapse = ", "), "; extra: ",
      paste(extra_panels, collapse = ", ")
    )
  }

  plots_by_layout_key <- setNames(
    panel_plots[unname(layout$key_to_panel)],
    names(layout$key_to_panel)
  )

  assembled <- patchwork::wrap_plots(
    plots_by_layout_key,
    design = layout$design,
    widths = layout$widths_mm,
    heights = layout$heights_mm,
    guides = "keep"
  )
  attr(assembled, "physical_size_mm") <- get_figure_size_mm(figure)
  assembled
}

ensure_figure_output_dirs <- function(output_dir) {
  invisible(lapply(
    file.path(output_dir, names(FIG_SIZE)),
    dir.create,
    recursive = TRUE,
    showWarnings = FALSE
  ))
}

save_panel <- function(plot, figure, panel, path, dpi = 600, bg = "white") {
  size <- get_panel_size_mm(figure, panel)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    filename = path,
    plot = plot,
    width = unname(size[["width_mm"]]),
    height = unname(size[["height_mm"]]),
    units = "mm",
    dpi = dpi,
    limitsize = FALSE,
    bg = bg
  )
  invisible(path)
}

save_figure <- function(plot, figure, path, dpi = 600, bg = "white") {
  validate_figure_layout(figure)
  size <- get_figure_size_mm(figure)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    filename = path,
    plot = plot,
    width = unname(size[["width_mm"]]),
    height = unname(size[["height_mm"]]),
    units = "mm",
    dpi = dpi,
    limitsize = FALSE,
    bg = bg
  )
  invisible(path)
}
