script_args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_args[grepl("^--file=", script_args)])

project_dir <- if (length(script_path) > 0) {
  dirname(normalizePath(script_path[[1]], winslash = "/", mustWork = FALSE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

plot_scripts <- file.path(
  project_dir,
  "code",
  c(
    "plot_link.R",
    "plot_trade.R",
    "plot_trade_diff.R",
    "plot_network.R"
  )
)

for (plot_script in plot_scripts) {
  message("Sourcing: ", plot_script)
  source(plot_script, local = FALSE)
}
