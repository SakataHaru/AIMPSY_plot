# AIM-Technology final energy demand plots -----------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(gdxrrw)
library(tibble)

source(file.path("code", "dataframe.R"))

case_name <- "AIM-Technology final energy demand scenario comparison"

energy_main_dir <- "//SY_3600_01/atfs02/Individual/hsakata/AIMEnduse/output/softlink_Sc/2606111707/main"
target_scenarios <- c("300C", "300C-LimCCS", "300C-LimCCS_bio", "300C-H2low", "300C-extremeH2")
plot_year <- 2050
base_dir <- file.path("output", "global_260527")
out_dir <- file.path(base_dir, "scenario_anarysis", "plot_finene")

region_list <- c(
  "JPN", "CHN", "IND", "IDN", "KOR", "THA", "MYS", "VNM",
  "XSE", "XSA", "XEA", "XCS", "XME", "AUS", "XOC",
  "CAN", "USA", "XE15", "XE10", "XE3", "TUR", "XEWI",
  "XEEI", "XENI", "RUS", "MEX", "BRA", "XLM", "ZAF",
  "XAF", "XNF"
)
region_list_plot <- c(region_list, "World")

facet_ncol <- 6

MyTheme <- theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    strip.background = element_rect(fill = "white", colour = "white"),
    strip.text = element_text(size = 8, colour = "black", face = "bold"),
    axis.text.x = element_text(size = 6, angle = 45, vjust = 0.9, hjust = 1),
    axis.text.y = element_text(size = 7),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.title = element_blank(),
    axis.ticks.length = grid::unit(0.15, "cm")
  )

read_iamc_gdx <- function(main_dir) {
  gdx_path <- file.path(main_dir, "merged_output.gdx")

  if (!file.exists(gdx_path)) {
    stop("Missing GDX file: ", gdx_path)
  }

  rgdx.param(gdx_path, "iamc_gdx") %>%
    transmute(
      Scenario = as.character(Sc),
      Region = as.character(Sr),
      Variable = as.character(Sv),
      Year = as.numeric(as.character(Y5)),
      Value = suppressWarnings(as.numeric(as.character(iamc_gdx))),
      Model = "AIM-Technology",
      Case = case_name
    ) %>%
    filter(!is.na(Value))
}

save_png <- function(plot, filename, width = 300, height = 300) {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    units = "mm",
    dpi = 300,
    bg = "white"
  )
  invisible(TRUE)
}

df_base_region <- read_iamc_gdx(energy_main_dir) %>%
  filter(
    Region %in% region_list,
    Scenario %in% target_scenarios,
    Year == plot_year
  )

df_world <- df_base_region %>%
  group_by(Scenario, Model, Variable, Year, Case) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Region = "World")

df_base <- bind_rows(df_base_region, df_world) %>%
  mutate(
    Scenario = factor(Scenario, levels = target_scenarios),
    Region = factor(Region, levels = region_list_plot)
  )

plot_specs <- tribble(
  ~key,          ~title,                         ~spec,
  "total",      "Final energy demand",           list(finene),
  "industry",   "Industry final energy demand",  list(finind),
  "buildings",  "Buildings final energy demand", list(finbui),
  "transport",  "Transport final energy demand", list(fintra)
)

plot_final_energy <- function(variable_spec, title) {
  variable_spec <- variable_spec %>%
    mutate(Variable = as.character(Variable))

  fill_color <- as.character(variable_spec$Color)
  names(fill_color) <- as.character(variable_spec$Variable)

  fill_label <- as.character(variable_spec$Legend)
  names(fill_label) <- as.character(variable_spec$Variable)

  d <- df_base %>%
    filter(Variable %in% variable_spec$Variable) %>%
    mutate(
      Variable = factor(Variable, levels = rev(variable_spec$Variable)),
      Scenario = factor(as.character(Scenario), levels = target_scenarios),
      Region = factor(as.character(Region), levels = region_list_plot)
    )

  if (nrow(d) == 0) return(NULL)

  ggplot(d) +
    geom_bar(aes(x = Scenario, y = Value, fill = Variable), stat = "identity", width = 0.82) +
    expand_limits(y = 0) +
    scale_fill_manual(values = fill_color, labels = fill_label, name = "") +
    facet_wrap(~Region, ncol = facet_ncol, scales = "free_y", drop = FALSE) +
    labs(
      y = expression(paste("Final energy demand (EJ ", {yr^-1}, ")")),
      title = paste0(title, " in ", plot_year, " (AIM-Technology, ", case_name, ")")
    ) +
    MyTheme +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 8),
      strip.text = element_text(size = 8),
      axis.text.x = element_text(size = 5.5, angle = 45, vjust = 0.9, hjust = 1)
    )
}

for (i in seq_len(nrow(plot_specs))) {
  p <- plot_final_energy(
    variable_spec = plot_specs$spec[[i]][[1]],
    title = plot_specs$title[i]
  )
  if (is.null(p)) next

  save_png(
    plot = p,
    filename = file.path(
      out_dir,
      paste0("AIM_Technology_", plot_specs$key[i], "_final_energy_demand_", plot_year, "_scenario_comparison.png")
    ),
    width = 300,
    height = 300
  )
}
