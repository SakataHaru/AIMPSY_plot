# Global softlink 12-day plots -----------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(gdxrrw)
library(tibble)

for (gams_sys_dir in c("C:/GAMS/38", "C:/GAMS/win64/26.1")) {
  if (dir.exists(gams_sys_dir)) {
    igdx(gams_sys_dir)
    break
  }
}

case_name <- "Global softlink 12-day"

aimpsy <- '//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output'
scenario <- 'global_softlink'
region <- 'global'
date <- '2604281658'
main_dir <- paste0(aimpsy, "/", scenario, "/", region, "/", date, "/main")

region_list <- c(
  "JPN", "CHN", "IND", "IDN", "KOR", "THA", "MYS", "VNM",
  "XSE", "XSA", "XEA", "XCS", "XME", "AUS", "XOC",
  "CAN", "USA", "XE15", "XE10", "XE3", "TUR", "XEWI",
  "XEEI", "XENI", "RUS", "MEX", "BRA", "XLM", "ZAF",
  "XAF", "XNF"
)

target_scenarios <- c("300C", "700C")
target_years <- seq(2020, 2050, by = 5)
softlink_dir <- if (basename(normalizePath(getwd(), winslash = "/", mustWork = FALSE)) == "softlink") {
  "."
} else {
  "softlink"
}
base_dir <- file.path(softlink_dir, "output", "global_softlink", "global_12day")

scenario_color <- c("300C" = "#1f78b4", "700C" = "#e31a1c")
point_shape <- 16

MyTheme <- theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    strip.background = element_rect(fill = "white", colour = "white"),
    strip.text.x = element_text(size = 10, colour = "black", face = "bold"),
    strip.text.y = element_text(size = 10, colour = "black", angle = 270, face = "bold"),
    axis.text.x = element_text(
      size = 8,
      angle = 45,
      vjust = 0.9,
      hjust = 1,
      margin = margin(t = 3, r = 0, b = 0, l = 0)
    ),
    axis.text.y = element_text(
      size = 8,
      margin = margin(t = 0, r = 3, b = 0, l = 0)
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.title = element_blank(),
    axis.ticks.length = grid::unit(0.15, "cm"),
    plot.tag = element_text(face = "bold")
  )

set_plot <- function(var) {
  plt <- list()
  plt$Color <- as.character(var$Color)
  names(plt$Color) <- as.character(var$Variable)
  plt$Legend <- as.character(var$Legend)
  names(plt$Legend) <- as.character(var$Variable)
  plt
}

secele <- tribble(
  ~Variable,                  ~Legend,          ~Color,
  "Sec_Ene_Ele_Oil_wo_CCS",   "Oil w/o CCS",    "sandybrown",
  "Sec_Ene_Ele_Oil_w_CCS",    "Oil w/ CCS",     "tan3",
  "Sec_Ene_Ele_Coa_wo_CCS",   "Coal w/o CCS",   "grey50",
  "Sec_Ene_Ele_Coa_w_CCS",    "Coal w/ CCS",    "grey30",
  "Sec_Ene_Ele_Gas_wo_CCS",   "Gas w/o CCS",    "lightgoldenrod",
  "Sec_Ene_Ele_Gas_w_CCS",    "Gas w/ CCS",     "lightgoldenrod3",
  "Sec_Ene_Ele_Nuc",          "Nuclear",        "moccasin",
  "Sec_Ene_Ele_Hyp",          "Hydro",          "lightsteelblue",
  "Sec_Ene_Ele_Bio_wo_CCS",   "Biomass w/o CCS","darkolivegreen2",
  "Sec_Ene_Ele_Bio_w_CCS",    "Biomass w/ CCS", "darkolivegreen4",
  "Sec_Ene_Ele_Geo",          "Geothermal",     "peru",
  "Sec_Ene_Ele_Solar",        "Solar",          "lightsalmon",
  "Sec_Ene_Ele_Win",          "Wind",           "lightskyblue3",
  "Sec_Ene_Ele_Hyd",          "Hydrogen",       "thistle2"
)

read_iamc_gdx <- function(main_dir) {
  gdx_path <- file.path(main_dir, "merged_output.gdx")

  if (!file.exists(gdx_path)) {
    stop("Missing GDX file: ", gdx_path)
  }

  rgdx.param(gdx_path, "iamc_gdx") %>%
    transmute(
      Scenario = as.character(Sc),
      Region   = as.character(Sr),
      Variable = as.character(Sv),
      Year     = as.numeric(as.character(Y5)),
      Value    = suppressWarnings(as.numeric(as.character(iamc_gdx))),
      Case     = case_name
    ) %>%
    filter(!is.na(Value))
}

df_base <- read_iamc_gdx(main_dir) %>%
  filter(
    Region %in% region_list,
    Scenario %in% target_scenarios,
    Year %in% target_years
  ) %>%
  mutate(
    Scenario = factor(Scenario, levels = target_scenarios),
    Region = factor(Region, levels = region_list),
    Case = case_name
  )

target_vars <- c(
  "Cap_Ele_Solar",
  "Cap_Ele_Win",
  "Cap_Ele_Hyp",
  "Cap_Ele_Sto_Bat",
  "Cap_Hyd_Ele",
  "Sec_Ene_Ele_Cur",
  "Sec_Ene_Ele_Tra_Los",
  "Sec_Ene_Hyd",
  "Sec_Ene_Hyd_Ele",
  "Sec_Ene_Inp_Ele_Hyd"
)

df_plot <- df_base %>%
  filter(Variable %in% target_vars)

plot_global_softlink_facet <- function(var, ylab, title_txt, facet_ncol = 6) {
  d <- df_plot %>% filter(Variable == var)
  if (nrow(d) == 0) return(NULL)

  ggplot(d, aes(Year, Value, color = Scenario, group = interaction(Region, Scenario))) +
    geom_line(linewidth = 0.35) +
    geom_point(shape = point_shape, size = 1.8, stroke = 0.5) +
    expand_limits(y = 0) +
    scale_color_manual(values = scenario_color, drop = FALSE) +
    facet_wrap(~Region, ncol = facet_ncol, scales = "free_y", drop = FALSE) +
    labs(
      y = ylab,
      title = paste0(title_txt, " (", paste(target_scenarios, collapse = " / "), ", ", case_name, ")")
    ) +
    MyTheme +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 8),
      axis.text = element_text(size = 7),
      axis.title.x = element_blank()
    )
}

plot_specs <- tribble(
  ~group,          ~var,                  ~ylab,                                   ~title,                              ~fname,
  "plot_capacity", "Cap_Ele_Solar",       "Capacity (GW)",                         "Solar power capacity",              "Cap_global_softlink_Solar_31reg.png",
  "plot_capacity", "Cap_Ele_Win",         "Capacity (GW)",                         "Wind power capacity",               "Cap_global_softlink_Wind_31reg.png",
  "plot_capacity", "Cap_Ele_Hyp",         "Capacity (GW)",                         "Hydro power capacity",              "Cap_global_softlink_Hydro_31reg.png",
  "plot_capacity", "Cap_Ele_Sto_Bat",     "Capacity (GW)",                         "Battery capacity",                  "Cap_global_softlink_Battery_31reg.png",
  "plot_capacity", "Cap_Hyd_Ele",         "Capacity (GW)",                         "Electrolyzer capacity",             "Cap_global_softlink_Electrolyzer_31reg.png",
  "plot_eleuse",   "Sec_Ene_Ele_Cur",     "Curtailment (EJ/yr)",                   "Electricity curtailment",           "Eleuse_global_softlink_Curtailment_31reg.png",
  "plot_eleuse",   "Sec_Ene_Ele_Tra_Los", "Transmission Loss (EJ/yr)",             "Transmission Loss",                 "Eleuse_global_softlink_TransmissionLoss_31reg.png",
  "plot_hydrogen", "Sec_Ene_Hyd",         "Hydrogen production (EJ/yr)",           "Hydrogen production",               "Hyd_global_softlink_Production_31reg.png",
  "plot_hydrogen", "Sec_Ene_Hyd_Ele",     "Hydrogen from electrolysis (EJ/yr)",    "Hydrogen from electrolysis",        "Hyd_global_softlink_Electrolysis_31reg.png",
  "plot_hydrogen", "Sec_Ene_Inp_Ele_Hyd", "Electricity input to hydrogen (EJ/yr)", "Electricity input to hydrogen",     "Hyd_global_softlink_ElectricityInput_31reg.png"
)

save_width_mm <- 240
save_height_mm <- 220
facet_ncol <- 6

for (i in seq_len(nrow(plot_specs))) {
  out_dir <- file.path(base_dir, plot_specs$group[i])
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  p <- plot_global_softlink_facet(
    var = plot_specs$var[i],
    ylab = plot_specs$ylab[i],
    title_txt = plot_specs$title[i],
    facet_ncol = facet_ncol
  )

  if (!is.null(p)) {
    ggsave(
      filename = file.path(out_dir, plot_specs$fname[i]),
      plot = p,
      width = save_width_mm,
      height = save_height_mm,
      units = "mm",
      dpi = 300,
      bg = "white"
    )
  }
}

plt <- set_plot(secele)

df_secele <- df_base %>%
  filter(Variable %in% secele$Variable, Year == 2050) %>%
  mutate(
    Variable = factor(Variable, levels = rev(secele$Variable)),
    Region = factor(Region, levels = region_list)
  )

g_secele <- df_secele %>%
  ggplot() +
  geom_bar(aes(x = Region, y = Value, fill = Variable), stat = "identity") +
  scale_fill_manual(values = plt$Color, labels = plt$Legend, name = "") +
  facet_wrap(~Scenario, ncol = 1) +
  guides(fill = guide_legend(reverse = FALSE)) +
  labs(
    y = expression(paste("Power generation (EJ ", {yr^-1}, ")")),
    title = paste0("Power generation in 2050 (", paste(target_scenarios, collapse = " / "), ", ", case_name, ")")
  ) +
  MyTheme +
  theme(
    title = element_text(size = 10),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_blank()
  )

out_dir <- file.path(base_dir, "plot_power")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

ggsave(
  filename = file.path(out_dir, "power_generation_2050_300C_700C_global_softlink_31reg.png"),
  plot = g_secele,
  width = 260,
  height = 260,
  units = "mm",
  dpi = 300,
  bg = "white"
)

df_world_total <- df_base %>%
  filter(Variable %in% target_vars) %>%
  group_by(Scenario, Variable, Year, Case) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")

plot_world_total <- function(var, ylab, title_txt) {
  d <- df_world_total %>% filter(Variable == var)
  if (nrow(d) == 0) return(NULL)

  ggplot(d, aes(Year, Value, color = Scenario, group = Scenario)) +
    geom_line(linewidth = 0.5) +
    geom_point(shape = point_shape, size = 2.0, stroke = 0.5) +
    expand_limits(y = 0) +
    scale_color_manual(values = scenario_color, drop = FALSE) +
    labs(
      y = ylab,
      title = paste0(title_txt, " - world total (", paste(target_scenarios, collapse = " / "), ", ", case_name, ")")
    ) +
    MyTheme +
    theme(legend.position = "bottom")
}

out_dir <- file.path(base_dir, "plot_world_total")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

for (i in seq_len(nrow(plot_specs))) {
  p <- plot_world_total(
    var = plot_specs$var[i],
    ylab = plot_specs$ylab[i],
    title_txt = plot_specs$title[i]
  )

  if (!is.null(p)) {
    ggsave(
      filename = file.path(out_dir, sub("_31reg\\.png$", "_world_total.png", plot_specs$fname[i])),
      plot = p,
      width = 120,
      height = 90,
      units = "mm",
      dpi = 300,
      bg = "white"
    )
  }
}
