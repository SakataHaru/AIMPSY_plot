# Asia hydrogen constraint comparison ---------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(gdxrrw)
library(purrr)
library(tibble)

gams_sys_dir <- "C:/GAMS/win64/26.1"
if (dir.exists(gams_sys_dir)) {
  igdx(gams_sys_dir)
}

asia_case_meta <- tribble(
  ~Case,              ~main_dir,
  "H2 constrained",   "//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output/global2050/Asia/2604082344/main",
  "H2 unconstrained", "//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output/global2050/Asia/2604090051/main"
)

region_list <- c(
  "JPN", "CHN", "IND", "IDN", "KOR", "THA", "MYS", "VNM",
  "XSE", "XSA", "XEA"
)

case_levels_plot <- asia_case_meta$Case
target_scenario <- "300C"
target_years <- seq(2020, 2050, by = 5)
base_dir <- file.path("output", "Asia", "hydrogen_case")

line_color <- c(
  "H2 constrained"   = "indianred2",
  "H2 unconstrained" = "deepskyblue2"
)

shape <- c(
  "H2 constrained"   = 16,
  "H2 unconstrained" = 4
)

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

read_iamc_gdx <- function(main_dir, case_name) {
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

df_base <- pmap_dfr(
  asia_case_meta,
  function(Case, main_dir) {
    read_iamc_gdx(main_dir = main_dir, case_name = Case)
  }
) %>%
  filter(
    Region %in% region_list,
    Scenario == target_scenario,
    Year %in% target_years
  )

target_vars <- c(
  "Cap_Ele_Solar",
  "Cap_Ele_Win",
  "Cap_Ele_Hyp",
  "Cap_Ele_Sto_Bat",
  "Sec_Ene_Ele_Cur",
  "Sec_Ene_Ele_Tra_Los",
  "Sec_Ene_Hyd",
  "Sec_Ene_Hyd_Ele",
  "Sec_Ene_Inp_Ele_Hyd"
)

df_plot <- df_base %>%
  filter(Variable %in% target_vars) %>%
  mutate(
    Case = factor(Case, levels = case_levels_plot),
    Region = factor(Region, levels = region_list)
  )

bg_2050 <- df_plot %>%
  filter(Year == 2050) %>%
  select(Region, Variable, Case, Value) %>%
  pivot_wider(names_from = Case, values_from = Value) %>%
  mutate(
    diff = `H2 unconstrained` - `H2 constrained`,
    bg = case_when(
      is.na(diff) ~ "missing",
      diff > 0 ~ "unconstrained_gt",
      diff < 0 ~ "unconstrained_lt",
      TRUE ~ "equal"
    ),
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf
  )

plot_h2_case_comparison_facet <- function(var, ylab, title_txt, facet_ncol = 6) {
  d <- df_plot %>% filter(Variable == var)
  if (nrow(d) == 0) return(NULL)
  
  bg_d <- bg_2050 %>% filter(Variable == var)
  
  ggplot(d) +
    geom_rect(
      data = bg_d,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = bg),
      inherit.aes = FALSE,
      alpha = 0.15
    ) +
    geom_line(
      aes(Year, Value, color = Case, group = Case),
      linewidth = 0.35
    ) +
    geom_point(
      aes(Year, Value, color = Case, shape = Case, group = Case),
      size = 2.0,
      stroke = 0.6
    ) +
    scale_fill_manual(
      values = c(
        unconstrained_gt = "#F4A6A6",
        unconstrained_lt = "#A6C8E5",
        equal = "grey90",
        missing = "grey95"
      ),
      guide = "none"
    ) +
    scale_color_manual(values = line_color, drop = FALSE) +
    scale_shape_manual(values = shape, drop = FALSE) +
    facet_wrap(~Region, ncol = facet_ncol, scales = "free_y", drop = FALSE) +
    labs(
      y = ylab,
      title = paste0(title_txt, " (", target_scenario, ")")
    ) +
    MyTheme +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 9),
      legend.title = element_blank(),
      strip.text = element_text(size = 8),
      axis.text = element_text(size = 7),
      axis.title.x = element_blank()
    )
}

plot_specs <- tribble(
  ~group,          ~var,                  ~ylab,                                   ~title,                              ~fname,
  "plot_capacity", "Cap_Ele_Solar",       "Capacity (GW)",                         "Solar power capacity",              "Cap_h2case_comparison_Solar_Asia.png",
  "plot_capacity", "Cap_Ele_Win",         "Capacity (GW)",                         "Wind power capacity",               "Cap_h2case_comparison_Wind_Asia.png",
  "plot_capacity", "Cap_Ele_Hyp",         "Capacity (GW)",                         "Hydro power capacity",              "Cap_h2case_comparison_Hydro_Asia.png",
  "plot_capacity", "Cap_Ele_Sto_Bat",     "Capacity (GW)",                         "Battery capacity",                  "Cap_h2case_comparison_Battery_Asia.png",
  "plot_eleuse",   "Sec_Ene_Ele_Cur",     "Curtailment (EJ/yr)",                   "Electricity curtailment",           "Eleuse_h2case_comparison_Curtailment_Asia.png",
  "plot_eleuse",   "Sec_Ene_Ele_Tra_Los", "Transmission Loss (EJ/yr)",             "Transmission Loss",                 "Eleuse_h2case_comparison_TransmissionLoss_Asia.png",
  "plot_hydrogen", "Sec_Ene_Hyd",         "Hydrogen production (EJ/yr)",           "Hydrogen production",               "Hyd_h2case_comparison_Production_Asia.png",
  "plot_hydrogen", "Sec_Ene_Hyd_Ele",     "Hydrogen from electrolysis (EJ/yr)",    "Hydrogen from electrolysis",        "Hyd_h2case_comparison_Electrolysis_Asia.png",
  "plot_hydrogen", "Sec_Ene_Inp_Ele_Hyd", "Electricity input to hydrogen (EJ/yr)", "Electricity input to hydrogen",     "Hyd_h2case_comparison_ElectricityInput_Asia.png"
)

save_width_mm <- 200
save_height_mm <- 180
facet_ncol <- 6

for (i in seq_len(nrow(plot_specs))) {
  out_dir <- file.path(base_dir, plot_specs$group[i])
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  p <- plot_h2_case_comparison_facet(
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
    Case = factor(Case, levels = case_levels_plot),
    Region = factor(Region, levels = region_list)
  )

g_secele <- df_secele %>%
  ggplot() +
  geom_bar(aes(x = Case, y = Value, fill = Variable), stat = "identity") +
  scale_fill_manual(values = plt$Color, labels = plt$Legend, name = "") +
  facet_wrap(vars(Region), nrow = 2, scales = "free_y") +
  guides(fill = guide_legend(reverse = FALSE)) +
  labs(y = expression(paste("Power generation (EJ ", {yr^-1}, ")"))) +
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
  filename = file.path(out_dir, "power_generation_2050_300C.png"),
  plot = g_secele,
  width = 220,
  height = 200,
  units = "mm",
  dpi = 300,
  bg = "white"
)
