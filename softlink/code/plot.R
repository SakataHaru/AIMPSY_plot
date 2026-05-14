# Global softlink 12-day plots -----------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(gdxrrw)
library(tibble)

#for (gams_sys_dir in c("C:/GAMS/38", "C:/GAMS/win64/26.1")) {
#  if (dir.exists(gams_sys_dir)) {
#    igdx(gams_sys_dir)
#    break
#  }
#}

case_name <- "Global softlink model comparison"

aimpsy <- '//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output'
scenario <- 'global_softlink'
region <- 'global'
date <- '2605131516'
power_main_dir <- paste0(aimpsy, "/", scenario, "/", region, "/", date, "/main")
energy_main_dir <- "//SY_3600_01/atfs02/Individual/hsakata/AIMEnduse/output/softlink_first/2605111701/main"

region_list <- c(
  "JPN", "CHN", "IND", "IDN", "KOR", "THA", "MYS", "VNM",
  "XSE", "XSA", "XEA", "XCS", "XME", "AUS", "XOC",
  "CAN", "USA", "XE15", "XE10", "XE3", "TUR", "XEWI",
  "XEEI", "XENI", "RUS", "MEX", "BRA", "XLM", "ZAF",
  "XAF", "XNF"
)

target_scenarios <- c("300C-LimCCS_bio", "300C-LimCCS", "300C", "Baseline")
target_years <- seq(2020, 2050, by = 5)
softlink_dir <- if (basename(normalizePath(getwd(), winslash = "/", mustWork = FALSE)) == "softlink") {
  "."
} else {
  "softlink"
}
base_dir <- file.path(softlink_dir, "output", "global_softlink", "global_compare")

model_color <- c("Energy system model" = "#1f78b4", "Power model" = "#e31a1c")
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

read_iamc_gdx <- function(main_dir, model_name) {
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
      Model    = model_name,
      Case     = case_name
    ) %>%
    filter(!is.na(Value))
}

df_base_region <- bind_rows(
  read_iamc_gdx(energy_main_dir, "Energy system model"),
  read_iamc_gdx(power_main_dir, "Power model")
) %>%
  filter(
    Region %in% region_list,
    Scenario %in% target_scenarios,
    Year %in% target_years
  ) %>%
  mutate(
    Scenario = factor(Scenario, levels = target_scenarios),
    Region = factor(Region, levels = region_list),
    Model = factor(Model, levels = names(model_color)),
    Case = case_name
  )

target_vars <- c(
  "Cap_Ele_Solar",
  "Cap_Ele_Win",
  "Cap_Ele_Hyp",
  "Cap_Ele_Sto_Bat",
  "Cap_Hyd_Ele",
  "Fin_Ene_Ele",
  "Sec_Ene_Ele",
  "Sec_Ene_Ele_Cur",
  "Sec_Ene_Ele_Tra_Los",
  "Sec_Ene_Ele_Tra_Los_Dis",
  "Sec_Ene_Ele_Tra_Los_Tra",
  "Sec_Ene_Hyd",
  "Sec_Ene_Hyd_Ele",
  "Sec_Ene_Inp_Ele_Hyd"
)

region_list_plot <- c(region_list, "World")

df_world <- df_base_region %>%
  group_by(Scenario, Model, Variable, Year, Case) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Region = "World")

df_base <- bind_rows(df_base_region, df_world) %>%
  mutate(
    Scenario = factor(Scenario, levels = target_scenarios),
    Region = factor(Region, levels = region_list_plot),
    Model = factor(Model, levels = names(model_color))
  )

df_plot <- df_base %>%
  filter(Variable %in% target_vars)

plot_global_softlink_facet <- function(var, ylab, title_txt, scenario_name, facet_ncol = 6) {
  d <- df_plot %>% filter(Variable == var, Scenario == scenario_name)
  if (nrow(d) == 0) return(NULL)

  ggplot(d, aes(Year, Value, color = Model, group = interaction(Region, Model))) +
    geom_line(linewidth = 0.35) +
    geom_point(shape = point_shape, size = 1.8, stroke = 0.5) +
    expand_limits(y = 0) +
    scale_color_manual(values = model_color, drop = FALSE) +
    facet_wrap(~Region, ncol = facet_ncol, scales = "free_y", drop = FALSE) +
    labs(
      y = ylab,
      title = paste0(title_txt, " (", scenario_name, ", ", case_name, ")")
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
  "plot_capacity", "Cap_Ele_Solar",       "Capacity (GW)",                         "Solar power capacity",              "Cap_global_softlink_Solar_31reg_world.png",
  "plot_capacity", "Cap_Ele_Win",         "Capacity (GW)",                         "Wind power capacity",               "Cap_global_softlink_Wind_31reg_world.png",
  "plot_capacity", "Cap_Ele_Hyp",         "Capacity (GW)",                         "Hydro power capacity",              "Cap_global_softlink_Hydro_31reg_world.png",
  "plot_capacity", "Cap_Ele_Sto_Bat",     "Capacity (GW)",                         "Battery capacity",                  "Cap_global_softlink_Battery_31reg_world.png",
  "plot_capacity", "Cap_Hyd_Ele",         "Capacity (GW)",                         "Electrolyzer capacity",             "Cap_global_softlink_Electrolyzer_31reg_world.png",
  "plot_eleuse",   "Sec_Ene_Ele_Cur",     "Curtailment (EJ/yr)",                   "Electricity curtailment",           "Eleuse_global_softlink_Curtailment_31reg_world.png",
  "plot_hydrogen", "Sec_Ene_Hyd",         "Hydrogen production (EJ/yr)",           "Hydrogen production",               "Hyd_global_softlink_Production_31reg_world.png",
  "plot_hydrogen", "Sec_Ene_Hyd_Ele",     "Hydrogen from electrolysis (EJ/yr)",    "Hydrogen from electrolysis",        "Hyd_global_softlink_Electrolysis_31reg_world.png",
  "plot_hydrogen", "Sec_Ene_Inp_Ele_Hyd", "Electricity input to hydrogen (EJ/yr)", "Electricity input to hydrogen",     "Hyd_global_softlink_ElectricityInput_31reg_world.png"
)

transmission_fill <- c(
  "Distribution loss" = "#9ecae1",
  "Transmission loss" = "#fdae6b"
)

plot_transmission_loss <- function(scenario_name, facet_ncol = 6) {
  d_total <- df_plot %>%
    filter(Variable == "Sec_Ene_Ele_Tra_Los", Scenario == scenario_name)

  d_split <- df_plot %>%
    filter(
      Variable %in% c("Sec_Ene_Ele_Tra_Los_Dis", "Sec_Ene_Ele_Tra_Los_Tra"),
      Scenario == scenario_name,
      Model == "Power model"
    ) %>%
    mutate(
      LossType = recode(
        Variable,
        "Sec_Ene_Ele_Tra_Los_Dis" = "Distribution loss",
        "Sec_Ene_Ele_Tra_Los_Tra" = "Transmission loss"
      ),
      LossType = factor(LossType, levels = c("Distribution loss", "Transmission loss"))
    )

  if (nrow(d_total) == 0 && nrow(d_split) == 0) return(NULL)

  if (nrow(d_total) == 0 && nrow(d_split) > 0) {
    d_total <- d_split %>%
      group_by(Scenario, Region, Year, Model, Case) %>%
      summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")
  }

  d_area <- d_split %>%
    group_by(Scenario, Region, Year) %>%
    arrange(LossType, .by_group = TRUE) %>%
    mutate(
      ymax = cumsum(Value),
      ymin = ymax - Value
    ) %>%
    ungroup()

  ggplot() +
    geom_ribbon(
      data = d_area,
      aes(Year, ymin = ymin, ymax = ymax, fill = LossType, group = interaction(Region, LossType)),
      alpha = 0.35
    ) +
    geom_line(
      data = d_total,
      aes(Year, Value, color = Model, group = interaction(Region, Model)),
      linewidth = 0.4
    ) +
    geom_point(
      data = d_total,
      aes(Year, Value, color = Model, group = interaction(Region, Model)),
      shape = point_shape,
      size = 1.8,
      stroke = 0.5
    ) +
    expand_limits(y = 0) +
    scale_color_manual(values = model_color, drop = FALSE) +
    scale_fill_manual(values = transmission_fill, drop = FALSE) +
    facet_wrap(~Region, ncol = facet_ncol, scales = "free_y", drop = FALSE) +
    labs(
      y = "Transmission Loss (EJ/yr)",
      title = paste0("Transmission loss (", scenario_name, ", ", case_name, ")")
    ) +
    MyTheme +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 8),
      axis.text = element_text(size = 7),
      axis.title.x = element_blank()
    )
}

plot_transmission_loss_share <- function(
    scenario_name,
    denominator_var,
    denominator_label,
    denominator_model = NULL,
    facet_ncol = 6) {
  d_denominator <- df_plot %>%
    filter(Variable == denominator_var, Scenario == scenario_name)

  if (!is.null(denominator_model)) {
    d_denominator <- d_denominator %>%
      filter(Model == denominator_model) %>%
      select(Scenario, Region, Year, Case, Denominator = Value)
    join_keys <- c("Scenario", "Region", "Year", "Case")
  } else {
    d_denominator <- d_denominator %>%
      select(Scenario, Region, Year, Model, Case, Denominator = Value)
    join_keys <- c("Scenario", "Region", "Year", "Model", "Case")
  }

  d_loss <- df_plot %>%
    filter(Variable == "Sec_Ene_Ele_Tra_Los", Scenario == scenario_name) %>%
    select(Scenario, Region, Year, Model, Case, TransmissionLoss = Value)

  d <- d_loss %>%
    inner_join(d_denominator, by = join_keys) %>%
    filter(!is.na(Denominator), Denominator > 0) %>%
    mutate(LossShare = 100 * TransmissionLoss / Denominator)

  d_split <- df_plot %>%
    filter(
      Variable %in% c("Sec_Ene_Ele_Tra_Los_Dis", "Sec_Ene_Ele_Tra_Los_Tra"),
      Scenario == scenario_name,
      Model == "Power model"
    ) %>%
    select(Scenario, Region, Year, Model, Case, Variable, SplitLoss = Value) %>%
    inner_join(d_denominator, by = join_keys) %>%
    filter(!is.na(Denominator), Denominator > 0) %>%
    mutate(
      LossType = recode(
        Variable,
        "Sec_Ene_Ele_Tra_Los_Dis" = "Distribution loss",
        "Sec_Ene_Ele_Tra_Los_Tra" = "Transmission loss"
      ),
      LossType = factor(LossType, levels = c("Distribution loss", "Transmission loss")),
      LossShare = 100 * SplitLoss / Denominator
    )

  if (nrow(d) == 0 && nrow(d_split) == 0) return(NULL)

  d_area <- d_split %>%
    group_by(Scenario, Region, Year) %>%
    arrange(LossType, .by_group = TRUE) %>%
    mutate(
      ymax = cumsum(LossShare),
      ymin = ymax - LossShare
    ) %>%
    ungroup()

  ggplot() +
    geom_ribbon(
      data = d_area,
      aes(Year, ymin = ymin, ymax = ymax, fill = LossType, group = interaction(Region, LossType)),
      alpha = 0.35
    ) +
    geom_line(
      data = d,
      aes(Year, LossShare, color = Model, group = interaction(Region, Model)),
      linewidth = 0.4
    ) +
    geom_point(
      data = d,
      aes(Year, LossShare, color = Model, group = interaction(Region, Model)),
      shape = point_shape,
      size = 1.8,
      stroke = 0.5
    ) +
    expand_limits(y = 0) +
    scale_color_manual(values = model_color, drop = FALSE) +
    scale_fill_manual(values = transmission_fill, drop = FALSE) +
    facet_wrap(~Region, ncol = facet_ncol, scales = "free_y", drop = FALSE) +
    labs(
      y = "Transmission loss rate (%)",
      title = paste0("Transmission loss rate relative to ", denominator_label, " (", scenario_name, ", ", case_name, ")")
    ) +
    MyTheme +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 8),
      axis.text = element_text(size = 7),
      axis.title.x = element_blank()
    )
}

save_width_mm <- 240
save_height_mm <- 220
facet_ncol <- 6

for (scenario_name in target_scenarios) {
  scenario_dir <- file.path(base_dir, scenario_name)

  for (i in seq_len(nrow(plot_specs))) {
    out_dir <- file.path(scenario_dir, plot_specs$group[i])
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

    p <- plot_global_softlink_facet(
      var = plot_specs$var[i],
      ylab = plot_specs$ylab[i],
      title_txt = plot_specs$title[i],
      scenario_name = scenario_name,
      facet_ncol = facet_ncol
    )

    if (!is.null(p)) {
      ggsave(
        filename = file.path(out_dir, paste0(scenario_name, "_", plot_specs$fname[i])),
        plot = p,
        width = save_width_mm,
        height = save_height_mm,
        units = "mm",
        dpi = 300,
        bg = "white"
      )
    }
  }

  out_dir <- file.path(scenario_dir, "plot_transmission")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  old_loss_share_file <- file.path(out_dir, paste0(scenario_name, "_TransmissionLossShare_global_softlink_31reg_world.png"))
  if (file.exists(old_loss_share_file)) file.remove(old_loss_share_file)

  p <- plot_transmission_loss(
    scenario_name = scenario_name,
    facet_ncol = facet_ncol
  )

  if (!is.null(p)) {
    ggsave(
      filename = file.path(out_dir, paste0(scenario_name, "_TransmissionLoss_global_softlink_31reg_world.png")),
      plot = p,
      width = save_width_mm,
      height = save_height_mm,
      units = "mm",
      dpi = 300,
      bg = "white"
    )
  }

  p <- plot_transmission_loss_share(
    scenario_name = scenario_name,
    denominator_var = "Sec_Ene_Ele",
    denominator_label = "model-specific Sec_Ene_Ele",
    facet_ncol = facet_ncol
  )

  if (!is.null(p)) {
    ggsave(
      filename = file.path(out_dir, paste0(scenario_name, "_TransmissionLossShare_SecEneEle_global_softlink_31reg_world.png")),
      plot = p,
      width = save_width_mm,
      height = save_height_mm,
      units = "mm",
      dpi = 300,
      bg = "white"
    )
  }

  p <- plot_transmission_loss_share(
    scenario_name = scenario_name,
    denominator_var = "Fin_Ene_Ele",
    denominator_label = "AIM-Technology Fin_Ene_Ele",
    denominator_model = "Energy system model",
    facet_ncol = facet_ncol
  )

  if (!is.null(p)) {
    ggsave(
      filename = file.path(out_dir, paste0(scenario_name, "_TransmissionLossShare_AIMTechnologyFinEneEle_global_softlink_31reg_world.png")),
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
    Region = factor(Region, levels = region_list_plot)
  )

for (scenario_name in target_scenarios) {
  d <- df_secele %>% filter(Scenario == scenario_name)
  if (nrow(d) == 0) next

  g_secele <- d %>%
    ggplot() +
    geom_bar(aes(x = Model, y = Value, fill = Variable), stat = "identity") +
    scale_fill_manual(values = plt$Color, labels = plt$Legend, name = "") +
    facet_wrap(~Region, ncol = 6, scales = "free_y", drop = FALSE) +
    guides(fill = guide_legend(reverse = FALSE)) +
    labs(
      y = expression(paste("Power generation (EJ ", {yr^-1}, ")")),
      title = paste0("Power generation in 2050 (", scenario_name, ", ", case_name, ")")
    ) +
    MyTheme +
    theme(
      title = element_text(size = 10),
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      legend.title = element_blank(),
      strip.text = element_text(size = 8),
      axis.text.x = element_text(size = 6, angle = 45, vjust = 0.9, hjust = 1)
    )

  out_dir <- file.path(base_dir, scenario_name, "plot_power")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  ggsave(
    filename = file.path(out_dir, paste0(scenario_name, "_power_generation_2050_global_softlink_31reg_world.png")),
    plot = g_secele,
    width = 260,
    height = 300,
    units = "mm",
    dpi = 300,
    bg = "white"
  )
}
