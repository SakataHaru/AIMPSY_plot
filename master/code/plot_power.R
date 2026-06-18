# import data and set up -----------------------------------------------

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
power_dates <- c("PSM-12DAY" = "2605271157")
power_main_dirs <- file.path(aimpsy, scenario, region, power_dates, "main")
names(power_main_dirs) <- names(power_dates)
energy_main_dir <- "//SY_3600_01/atfs02/Individual/hsakata/AIMEnduse/output/softlink_Sc/2606111707/main"

region_list <- c(
  "JPN", "CHN", "IND", "IDN", "KOR", "THA", "MYS", "VNM",
  "XSE", "XSA", "XEA", "XCS", "XME", "AUS", "XOC",
  "CAN", "USA", "XE15", "XE10", "XE3", "TUR", "XEWI",
  "XEEI", "XENI", "RUS", "MEX", "BRA", "XLM", "ZAF",
  "XAF", "XNF"
)

target_scenarios <- c("300C", "300C-LimCCS", "300C-LimCCS_bio", "300C-H2low", "300C-extremeH2")
target_years <- seq(2005, 2050, by = 5)
base_dir <- file.path("output", paste0("global_", substr(power_dates[["PSM-12DAY"]], 1, 6)))

model_color <- c("AIM-Technology" = "black", "PSM-12DAY" = "#e31a1c")
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
  read_iamc_gdx(energy_main_dir, "AIM-Technology"),
  read_iamc_gdx(power_main_dirs[["PSM-12DAY"]], "PSM-12DAY")
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
  "Sec_Ene_Ele_Solar",
  "Sec_Ene_Ele_Win",
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

save_width_mm <- 240
save_height_mm <- 220
facet_ncol <- 6

save_png <- function(plot, filename, width = save_width_mm, height = save_height_mm) {
  if (is.null(plot)) return(invisible(FALSE))

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

# Select and run only the plot section needed below.

# plot_capacity ---------------------------------------------------------------
# Run this section only when capacity plots are needed.
capacity_specs <- tribble(
  ~var,              ~ylab,           ~title,                    ~fname,
  "Cap_Ele_Solar",   "Capacity (GW)", "Solar power capacity",    "Cap_global_softlink_Solar_31reg_world.png",
  "Cap_Ele_Win",     "Capacity (GW)", "Wind power capacity",     "Cap_global_softlink_Wind_31reg_world.png",
  "Cap_Ele_Hyp",     "Capacity (GW)", "Hydro power capacity",    "Cap_global_softlink_Hydro_31reg_world.png",
  "Cap_Ele_Sto_Bat", "Capacity (GW)", "Battery capacity",        "Cap_global_softlink_Battery_31reg_world.png",
  "Cap_Hyd_Ele",     "Capacity (GW)", "Electrolyzer capacity",   "Cap_global_softlink_Electrolyzer_31reg_world.png"
)

for (scenario_name in target_scenarios) {
  out_dir <- file.path(base_dir, scenario_name, "plot_capacity")

  for (i in seq_len(nrow(capacity_specs))) {
    d <- df_plot %>%
      filter(Variable == capacity_specs$var[i], Scenario == scenario_name)
    if (nrow(d) == 0) next

    p <- ggplot(d, aes(Year, Value, color = Model, group = interaction(Region, Model))) +
      geom_line(linewidth = 0.35) +
      geom_point(shape = point_shape, size = 1.8, stroke = 0.5) +
      expand_limits(y = 0) +
      scale_color_manual(values = model_color, drop = FALSE) +
      facet_wrap(~Region, ncol = facet_ncol, scales = "free_y", drop = FALSE) +
      labs(
        y = capacity_specs$ylab[i],
        title = paste0(capacity_specs$var[i], " (", scenario_name, ", ", case_name, ")")
      ) +
      MyTheme +
      theme(
        legend.position = "bottom",
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 7),
        axis.title.x = element_blank()
      )

    save_png(
      plot = p,
      filename = file.path(out_dir, paste0(scenario_name, "_", capacity_specs$fname[i]))
    )
  }
}

# plot_eleuse -----------------------------------------------------------------
# Run this section only when electricity-use plots are needed.
eleuse_specs <- tribble(
  ~var,                ~ylab,                 ~title,                    ~fname,
  "Sec_Ene_Ele_Cur",   "Curtailment (EJ/yr)", "Electricity curtailment",  "Eleuse_global_softlink_Curtailment_31reg_world.png"
)

for (scenario_name in target_scenarios) {
  out_dir <- file.path(base_dir, scenario_name, "plot_eleuse")

  for (i in seq_len(nrow(eleuse_specs))) {
    d <- df_plot %>%
      filter(Variable == eleuse_specs$var[i], Scenario == scenario_name)
    if (nrow(d) == 0) next

    p <- ggplot(d, aes(Year, Value, color = Model, group = interaction(Region, Model))) +
      geom_line(linewidth = 0.35) +
      geom_point(shape = point_shape, size = 1.8, stroke = 0.5) +
      expand_limits(y = 0) +
      scale_color_manual(values = model_color, drop = FALSE) +
      facet_wrap(~Region, ncol = facet_ncol, scales = "free_y", drop = FALSE) +
      labs(
        y = eleuse_specs$ylab[i],
        title = paste0(eleuse_specs$var[i], " (", scenario_name, ", ", case_name, ")")
      ) +
      MyTheme +
      theme(
        legend.position = "bottom",
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 7),
        axis.title.x = element_blank()
      )

    save_png(
      plot = p,
      filename = file.path(out_dir, paste0(scenario_name, "_", eleuse_specs$fname[i]))
    )
  }
}

# plot_CF ---------------------------------------------------------------------
# Run this section only when capacity factor plots are needed.
cf_specs <- tribble(
  ~tech,   ~gen_var,             ~cap_var,         ~file_tag,
  "Solar", "Sec_Ene_Ele_Solar",  "Cap_Ele_Solar",  "Solar",
  "Wind",  "Sec_Ene_Ele_Win",    "Cap_Ele_Win",    "Wind"
)
ej_to_mwh <- 277777777.7777778
gw_to_mw <- 1000
hours_per_year <- 8760

for (scenario_name in target_scenarios) {
  out_dir <- file.path(base_dir, scenario_name, "plot_CF")

  for (i in seq_len(nrow(cf_specs))) {
    d_gen <- df_plot %>%
      filter(Variable == cf_specs$gen_var[i], Scenario == scenario_name) %>%
      select(Scenario, Region, Year, Model, Case, GenerationEJ = Value)

    d_cap <- df_plot %>%
      filter(Variable == cf_specs$cap_var[i], Scenario == scenario_name) %>%
      select(Scenario, Region, Year, Model, Case, CapacityGW = Value)

    d <- d_gen %>%
      inner_join(d_cap, by = c("Scenario", "Region", "Year", "Model", "Case")) %>%
      filter(!is.na(CapacityGW), CapacityGW > 0) %>%
      mutate(CapacityFactor = 100 * GenerationEJ * ej_to_mwh / (CapacityGW * gw_to_mw * hours_per_year))
    if (nrow(d) == 0) next

    p <- ggplot(d, aes(Year, CapacityFactor, color = Model, group = interaction(Region, Model))) +
      geom_line(linewidth = 0.35) +
      geom_point(shape = point_shape, size = 1.8, stroke = 0.5) +
      expand_limits(y = 0) +
      scale_color_manual(values = model_color, drop = FALSE) +
      facet_wrap(~Region, ncol = facet_ncol, scales = "free_y", drop = FALSE) +
      labs(
        y = "Capacity factor (%)",
        title = paste0(cf_specs$tech[i], " capacity factor (", scenario_name, ", ", case_name, ")")
      ) +
      MyTheme +
      theme(
        legend.position = "bottom",
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 7),
        axis.title.x = element_blank()
      )

    save_png(
      plot = p,
      filename = file.path(out_dir, paste0(scenario_name, "_CF_", cf_specs$file_tag[i], "_global_softlink_31reg_world.png"))
    )
  }
}

# plot_transmission -----------------------------------------------------------
# Run this section only when transmission loss plots are needed.
transmission_fill <- c(
  "Distribution loss" = "#9ecae1",
  "Transmission loss" = "#fdae6b"
)

for (scenario_name in target_scenarios) {
  out_dir <- file.path(base_dir, scenario_name, "plot_transmission")
  old_loss_share_file <- file.path(out_dir, paste0(scenario_name, "_TransmissionLossShare_global_softlink_31reg_world.png"))
  if (file.exists(old_loss_share_file)) file.remove(old_loss_share_file)

  d_total <- df_plot %>%
    filter(Variable == "Sec_Ene_Ele_Tra_Los", Scenario == scenario_name)

  d_split <- df_plot %>%
    filter(
      Variable %in% c("Sec_Ene_Ele_Tra_Los_Dis", "Sec_Ene_Ele_Tra_Los_Tra"),
      Scenario == scenario_name,
      Model == "PSM-12DAY"
    ) %>%
    mutate(
      LossType = recode(
        Variable,
        "Sec_Ene_Ele_Tra_Los_Dis" = "Distribution loss",
        "Sec_Ene_Ele_Tra_Los_Tra" = "Transmission loss"
      ),
      LossType = factor(LossType, levels = c("Distribution loss", "Transmission loss"))
    )

  d_area <- d_split %>%
    group_by(Scenario, Region, Model, Year) %>%
    arrange(LossType, .by_group = TRUE) %>%
    mutate(
      ymax = cumsum(Value),
      ymin = ymax - Value
    ) %>%
    ungroup()

  p <- ggplot() +
    geom_ribbon(
      data = d_area,
      aes(Year, ymin = ymin, ymax = ymax, fill = LossType, group = interaction(Region, Model, LossType)),
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
      title = paste0("Sec_Ene_Ele_Tra_Los (", scenario_name, ", ", case_name, ")")
    ) +
    MyTheme +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 8),
      axis.text = element_text(size = 7),
      axis.title.x = element_blank()
    )

  save_png(
    plot = p,
    filename = file.path(out_dir, paste0(scenario_name, "_TransmissionLoss_global_softlink_31reg_world.png"))
  )

  loss_share_specs <- tribble(
    ~denominator_var, ~denominator_label,             ~denominator_model,      ~file_tag,                  ~rate_label,
    "Sec_Ene_Ele",    "model-specific Sec_Ene_Ele",   NA_character_,           "SecEneEle",                "loss/sup",
    "Fin_Ene_Ele",    "AIM-Technology Fin_Ene_Ele",   "AIM-Technology",        "AIMTechnologyFinEneEle",   "loss/(dem+loss)"
  )

  for (j in seq_len(nrow(loss_share_specs))) {
    d_denominator <- df_plot %>%
      filter(Variable == loss_share_specs$denominator_var[j], Scenario == scenario_name)

    if (is.na(loss_share_specs$denominator_model[j])) {
      d_denominator <- d_denominator %>%
        select(Scenario, Region, Year, Model, Case, Denominator = Value)
      join_keys <- c("Scenario", "Region", "Year", "Model", "Case")
    } else {
      d_denominator <- d_denominator %>%
        filter(Model == loss_share_specs$denominator_model[j]) %>%
        select(Scenario, Region, Year, Case, Denominator = Value)
      join_keys <- c("Scenario", "Region", "Year", "Case")
    }

    d_loss_share <- df_plot %>%
      filter(Variable == "Sec_Ene_Ele_Tra_Los", Scenario == scenario_name) %>%
      select(Scenario, Region, Year, Model, Case, TransmissionLoss = Value) %>%
      inner_join(d_denominator, by = join_keys) %>%
      mutate(
        RateDenominator = if (loss_share_specs$file_tag[j] == "SecEneEle") {
          Denominator
        } else {
          Denominator + TransmissionLoss
        }
      ) %>%
      filter(!is.na(RateDenominator), RateDenominator > 0) %>%
      mutate(LossShare = 100 * TransmissionLoss / RateDenominator)

    d_split_share <- df_plot %>%
      filter(
        Variable %in% c("Sec_Ene_Ele_Tra_Los_Dis", "Sec_Ene_Ele_Tra_Los_Tra"),
        Scenario == scenario_name,
        Model == "PSM-12DAY"
      ) %>%
      select(Scenario, Region, Year, Model, Case, Variable, SplitLoss = Value) %>%
      inner_join(d_denominator, by = join_keys) %>%
      inner_join(
        d_loss_share %>%
          select(Scenario, Region, Year, Model, Case, TransmissionLoss, RateDenominator),
        by = c("Scenario", "Region", "Year", "Model", "Case")
      ) %>%
      filter(!is.na(RateDenominator), RateDenominator > 0) %>%
      mutate(
        LossType = recode(
          Variable,
          "Sec_Ene_Ele_Tra_Los_Dis" = "Distribution loss",
          "Sec_Ene_Ele_Tra_Los_Tra" = "Transmission loss"
        ),
        LossType = factor(LossType, levels = c("Distribution loss", "Transmission loss")),
        LossShare = 100 * SplitLoss / RateDenominator
      )

    d_area_share <- d_split_share %>%
      group_by(Scenario, Region, Model, Year) %>%
      arrange(LossType, .by_group = TRUE) %>%
      mutate(
        ymax = cumsum(LossShare),
        ymin = ymax - LossShare
      ) %>%
      ungroup()

    p <- ggplot() +
      geom_ribbon(
        data = d_area_share,
        aes(Year, ymin = ymin, ymax = ymax, fill = LossType, group = interaction(Region, Model, LossType)),
        alpha = 0.35
      ) +
      geom_line(
        data = d_loss_share,
        aes(Year, LossShare, color = Model, group = interaction(Region, Model)),
        linewidth = 0.4
      ) +
      geom_point(
        data = d_loss_share,
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
        y = paste0(loss_share_specs$rate_label[j], " (%)"),
        title = paste0(loss_share_specs$rate_label[j], " (", scenario_name, ", ", case_name, ")")
      ) +
      MyTheme +
      theme(
        legend.position = "bottom",
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 7),
        axis.title.x = element_blank()
      )

    save_png(
      plot = p,
      filename = file.path(out_dir, paste0(scenario_name, "_TransmissionLossShare_", loss_share_specs$file_tag[j], "_global_softlink_31reg_world.png"))
    )
  }
}

# plot_hydrogen ---------------------------------------------------------------
# Run this section only when hydrogen plots are needed.
hydrogen_specs <- tribble(
  ~var,                    ~ylab,                                  ~title,                           ~fname,
  "Sec_Ene_Hyd",           "Hydrogen production (EJ/yr)",          "Hydrogen production",             "Hyd_global_softlink_Production_31reg_world.png",
  "Sec_Ene_Hyd_Ele",       "Hydrogen from electrolysis (EJ/yr)",   "Hydrogen from electrolysis",      "Hyd_global_softlink_Electrolysis_31reg_world.png",
  "Sec_Ene_Inp_Ele_Hyd",   "Electricity input to hydrogen (EJ/yr)","Electricity input to hydrogen",   "Hyd_global_softlink_ElectricityInput_31reg_world.png"
)
hydrogen_ej_to_mwh <- 277777777.7777778
hydrogen_gw_to_mw <- 1000
hydrogen_hours_per_year <- 8760

for (scenario_name in target_scenarios) {
  out_dir <- file.path(base_dir, scenario_name, "plot_hydrogen")

  for (i in seq_len(nrow(hydrogen_specs))) {
    d <- df_plot %>%
      filter(Variable == hydrogen_specs$var[i], Scenario == scenario_name)
    if (nrow(d) == 0) next

    p <- ggplot(d, aes(Year, Value, color = Model, group = interaction(Region, Model))) +
      geom_line(linewidth = 0.35) +
      geom_point(shape = point_shape, size = 1.8, stroke = 0.5) +
      expand_limits(y = 0) +
      scale_color_manual(values = model_color, drop = FALSE) +
      facet_wrap(~Region, ncol = facet_ncol, scales = "free_y", drop = FALSE) +
      labs(
        y = hydrogen_specs$ylab[i],
        title = paste0(hydrogen_specs$var[i], " (", scenario_name, ", ", case_name, ")")
      ) +
      MyTheme +
      theme(
        legend.position = "bottom",
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 7),
        axis.title.x = element_blank()
      )

    save_png(
      plot = p,
      filename = file.path(out_dir, paste0(scenario_name, "_", hydrogen_specs$fname[i]))
    )
  }

  d_hyd_gen <- df_plot %>%
    filter(Variable == "Sec_Ene_Hyd_Ele", Scenario == scenario_name) %>%
    select(Scenario, Region, Year, Model, Case, HydrogenFromElectrolysisEJ = Value)

  d_hyd_cap <- df_plot %>%
    filter(Variable == "Cap_Hyd_Ele", Scenario == scenario_name) %>%
    select(Scenario, Region, Year, Model, Case, ElectrolyzerCapacityGW = Value)

  d_hyd_cf <- d_hyd_gen %>%
    inner_join(d_hyd_cap, by = c("Scenario", "Region", "Year", "Model", "Case")) %>%
    filter(!is.na(ElectrolyzerCapacityGW), ElectrolyzerCapacityGW > 0) %>%
    mutate(
      ElectrolyzerCapacityFactor =
        100 * HydrogenFromElectrolysisEJ * hydrogen_ej_to_mwh /
          (ElectrolyzerCapacityGW * hydrogen_gw_to_mw * hydrogen_hours_per_year)
    )

  if (nrow(d_hyd_cf) > 0) {
    p <- ggplot(d_hyd_cf, aes(Year, ElectrolyzerCapacityFactor, color = Model, group = interaction(Region, Model))) +
      geom_line(linewidth = 0.35) +
      geom_point(shape = point_shape, size = 1.8, stroke = 0.5) +
      expand_limits(y = 0) +
      scale_color_manual(values = model_color, drop = FALSE) +
      facet_wrap(~Region, ncol = facet_ncol, scales = "free_y", drop = FALSE) +
      labs(
        y = "Capacity factor (%)",
        title = paste0("Electrolyzer capacity factor from hydrogen production (", scenario_name, ", ", case_name, ")")
      ) +
      MyTheme +
      theme(
        legend.position = "bottom",
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 7),
        axis.title.x = element_blank()
      )

    save_png(
      plot = p,
      filename = file.path(out_dir, paste0(scenario_name, "_Hyd_CF_Electrolysis_global_softlink_31reg_world.png"))
    )
  }
}

# plot_power ------------------------------------------------------------------
# Run this section only when power generation mix plots are needed.
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

secele_color <- as.character(secele$Color)
names(secele_color) <- as.character(secele$Variable)
secele_legend <- as.character(secele$Legend)
names(secele_legend) <- as.character(secele$Variable)

df_secele <- df_base %>%
  filter(Variable %in% secele$Variable, Year == 2050) %>%
  mutate(
    Variable = factor(Variable, levels = rev(secele$Variable)),
    Region = factor(Region, levels = region_list_plot)
  )

for (scenario_name in target_scenarios) {
  d <- df_secele %>% filter(Scenario == scenario_name)
  if (nrow(d) == 0) next

  p <- d %>%
    ggplot() +
    geom_bar(aes(x = Model, y = Value, fill = Variable), stat = "identity") +
    scale_fill_manual(values = secele_color, labels = secele_legend, name = "") +
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

  save_png(
    plot = p,
    filename = file.path(
      base_dir,
      scenario_name,
      "plot_power",
      paste0(scenario_name, "_power_generation_2050_global_softlink_31reg_world.png")
    ),
    width = 260,
    height = 300
  )
}

# scenario_analysis -----------------------------------------------------------
# Compare all target scenarios in one plot.
scenario_analysis_dir <- file.path(base_dir, "scenario_anarysis")

scenario_shape <- setNames(
  rep(c(16, 17, 15, 18, 8, 3, 4, 7, 9, 10), length.out = length(target_scenarios)),
  target_scenarios
)

scenario_linetype <- setNames(
  rep(c("solid", "dashed", "dotted", "dotdash", "longdash"), length.out = length(target_scenarios)),
  target_scenarios
)

prep_scenario_line_data <- function(d, value_col = "Value") {
  d %>%
    mutate(
      Scenario = factor(as.character(Scenario), levels = target_scenarios),
      Model = factor(as.character(Model), levels = names(model_color)),
      Region = factor(as.character(Region), levels = region_list_plot)
    ) %>%
    filter(!is.na(.data[[value_col]]))
}

save_scenario_line_plot <- function(d, y_col, ylab, title, filename, folder, width = 260, height = 280) {
  if (nrow(d) == 0) return(invisible(FALSE))

  d <- prep_scenario_line_data(d, y_col)

  p <- ggplot(
    d,
    aes(
      x = Year,
      y = .data[[y_col]],
      color = Model,
      linetype = Scenario,
      shape = Scenario,
      group = interaction(Region, Model, Scenario)
    )
  ) +
    geom_line(linewidth = 0.35) +
    geom_point(size = 1.8, stroke = 0.5) +
    expand_limits(y = 0) +
    scale_color_manual(values = model_color, drop = TRUE, name = "Model") +
    scale_linetype_manual(values = scenario_linetype, drop = TRUE, name = "Scenario") +
    scale_shape_manual(values = scenario_shape, drop = TRUE, name = "Scenario") +
    facet_wrap(~Region, ncol = facet_ncol, scales = "free_y", drop = FALSE) +
    guides(
      color = guide_legend(order = 1, nrow = 1),
      linetype = guide_legend(order = 2, nrow = 1),
      shape = guide_legend(order = 2, nrow = 1)
    ) +
    labs(y = ylab, title = title) +
    MyTheme +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.text = element_text(size = 8),
      strip.text = element_text(size = 8),
      axis.text = element_text(size = 7),
      axis.title.x = element_blank()
    )

  save_png(
    plot = p,
    filename = file.path(scenario_analysis_dir, folder, filename),
    width = width,
    height = height
  )
}

scenario_raw_line_specs <- bind_rows(
  capacity_specs %>%
    mutate(folder = "plot_capacity", file_prefix = "scenario_analysis_capacity"),
  eleuse_specs %>%
    mutate(folder = "plot_eleuse", file_prefix = "scenario_analysis_eleuse"),
  tribble(
    ~var,                    ~ylab,                                  ~title,                           ~fname,                                                        ~folder,              ~file_prefix,
    "Sec_Ene_Ele_Tra_Los",   "Transmission Loss (EJ/yr)",            "Transmission loss",               "TransmissionLoss_global_softlink_31reg_world.png",            "plot_transmission",  "scenario_analysis_transmission",
    "Sec_Ene_Hyd",           "Hydrogen production (EJ/yr)",          "Hydrogen production",             "Hyd_global_softlink_Production_31reg_world.png",              "plot_hydrogen",      "scenario_analysis_hydrogen",
    "Sec_Ene_Hyd_Ele",       "Hydrogen from electrolysis (EJ/yr)",   "Hydrogen from electrolysis",      "Hyd_global_softlink_Electrolysis_31reg_world.png",            "plot_hydrogen",      "scenario_analysis_hydrogen",
    "Sec_Ene_Inp_Ele_Hyd",   "Electricity input to hydrogen (EJ/yr)","Electricity input to hydrogen",   "Hyd_global_softlink_ElectricityInput_31reg_world.png",        "plot_hydrogen",      "scenario_analysis_hydrogen"
  )
)

for (i in seq_len(nrow(scenario_raw_line_specs))) {
  d <- df_plot %>% filter(Variable == scenario_raw_line_specs$var[i])

  save_scenario_line_plot(
    d = d,
    y_col = "Value",
    ylab = scenario_raw_line_specs$ylab[i],
    title = paste0(scenario_raw_line_specs$title[i], " scenario comparison (", case_name, ")"),
    filename = paste0(scenario_raw_line_specs$file_prefix[i], "_", scenario_raw_line_specs$var[i], ".png"),
    folder = scenario_raw_line_specs$folder[i]
  )
}

for (i in seq_len(nrow(cf_specs))) {
  d_gen <- df_plot %>%
    filter(Variable == cf_specs$gen_var[i]) %>%
    select(Scenario, Region, Year, Model, Case, GenerationEJ = Value)

  d_cap <- df_plot %>%
    filter(Variable == cf_specs$cap_var[i]) %>%
    select(Scenario, Region, Year, Model, Case, CapacityGW = Value)

  d_cf <- d_gen %>%
    inner_join(d_cap, by = c("Scenario", "Region", "Year", "Model", "Case")) %>%
    filter(!is.na(CapacityGW), CapacityGW > 0) %>%
    mutate(CapacityFactor = 100 * GenerationEJ * ej_to_mwh / (CapacityGW * gw_to_mw * hours_per_year))

  save_scenario_line_plot(
    d = d_cf,
    y_col = "CapacityFactor",
    ylab = "Capacity factor (%)",
    title = paste0(cf_specs$tech[i], " capacity factor scenario comparison (", case_name, ")"),
    filename = paste0("scenario_analysis_CF_", cf_specs$file_tag[i], "_global_softlink_31reg_world.png"),
    folder = "plot_CF"
  )
}

for (j in seq_len(nrow(loss_share_specs))) {
  d_denominator <- df_plot %>% filter(Variable == loss_share_specs$denominator_var[j])

  if (is.na(loss_share_specs$denominator_model[j])) {
    d_denominator <- d_denominator %>%
      select(Scenario, Region, Year, Model, Case, Denominator = Value)
    join_keys <- c("Scenario", "Region", "Year", "Model", "Case")
  } else {
    d_denominator <- d_denominator %>%
      filter(Model == loss_share_specs$denominator_model[j]) %>%
      select(Scenario, Region, Year, Case, Denominator = Value)
    join_keys <- c("Scenario", "Region", "Year", "Case")
  }

  d_loss_share <- df_plot %>%
    filter(Variable == "Sec_Ene_Ele_Tra_Los") %>%
    select(Scenario, Region, Year, Model, Case, TransmissionLoss = Value) %>%
    inner_join(d_denominator, by = join_keys) %>%
    mutate(
      RateDenominator = if (loss_share_specs$file_tag[j] == "SecEneEle") {
        Denominator
      } else {
        Denominator + TransmissionLoss
      }
    ) %>%
    filter(!is.na(RateDenominator), RateDenominator > 0) %>%
    mutate(LossShare = 100 * TransmissionLoss / RateDenominator)

  save_scenario_line_plot(
    d = d_loss_share,
    y_col = "LossShare",
    ylab = paste0(loss_share_specs$rate_label[j], " (%)"),
    title = paste0(loss_share_specs$rate_label[j], " scenario comparison (", case_name, ")"),
    filename = paste0("scenario_analysis_TransmissionLossShare_", loss_share_specs$file_tag[j], "_global_softlink_31reg_world.png"),
    folder = "plot_transmission"
  )
}

d_hyd_gen_all <- df_plot %>%
  filter(Variable == "Sec_Ene_Hyd_Ele") %>%
  select(Scenario, Region, Year, Model, Case, HydrogenFromElectrolysisEJ = Value)

d_hyd_cap_all <- df_plot %>%
  filter(Variable == "Cap_Hyd_Ele") %>%
  select(Scenario, Region, Year, Model, Case, ElectrolyzerCapacityGW = Value)

d_hyd_cf_all <- d_hyd_gen_all %>%
  inner_join(d_hyd_cap_all, by = c("Scenario", "Region", "Year", "Model", "Case")) %>%
  filter(!is.na(ElectrolyzerCapacityGW), ElectrolyzerCapacityGW > 0) %>%
  mutate(
    ElectrolyzerCapacityFactor =
      100 * HydrogenFromElectrolysisEJ * hydrogen_ej_to_mwh /
        (ElectrolyzerCapacityGW * hydrogen_gw_to_mw * hydrogen_hours_per_year)
  )

save_scenario_line_plot(
  d = d_hyd_cf_all,
  y_col = "ElectrolyzerCapacityFactor",
  ylab = "Capacity factor (%)",
  title = paste0("Electrolyzer capacity factor scenario comparison (", case_name, ")"),
  filename = "scenario_analysis_Hyd_CF_Electrolysis_global_softlink_31reg_world.png",
  folder = "plot_hydrogen"
)

power_model_label <- c("AIM-Technology" = "AIM", "PSM-12DAY" = "PSM")
scenario_power_x_axis <- expand_grid(
  Scenario = target_scenarios,
  Model = names(model_color)
) %>%
  mutate(
    level = paste(Scenario, Model, sep = " | "),
    label = paste(Scenario, power_model_label[Model], sep = "\n")
  )
scenario_power_x_levels <- scenario_power_x_axis$level
scenario_power_x_labels <- setNames(scenario_power_x_axis$label, scenario_power_x_axis$level)

df_secele_scenario <- df_secele %>%
  mutate(
    Scenario = factor(as.character(Scenario), levels = target_scenarios),
    Model = factor(as.character(Model), levels = names(model_color)),
    ScenarioModel = factor(paste(Scenario, Model, sep = " | "), levels = scenario_power_x_levels),
    Region = factor(as.character(Region), levels = region_list_plot)
  )

df_secele_scenario_nonzero <- df_secele_scenario %>%
  filter(!is.na(Value), Value != 0) %>%
  mutate(Variable = droplevels(Variable))

if (nrow(df_secele_scenario_nonzero) > 0) {
  p <- df_secele_scenario_nonzero %>%
    ggplot() +
    geom_bar(aes(x = ScenarioModel, y = Value, fill = Variable), stat = "identity", width = 0.82) +
    scale_x_discrete(labels = scenario_power_x_labels, drop = FALSE) +
    scale_fill_manual(values = secele_color, labels = secele_legend, name = "") +
    facet_wrap(~Region, ncol = 6, scales = "free_y", drop = FALSE) +
    guides(fill = guide_legend(reverse = FALSE)) +
    labs(
      y = expression(paste("Power generation (EJ ", {yr^-1}, ")")),
      title = paste0("Power generation in 2050 scenario comparison (", case_name, ")")
    ) +
    MyTheme +
    theme(
      title = element_text(size = 10),
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      legend.title = element_blank(),
      strip.text = element_text(size = 8),
      axis.text.x = element_text(size = 5.5, angle = 45, vjust = 0.9, hjust = 1)
    )

  save_png(
    plot = p,
    filename = file.path(
      scenario_analysis_dir,
      "plot_power",
      "scenario_analysis_power_generation_2050_global_softlink_31reg_world.png"
    ),
    width = 300,
    height = 300
  )

  model_grouped_power_x_axis <- expand_grid(
    Model = names(model_color),
    Scenario = target_scenarios
  ) %>%
    mutate(
      level = paste(Model, Scenario, sep = " | "),
      label = paste(power_model_label[Model], Scenario, sep = "\n")
    )
  model_grouped_power_x_levels <- model_grouped_power_x_axis$level
  model_grouped_power_x_labels <- setNames(model_grouped_power_x_axis$label, model_grouped_power_x_axis$level)

  df_secele_model_grouped <- df_secele_scenario_nonzero %>%
    mutate(
      ModelScenarioGrouped = factor(paste(Model, Scenario, sep = " | "), levels = model_grouped_power_x_levels)
    )

  p_model_grouped <- df_secele_model_grouped %>%
    ggplot() +
    geom_bar(aes(x = ModelScenarioGrouped, y = Value, fill = Variable), stat = "identity", width = 0.82) +
    scale_x_discrete(labels = model_grouped_power_x_labels, drop = FALSE) +
    scale_fill_manual(values = secele_color, labels = secele_legend, name = "") +
    facet_wrap(~Region, ncol = 6, scales = "free_y", drop = FALSE) +
    guides(fill = guide_legend(reverse = FALSE)) +
    labs(
      y = expression(paste("Power generation (EJ ", {yr^-1}, ")")),
      title = paste0("Power generation in 2050 model-grouped scenario comparison (", case_name, ")")
    ) +
    MyTheme +
    theme(
      title = element_text(size = 10),
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      legend.title = element_blank(),
      strip.text = element_text(size = 8),
      axis.text.x = element_text(size = 5.5, angle = 45, vjust = 0.9, hjust = 1)
    )

  save_png(
    plot = p_model_grouped,
    filename = file.path(
      scenario_analysis_dir,
      "plot_power",
      "scenario_analysis_power_generation_2050_model_grouped_global_softlink_31reg_world.png"
    ),
    width = 300,
    height = 300
  )
}
