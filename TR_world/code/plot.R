
###########################################################################
# setting
###########################################################################

# library -----------------------------------------------------------------

# print R version
version

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(gdxrrw)
library(ggsci)
library(viridis)
library(patchwork)
library(foreach)
library(sf)
library(Cairo)
library(lemon)
library(imputeTS)

library(purrr)
library(magrittr)
library(readxl)
library(magrittr)
library(cowplot)
library(tibble)


# import data-------------------------------------------------------------------------
# 1) User settings (EDIT HERE)

#AIMPSY
aimpsy <- '//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output'
scenario <- 'global2050'

cdir <- getwd()
pdir <- paste0(cdir,'/prog/inc_prog')
odir <- paste0(cdir,'/output/NorthAmerica')
basedir <- 'AIMPSY-master'
plot_year <- '2050'
#time_resol <- '12DAY'
gams_sys_dir <- 'C:/GAMS/38'

# get gams system directory
igdx(gams_sys_dir)

Sys.setlocale("LC_TIME", "en_US.UTF-8")

# AIM-Technology: already merged across all regions
# Set the single AIM-T GDX path here (EDIT)
aim_gdx_path <- "/path/to/aim_technology/merged_output.gdx"


# 2) Region x date mapping table for PSM-6H (EDIT dates manually)
#    reg_in must have "i_" prefix

psm_meta <- tribble(
  ~RegionLabel, ~reg_in,   ~date_6h,
  "JPN",        "i_JPN",   "2601271717",
  "CHN",        "i_CHN",   "2601271737",
  "IND",        "i_IND",   "2601291750",
  "IDN",        "i_IDN",   "2601291801",
  "KOR",        "i_KOR",   "2601291940",
  "THA",        "i_THA",   "2601291942",
  "MYS",        "i_MYS",   "2601261848",
  "VNM",        "i_VNM",   "2601291945",
  "XSE",        "i_XSE",   "2601291952",
  "XSA",        "i_XSA",   "2601291635",
  "XEA",        "i_XEA",   "2601291647",
  "XCS",        "i_XCS",   "2601291651",
  "XME",        "i_XME",   "2601291702",
  "AUS",        "i_AUS",   "2601290918",
  "XOC",        "i_XOC",   "2601290933",
  "CAN",        "i_CAN",   "2601291256",
  "USA",        "i_USA",   "2601291313",
  "XE15",       "i_XE15",  "2601271544",
  "XE10",       "i_XE10",  "2601271620",
  "XE3",        "i_XE3",   "2601271643",
  "TUR",        "i_TUR",   "2602031550",
  "XEWI",       "i_XEWI",  "2601261231",
  "XEEI",       "i_XEEI",  "2601261238",
  "XENI",       "i_XENI",  "2601261242",
  "RUS",        "i_RUS",   "2601271055",
  "MEX",        "i_MEX",   "2601271110",
  "BRA",        "i_BRA",   "2601271113",
 # "XLM",        "i_XLM",   "2601271130",
  "ZAF",        "i_ZAF",   "2601261516",
  "XAF",        "i_XAF",   "2601261518",
  "XNF",        "i_XNF",   "2601261834"
)


# 3) Path builder for PSM-6H
#    PSM-6H path: aimpsy/scenario/reg_in/date/main/merged_output.gdx

make_psm_gdx_path <- function(aimpsy, scenario, reg_in, date_6h) {
  str_c(aimpsy, "/", scenario, "/", reg_in, "/", date_6h, "/main/merged_output.gdx")
}


# 4) Core transformer for "iamc_gdx"

tidy_iamc <- function(df_raw, TR) {
  df_raw %>%
    rename(
      Scenario = "Sc",
      Region   = "Sr",
      Variable = "Sv",
      Year     = "Y5",
      Value    = "iamc_gdx"
    ) %>%
    mutate(
      Year     = as.numeric(as.character(Year)),
      Region   = as.character(Region),
      Scenario = as.character(Scenario)
    ) %>%
    pivot_wider(names_from = Year, values_from = Value, values_fill = 0) %>%
    pivot_longer(
      cols = -c(Scenario, Variable, Region),
      names_to = "Year", values_to = "Value"
    ) %>%
    pivot_wider(names_from = Variable, values_from = Value, values_fill = 0) %>%
    pivot_longer(
      cols = -c(Scenario, Region, Year),
      names_to = "Variable", values_to = "Value"
    ) %>%
    filter(Value != "N/A", !is.na(Value)) %>%
    mutate(
      Year        = as.numeric(Year),
      Value       = as.numeric(Value),
      TR          = TR
    )
}


# 5) Safe reader

read_one_safe <- function(gdx_path, TR) {
  if (!file.exists(gdx_path)) {
    message("MISSING FILE: ", gdx_path)
    return(tibble(
      Scenario = character(), Region = character(), Year = numeric(),
      Variable = character(), Value = numeric(),TR = character()
    ))
  }
  
  tryCatch(
    {
      df_raw <- rgdx.param(gdx_path, "iamc_gdx")
      tidy_iamc(df_raw, TR = TR)
    },
    error = function(e) {
      message("FAILED: ", gdx_path, " | ", conditionMessage(e))
      tibble(
        Scenario = character(), Region = character(), Year = numeric(),
        Variable = character(), Value = numeric(),TR = character()
        )
    }
  )
}


# 6) Read PSM-6H for all 31 regions, then bind

df_psm_6h <- pmap_dfr(
  psm_meta,
  function(RegionLabel,reg_in, date_6h) {
    
    gdx_path <- make_psm_gdx_path(
      aimpsy   = aimpsy,
      scenario = scenario,
      reg_in   = reg_in,
      date_6h  = date_6h
    )
    
    read_one_safe(
      gdx_path     = gdx_path,
      TR           = "PSM-6H"
    )
  }
)


# 7) Read AIM-Technology once (already merged across all regions)

# AIMTech 
aimtech <- '//SY_3600_01/atfs02/Individual/hsakata/AIMEnduse/output'
scenario <- 'global2100_PSModel'
time_resol <- 'AIM-T'
#reg_in <- 'NorthAmerica'
date <- '2512081616'
ddir <- paste0(aimtech,'/',scenario,'/',date)  # output path

df_aimt <- rgdx.param(str_c(ddir,'/main/merged_output.gdx'),'iamc_gdx') %>%   
  filter(Y5%in%c(2020:2050)) %>% 
  rename(Scenario='Sc',Region='Sr',Variable='Sv',Year='Y5',Value='iamc_gdx') %>% 
  mutate(Year=as.numeric(as.character(Year)),Region=as.character(Region),Scenario=as.character(Scenario)) %>% 
  pivot_wider(names_from=Year,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Scenario,Variable,Region),names_to='Year',values_to='Value') %>% 
  pivot_wider(names_from=Variable,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Scenario,Region,Year),names_to='Variable',values_to='Value') %>% 
  filter(Value != 'N/A') %>% filter(!is.na(Value)) %>% 
  mutate(Year=as.numeric(Year), Value=as.numeric(Value), TR="AIM-T")


# 8) Combined dataframe (what you want)
Region_list <- psm_meta %>% 
  pull(RegionLabel) %>% 
  unique()

df_all <- bind_rows(df_aimt, df_psm_6h) %>% 
  filter(Region%in%Region_list)


set_Sv <- rgdx.set(str_c(ddir,'/main/merged_output.gdx'),'Sv',te=TRUE) %>% rename(GAMS.code=1,Template.code=2)
set_Su <- rgdx.set(str_c(ddir,'/main/merged_output.gdx'),'Su',te=TRUE) %>% rename(GAMS.code=1,Unit=2)

scen_mat <- read_csv(str_c(ddir,'/main/scenario_table.csv'))

# Theme & Function --------------------------------------------------------

MyTheme <- theme_bw() +
  theme(
    panel.border=element_blank(),
    panel.grid.minor = element_line(color = NA), 
    axis.line=element_line(colour="black"),
    panel.background=element_rect(fill = "white"),
    panel.grid.major=element_blank(),
    strip.background=element_rect(fill="white", colour="white"),
    strip.text.x = element_text(size=10, colour = "black", angle = 0,face="bold"),
    strip.text.y = element_text(size=10, colour = "black", angle = 270,face="bold"),
    axis.text.x=element_text(size = 8,angle=45, vjust=0.9, hjust=1, margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")),
    axis.text.y=element_text(size = 8,margin = unit(c(t = 0, r = 0.3, b = 0, l = 0), "cm")),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=10),
    legend.text = element_text(size=9),
    legend.title = element_text(size=8),
    axis.ticks.length=unit(0.15,"cm"),
    plot.tag=element_text(face='bold')
  )

set_plot <- function(var){
  plt <- list()
  plt$Color <- as.character(var$Color); names(plt$Color) <- as.character(var$Variable)
  plt$Legend <- as.character(var$Legend); names(plt$Legend) <- as.character(var$Variable)
  return(plt)
}

source('code/dataframe.R')

year_all <- seq(2020,2050,by=5)


# 世界全体の排出経路  ----------------------------

line_color <- c(
  'Baseline' = 'black',
  '300C'     = 'indianred2',
  '700C'     = 'deepskyblue2'
)
g_emi_path<- df_AIMT %>% 
  filter(Region %in% "World")%>% 
  filter(Scenario%in%c('Baseline','300C','700C')) %>% 
  filter(Variable == 'Emi_CO2_Ene') %>% 
  ggplot() +
  geom_path(aes(x=Year,y=Value/1000,color=Scenario,group=Scenario),
            linewidth=1) +
  scale_color_manual(values=line_color) +
  labs(y = expression(paste({CO[2]},'emissions from energy sector (Gt-',{CO[2]},'',{yr^-1},')'))) +
  ylim(-10,50) +
  MyTheme +
  theme(legend.text=element_text(size=15),
        axis.title.y = element_text(size=10),
  )

g_emi_path_ele<- df_AIMT %>% 
  filter(Region %in% "World")%>% 
  filter(Scenario%in%c('Baseline','300C','700C')) %>% 
  filter(Variable == 'Emi_CO2_Ene_Sup_Ele') %>% 
  ggplot() +
  geom_path(aes(x=Year,y=Value/1000,color=Scenario,group=Scenario),
            linewidth=1) +
  scale_color_manual(values=line_color) +
  labs(y = expression(paste({CO[2]},'emissions from power sector (Gt-',{CO[2]},'',{yr^-1},')'))) +
  ylim(-10,50) +
  MyTheme +
  theme(legend.text=element_text(size=15),
        axis.title.y = element_text(size=10),
  )

l_emi <- plot_grid(
  get_legend(g_emi_path +
      theme(legend.text = element_text(size = 9), legend.title = element_blank()) +
      guides(color = guide_legend(nrow = 1))),
  nrow = 1)
path_ele <- plot_grid(g_emi_path+theme(legend.position='none'),g_emi_path_ele+theme(legend.position='none'),
                     nrow=1,rel_widths=c(1, 1),labels=c('a','b'),label_size=12)
finene_plot <- plot_grid(path_ele,l_emi,ncol=1,rel_heights=c(8,1))

ggsave(filename='output/emi/Fig_emission_pathway.png',plot=finene_plot,width=160,height=120,units='mm',dpi=300,bg='white')


# power generation------------------------------------------------------------------------

plt <- set_plot(secele)

df_secele <- df_all %>%
  filter(Scenario %in% c('Baseline','300C')) %>%
  filter(Variable %in% secele$Variable, Year == 2050) %>%
  mutate(
    Variable = factor(Variable, levels = rev(secele$Variable)))

g_secele <- df_secele %>%
  ggplot() +
  geom_bar(aes(x=TR,y=Value,fill=Variable),stat='identity') +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='') +
  facet_wrap(vars(Region),nrow=3,scales="free_y") +
  guides(fill=guide_legend(reverse=FALSE)) +
  labs(y=expression(paste('Power generation (EJ ',{yr^-1},')'))) +
  MyTheme +
  theme(
    title = element_text(size = 10),
    legend.position = "bottom", 
    legend.text = element_text(size = 10),
    legend.title = element_blank())

ggsave(filename='output/plot_power/world_secele.png',
       plot=g_secele,width=300,height=280,units='mm',dpi=300,bg='white')


# setting  -------------------------------------------------------------
TR_list <- c("AIM-T", "PSM-6H")
Region_list <- psm_meta %>% 
  pull(RegionLabel) %>%        # or pull(RegionLabel) でもOK
  unique()
line_color <- c("AIM-T"="black","PSM-1H"="indianred1","PSM-3H"="deepskyblue1","PSM-4H"="darkgoldenrod1","PSM-6H"="chartreuse2")
shape <- c("AIM-T"=16,"PSM-1H"=15,"PSM-3H"=17,"PSM-4H"=18,"PSM-6H"=4)


# 特定項目の全地域統合版-------------------------------------------------------------------------

Region_list <- psm_meta$RegionLabel

# 1) 対象変数（6個まとめて）
target_vars <- c(
  "Cap_Ele_Solar","Cap_Ele_Win","Cap_Ele_Sto_Bat","Cap_Hyd_Ele",
  "Sec_Ene_Ele_Cur", "Sec_Ene_Ele_Tra_Los", "Sec_Ene_Hyd_Ele"
)

# 2) 前処理（全6変数まとめて一回で） 
df_facet <- df_all %>%
  filter(Variable %in% target_vars, Region %in% Region_list, Scenario == '300C') %>%
  mutate(
    TR     = str_replace_all(str_trim(as.character(TR)), "–", "-"),
    TR     = factor(TR, levels = TR_list),
    Region = factor(as.character(Region), levels = Region_list)
  )

bg_2050 <- df_all %>%
  filter(Year == 2050,
         TR %in% c("AIM-T", "PSM-6H"),
         Variable %in% target_vars,
         Region %in% Region_list,
         Scenario == "300C") %>%
  mutate(
    TR     = str_replace_all(str_trim(as.character(TR)), "–", "-"),
    TR     = factor(TR, levels = TR_list),
    Region = factor(as.character(Region), levels = Region_list)
  ) %>% 
  select(Region, Variable, TR, Value) %>%
  tidyr::pivot_wider(names_from = TR, values_from = Value) %>%
  mutate(
    diff = `PSM-6H` - `AIM-T`,
    bg   = dplyr::case_when(
      diff >  0 ~ "PSM_gt",
      diff <  0 ~ "PSM_lt",
      TRUE      ~ "equal"
    )
  ) %>%
  mutate(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)

# 3) 31地域 facet 用の汎用プロット関数 
plot_tr_comparison_31facet <- function(var, ylab, title_txt,
                                       facet_ncol = 6) {
  
  d <- df_facet %>% filter(Variable == var)
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
      aes(Year, Value, color = TR, group = interaction(TR, Scenario)),
      size = 0.35
    ) +
    geom_point(
      aes(Year, Value, color = TR, shape = TR, group = interaction(TR, Scenario)),
      size = 2.0, stroke = 0.6
    ) +
    scale_fill_manual(
      values = c(PSM_gt = "#F4A6A6", PSM_lt = "#A6C8E5", equal = "grey90"),
      guide = "none"
    ) +
    scale_color_manual(values = line_color, drop = FALSE) +
    scale_shape_manual(values = shape, drop = FALSE) +
    facet_wrap(~Region, ncol = facet_ncol, scales = "free_y", drop = FALSE) +
    labs(y = ylab, title = title_txt) +
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

# 4) 出力仕様（6枚分）
plot_specs <- tibble::tribble(
  ~group,          ~var,               ~ylab,                                   ~title,                                  ~fname,
  "plot_capacity", "Cap_Ele_Solar",    "Capacity (GW)",                          "Solar power capacity",                  "Cap_TR_comparison_Solar_31reg.png",
  "plot_capacity", "Cap_Ele_Win",      "Capacity (GW)",                          "Wind power capacity",                   "Cap_TR_comparison_Wind_31reg.png",
  "plot_capacity", "Cap_Ele_Sto_Bat",  "Capacity (GW)",                          "Battery capacity",                      "Cap_TR_comparison_Battery_31reg.png",
  "plot_capacity", "Cap_Hyd_Ele",      "Capacity (GW)",                          "Electrolyzer capacity",                 "Cap_TR_comparison_Electrolyzer_31reg.png",
  "plot_eleuse",   "Sec_Ene_Ele_Cur",  "Curtailment (EJ/yr)",                    "Electricity curtailment",               "Eleuse_TR_comparison_Curtailment_31reg.png",
  "plot_eleuse",   "Sec_Ene_Ele_Tra_Los",  "Transmission Loss (EJ/yr)",          "Transmission Loss",                     "Eleuse_TR_comparison_TransmissionLoss_31reg.png",
  "plot_eleuse",   "Sec_Ene_Hyd_Ele",  "Electricity used for electrolysis (EJ/yr)","Electricity for hydrogen production", "Eleuse_TR_comparison_Electrolysis_31reg.png"
)

# 5) 6枚を個別に保存（31facet向けサイズ）
# 6列×6行を想定して、横長すぎないサイズにしてる
# 文字が詰まるなら height を増やす or facet_ncol を5に
save_width_mm  <- 300
save_height_mm <- 240
facet_ncol     <- 6

for (i in seq_len(nrow(plot_specs))) {
  
  out_dir <- file.path("output", plot_specs$group[i])
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  p <- plot_tr_comparison_31facet(
    var        = plot_specs$var[i],
    ylab       = plot_specs$ylab[i],
    title_txt  = plot_specs$title[i],
    facet_ncol = facet_ncol
  )
  
  if (!is.null(p)) {
    ggsave(
      filename = file.path(out_dir, plot_specs$fname[i]),
      plot     = p,
      width    = save_width_mm,
      height   = save_height_mm,
      units    = "mm",
      dpi      = 300,
      bg       = "white"
    )
  }
}

# 設備容量の比較-------------------------------------------------------------------------

target_vars <- c("Cap_Ele_Solar","Cap_Ele_Win","Cap_Ele_Sto_Bat","Cap_Hyd_Ele")

df_na_facet <- df_all %>%
  filter(Variable %in% target_vars, Region %in% Region_list) %>%
  mutate(TR=str_replace_all(str_trim(as.character(TR)),"–","-"),
         TR=factor(TR,levels=TR_list),
         Region=factor(Region,levels=Region_list))

plot_tr_comparison_facet <- function(var,y){
  d <- df_na_facet %>% filter(Variable==var); if(nrow(d)==0) return(NULL)
  ggplot(d)+
    geom_path(aes(Year,Value,color=TR,group=TR),size=0.4)+
    geom_point(aes(Year,Value,color=TR,shape=TR),size=2.5,stroke=0.7)+
    scale_color_manual(values=line_color,drop=FALSE)+
    scale_shape_manual(values=shape,drop=FALSE)+
    facet_wrap(~Region,nrow=1,scales="free_y",drop=FALSE)+
    labs(y=y,title=var)+MyTheme+
    theme(legend.position="bottom",
          legend.text=element_text(size=10),
          legend.title=element_blank(),
          strip.text=element_text(size=10))
}

p1 <- plot_tr_comparison_facet("Cap_Ele_Solar","Capacity (GW)")+
  labs(title = "Solar power capacity")
p2 <- plot_tr_comparison_facet("Cap_Ele_Win","Capacity (GW)")+
  labs(title = "Wind power capacity")
p3 <- plot_tr_comparison_facet("Cap_Ele_Sto_Bat","Capacity (GW)")+
  labs(title = "Battery capacity")
p4 <- plot_tr_comparison_facet("Cap_Hyd_Ele","Capacity (GW)")+
  labs(title = "Electrolyzer capacity")

# l_tr <- get_legend(p2+theme(legend.text=element_text(size=9),
#                             legend.title=element_blank())+
#                      guides(color=guide_legend(nrow=1)))

p_body <- plot_grid(p1+theme(legend.position="none"),
                    p2+theme(legend.position="none"),
                    p3+theme(legend.position="none"),
                    p4+theme(legend.position="bottom"),
                    ncol=1,align="v",labels=c("a","b","c","d"),label_size=12)


ggsave("output/plot_capacity/Cap_TR_comparison_NA_all.png",
       p_body,width=250,height=380,units="mm",dpi=300,bg="white")



# 電力の使用量-------------------------------------------------------------------------

target_vars <- c("Sec_Ene_Ele_Cur","Sec_Ene_Hyd_Ele")

df_na_facet <- df_all %>%
  filter(Variable %in% target_vars, Region %in% Region_list) %>%
  mutate(TR=str_replace_all(str_trim(as.character(TR)),"–","-"),
         TR=factor(TR,levels=TR_list),
         Region=factor(Region,levels=Region_list))

plot_tr_comparison_facet <- function(var,y){
  d <- df_na_facet %>% filter(Variable==var); if(nrow(d)==0) return(NULL)
  ggplot(d)+
    geom_path(aes(Year,Value,color=TR,group=TR),size=0.4)+
    geom_point(aes(Year,Value,color=TR,shape=TR),size=2.5,stroke=0.7)+
    scale_color_manual(values=line_color,drop=FALSE)+
    scale_shape_manual(values=shape,drop=FALSE)+
    facet_wrap(~Region,nrow=1,scales="free_y",drop=FALSE)+
    labs(y=y,title=var)+MyTheme+
    theme(legend.position="bottom",
          legend.text=element_text(size=10),
          legend.title=element_blank(),
          strip.text=element_text(size=10))
}

p1 <- plot_tr_comparison_facet("Sec_Ene_Ele_Cur","Curtailment (EJ/yr)")+
  labs(title = "Electricity curtailment")
p2 <- plot_tr_comparison_facet("Sec_Ene_Hyd_Ele","Electricity used for electrolysis (EJ/yr)")+
  labs(title = "Electricity consumption for hydrogen production")


# l_tr <- get_legend(p2+theme(legend.text=element_text(size=9),
#                             legend.title=element_blank())+
#                      guides(color=guide_legend(nrow=1)))

p_body <- plot_grid(p1+theme(legend.position="none"),
                    p2+theme(legend.position="bottom"),
                    ncol=1,align="v",labels=c("a","b"),label_size=12)


ggsave("output/plot_eleuse/eleuse_TR_comparison_NA_all.png",
       p_body,width=250,height=180,units="mm",dpi=300,bg="white")



# line_plot ---------------------------------------------------------------

TR_list <- c("AIM-T","PSM-1H","PSM-3H", "PSM-4H", "PSM-6H")

plot_nested_tabset <- list()

# 変数リスト（Cap_Add/Invは除外）
var_list <- df_all %>%
  filter(!str_detect(Variable, "^Cap_Add|^Inv")) %>%
  pull(Variable) %>%
  unique() %>%
  sort()

# 変数→カテゴリの割当
var_class <- tibble(Variable = var_list) %>%
  left_join(set_Sv, by = c("Variable" = "GAMS.code")) %>%
  mutate(
    Category = case_when(
      str_detect(Template.code, "^Capacity\\|") ~ "Capacity",
      str_detect(Template.code, "^Carbon Capture\\|") ~ "Carbon Capture",
      str_detect(Template.code, "^Emissions\\|") ~ "Emissions",
      str_detect(Template.code, "^Secondary Energy\\|Electricity") ~ "Secondary Energy|Electricity",
      str_detect(Template.code, "^Secondary Energy\\|Hydrogen|Secondary Energy Input\\|Electricity\\|Hydrogen") ~ "Secondary Energy|Hydrogen",
      TRUE ~ "Other"
    )
  )

line_color <- c(
  'AIM-T'  = 'black',
  'PSM-1H' = 'indianred1',
  'PSM-3H' = 'deepskyblue1',
  'PSM-4H' = 'darkgoldenrod1',
  "PSM-6H" = "chartreuse2")

shape <- c(
  'AIM-T'  = 16,
  "PSM-1H" = 15,
  "PSM-3H" = 17,
  "PSM-4H" = 18,
  "PSM-6H" = 4)

# カテゴリ情報
category_info <- list(
  "Capacity" = list(folder = "Capacity", ylabel = expression(paste('GW'))),
  "Secondary Energy|Electricity" = list(folder = "Secene_Ele", ylabel = expression(paste('EJ/yr'))),
  "Secondary Energy|Hydrogen" = list(folder = "Secene_Hyd", ylabel = expression(paste('EJ/yr')))
)

# ファイル名用のサニタイズ
sanitize <- function(x) gsub("[/|\\\\: ]", "_", x)

# # 地域一覧
# regions <- df_all %>% 
#   pull(Region) %>% 
#   unique() %>% 
#   sort()

for (reg in regions) {
  base_dir <- file.path("output", sanitize(reg))
  if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)
  
  for (cat in names(category_info)) {
    # カテゴリに属する変数
    var_list_cat <- var_class %>%
      filter(Category == cat) %>%
      pull(Variable)
    
    # 地域/カテゴリの出力フォルダ
    cat_dir <- file.path(base_dir, category_info[[cat]]$folder)
    if (!dir.exists(cat_dir)) dir.create(cat_dir, recursive = TRUE)
    
    ylabel <- category_info[[cat]]$ylabel
    
    for (var in var_list_cat) {
      df_plot <- df_all %>%
        filter(
          Region == reg,
          Variable == var,
          Scenario == "300C",
          Year %in% 2020:2050
        )
      if (nrow(df_plot) == 0) next
      
      df_plot <- df_plot %>%
        mutate(TR = factor(TR, levels = TR_list))
      
      p <- ggplot(df_plot) +
        geom_path(
          aes(x = Year, y = Value, color = TR, group = TR),
          size = 0.4
        ) +
        geom_point(
          aes(x = Year, y = Value, color = TR, shape = TR),
          size = 2.5,
          stroke = 0.7
        ) +
        scale_color_manual(values = line_color, drop = FALSE) +
        scale_shape_manual(values = shape, drop = FALSE) +
        labs(
          y = ylabel,
          title = paste0(var, " — ", reg)
        ) +
        MyTheme +
        theme(
          legend.position = "right",
          legend.text  = element_text(size = 9),
          legend.title = element_blank()
        ) +
        guides(color = guide_legend(nrow = 9))
      
      filename <- file.path(cat_dir, paste0(sanitize(var), ".png"))
      ggsave(filename = filename, plot = p,
             width = 120, height = 100, units = "mm", dpi = 300, bg = "white")
    }
  }
}

# Interconnection capacity ------------------------------------------------
# 基本設定

years    <- seq(2020, 2050, 5)
scenario <- "300C"

TR_list <- c("PSM-1H","PSM-3H", "PSM-4H", "PSM-6H")

tr_map <- tibble(
  TR     = c("PSM-1H","PSM-3H", "PSM-4H", "PSM-6H"),
  Folder = c("2512131220","2512122002", "2512120847", "2512111017")
)

# 対象リージョン定義
region_map <- list(
  USA           = "^USA",
  CAN           = "^CAN",
  MEX           = "^MEX",
  NorthAmerica  = "^(USA|CAN|MEX)"
)

# 色・shape（TR 名をキーに指定）
line_color <- c(
  "PSM-1H" = "indianred1",
  "PSM-3H" = "deepskyblue1",
  "PSM-4H" = "darkgoldenrod1",
  "PSM-6H" = "chartreuse2"
)

shape <- c(
  "PSM-1H" = 15,
  "PSM-3H" = 17,
  "PSM-4H" = 18,
  "PSM-6H" = 4
)

# ガード（事故防止）
stopifnot(all(TR_list %in% names(line_color)))
stopifnot(all(TR_list %in% names(shape)))

# GDX 読み込み（全 TR × 全年）

df_exchange_cap <- map_dfr(1:nrow(tr_map), function(i) {
  
  tr_label <- tr_map$TR[i]
  folder   <- tr_map$Folder[i]
  
  map_dfr(years, function(year) {
    
    file_path <- sprintf(
      "//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output/global2050/NorthAmerica/%s/gams_output/gdx_secondary/prm2sec/%s/%d.gdx",
      folder, scenario, year
    )
    
    cap <- tryCatch(
      rgdx.param(file_path, "CAPACITY"),
      error = function(e) {
        warning(paste("読み込み失敗:", file_path))
        NULL
      }
    )
    
    if (is.null(cap)) return(NULL)
    
    cap %>%
      mutate(across(where(is.factor), as.character)) %>%
      filter(TECH == "t2d_exchange") %>%
      mutate(
        YEAR     = as.numeric(YEAR),
        Year     = year,
        Scenario = scenario,
        TR       = tr_label
      )
  })
})

# 地域別集計関数

make_df_plot <- function(region_name, region_pattern) {
  
  df_exchange_cap %>%
    filter(
      TECH == "t2d_exchange",
      str_detect(REGION, region_pattern),
      Scenario == scenario
    ) %>%
    mutate(
      TR = factor(TR, levels = TR_list)
    ) %>%
    group_by(YEAR, TR) %>%
    summarise(
      CAPACITY = sum(CAPACITY),
      .groups  = "drop"
    ) %>%
    mutate(Region = region_name)
}

df_plot_all <- map_dfr(
  names(region_map),
  ~ make_df_plot(.x, region_map[[.x]])
)

# プロット関数

plot_exchange <- function(df_region) {
  
  ggplot(df_region) +
    geom_line(
      aes(x = YEAR, y = CAPACITY / 1e3, color = TR, group = TR),
      linewidth = 0.6
    ) +
    geom_point(
      aes(x = YEAR, y = CAPACITY / 1e3, color = TR, shape = TR),
      size = 2
    ) +
    geom_hline(yintercept = 0, linewidth = 0.2, color = "grey60") +
    scale_color_manual(
      values = line_color,
      drop   = FALSE
    ) +
    scale_shape_manual(
      values = shape,
      drop   = FALSE
    ) +
    labs(
      x = "Year",
      y = "Interconnection capacity (GW)",
      title = unique(df_region$Region)
    ) +
    MyTheme +
    theme(
      legend.position = "right",
      legend.title    = element_blank(),
      legend.text     = element_text(size = 9),
      plot.title      = element_text(hjust = 0.5, size = 11)
    )
}

# 保存（USA / CAN / MEX / NorthAmerica）

df_plot_all %>%
  group_split(Region) %>%
  walk(function(df_reg) {
    
    region_name <- unique(df_reg$Region)
    
    p <- plot_exchange(df_reg)
    
    ggsave(
      filename = sprintf(
        "output/transmission/g_exchange_%s_%s.png",
        region_name, scenario
      ),
      plot   = p,
      width  = 120,
      height = 100,
      units  = "mm",
      dpi    = 300,
      bg     = "white"
    )
  })


# Capacity ---------------------------------------------------------------

TR_list <- c("AIM-T","PSM-1H","PSM-3H", "PSM-4H", "PSM-6H")
years    <- seq(2020, 2050, 5)
scenario <- "300C"

line_color <- c(
  'AIM-T'  = 'black',
  'PSM-1H' = 'indianred1',
  'PSM-3H' = 'deepskyblue1',
  'PSM-4H' = 'darkgoldenrod1',
  "PSM-6H" = "chartreuse2")

shape <- c(
  "AIM-T"  = 16,
  "PSM-1H" = 15,
  "PSM-3H" = 17,
  "PSM-4H" = 18,
  "PSM-6H" = 4)

target_vars <- c(
  "Cap_Ele_Solar",
  "Cap_Ele_Win",
  "Cap_Ele_Sto_Bat",
  "Cap_Hyd_Ele")

Region_list <- c("NAR", "USA", "CAN", "MEX")

df_na_facet <- df_all %>%
  filter(Variable %in% target_vars,Region %in% Region_list) %>%
  mutate(
#    TR     = factor(TR, levels = TR_list),
    Region = factor(Region, levels = Region_list))
 

plot_tr_comparison_facet <- function(var_name, ylabel) {
  
  ggplot(
    df_na_facet %>% filter(Variable == var_name)
  ) +
    geom_path(
      aes(x = Year, y = Value, color = TR, group = TR),
      size = 0.4
    ) +
    geom_point(
      aes(x = Year, y = Value, color = TR, shape = TR),
      size = 2.5,
      stroke = 0.7
    ) +
    scale_color_manual(values = line_color) +
    scale_shape_manual(values = shape) +
    facet_wrap(vars(Region),nrow=1,scales="free_y") +
    labs(
      y = ylabel,
      title = var_name
    ) +
    MyTheme +
    theme(
      legend.position = "bottom",
      legend.text  = element_text(size = 9),
      legend.title = element_blank(),
      strip.text   = element_text(size = 10)
    )
}

for (var in target_vars) {
  
  p <- plot_tr_comparison_facet(
    var_name = var,
    ylabel   = expression(paste("Capacity (GW)"))
  )
  
  ggsave(
    filename = file.path("output/plot_capacity/", paste0(var, "_TR_comparison_NA.png")),
    plot     = p,
    width    = 300,
    height   = 140,
    units    = "mm",
    dpi      = 300,
    bg       = "white"
  )
}


# Sec_Ele ---------------------------------------------------------------

TR_list <- c("AIM-T","PSM-1H","PSM-3H","PSM-4H","PSM-6H")
years    <- seq(2020, 2050, 5)
scenario <- "300C"

line_color <- c(
  "AIM-T"="black",
  "PSM-1H"="indianred1",
  "PSM-3H"="deepskyblue1",
  "PSM-4H"="darkgoldenrod1",
  "PSM-6H"="chartreuse2")

target_vars <- c("Sec_Ene_Ele_Cur","Sec_Ene_Hyd_Ele")
Region_list <- c("NAR","USA","CAN","MEX")

df_na_bar <- df_all %>%
  filter(
    Variable %in% target_vars,
    Region %in% Region_list,
    Year == 2050
  ) %>%
  mutate(
    TR = as.character(TR),
    TR = str_trim(TR),
    TR = str_replace_all(TR,"–","-"),
    TR = factor(TR, levels = TR_list),
    Region = factor(Region, levels = Region_list)
  )

plot_tr_bar_facet <- function(var_name, ylabel) {
  
  df_tmp <- df_na_bar %>% filter(Variable == var_name)
  if (nrow(df_tmp) == 0) return(NULL)
  
  ggplot(df_tmp) +
    geom_col(
      aes(x = TR, y = Value, fill = TR),
      width = 0.7
    ) +
    scale_fill_manual(values = line_color, drop = FALSE) +
    facet_wrap(
      ~Region,
      nrow = 1,
      scales = "free_y",
      drop = FALSE
    ) +
    labs(
      y = ylabel,
      x = NULL,
      title = paste0(var_name, " (2050)")
    ) +
    MyTheme +
    theme(
      legend.position = "bottom",
      legend.text  = element_text(size = 9),
      legend.title = element_blank(),
      strip.text   = element_text(size = 10)
    )
}

for (var in target_vars) {
  
  p <- plot_tr_bar_facet(
    var_name = var,
    ylabel   = expression(paste("Electricity consumption (EJ/yr)"))
  )
  
  if (is.null(p)) next
  
  ggsave(
    filename = file.path(
      "output/plot_eleuse/",
      paste0(var, "_TR_bar_2050_NA.png")
    ),
    plot   = p,
    width  = 300,
    height = 140,
    units  = "mm",
    dpi    = 300,
    bg     = "white"
  )
}
