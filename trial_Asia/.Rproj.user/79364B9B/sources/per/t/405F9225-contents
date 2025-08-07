
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

# setting -----------------------------------------------------------------

aimpsy <- '//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output'
scenario <- 'global2050'

cdir <- getwd()
pdir <- paste0(cdir,'/prog/inc_prog')
odir <- paste0(cdir,'/output/trial_Asia')
basedir <- 'AIMPSY-master'
plot_year <- '2050'
time_resol <- '12DAY'
gams_sys_dir <- 'C:/GAMS/38'

# get gams system directory
igdx(gams_sys_dir)

Sys.setlocale("LC_TIME", "en_US.UTF-8")

# # define geometry
# geometry_NEA <- st_read(paste0(cdir,'/define/NEA_wo.geojson')) %>% 
#   rename(Sr=N_PSY)
# geometry_NEA_w <- st_read(paste0(cdir,'/define/NEA_w.geojson')) %>% 
#   transmute(Sr=N_PSY,geometry)


# import data -------------------------------------------------------------

# PSY Asia_all ------------------------------------------------------------
reg_in <- 'Asia'
date <- '2506022020'
ddir <- paste0(aimpsy,'/',scenario,'/',reg_in,'/',date)  # output path


gams_sys_dir <- system("which gams --skip-alias|xargs dirname", intern = TRUE)
igdx(gams_sys_dir)

options(bitmapType='cairo')

year_all <- seq(2020,2050,by=5)

Asia_list <- c("JPN", "CHN", "KOR", "IND", "IDN",  
               "THA", "MYS", "VNM", "XSE", "XSA", "XEA")

df_AA <- rgdx.param(str_c(ddir,'/main/merged_output.gdx'),'iamc_gdx') %>% 
  rename(Scenario='Sc',Region='Sr',Variable='Sv',Year='Y5',Value='iamc_gdx') %>% 
  mutate(Year=as.numeric(as.character(Year)),Region=as.character(Region),Scenario=as.character(Scenario)) %>% 
  pivot_wider(names_from=Year,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Scenario,Variable,Region),names_to='Year',values_to='Value') %>% 
  pivot_wider(names_from=Variable,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Scenario,Region,Year),names_to='Variable',values_to='Value') %>% 
  filter(Value != 'N/A') %>% filter(!is.na(Value)) %>% 
  mutate(Year=as.numeric(Year), Value=as.numeric(Value), Model="PSY_AA") %>% 
  filter(Region%in%Asia_list)

df_AA_tot <- df_AA %>%
  group_by(Scenario, Model, Variable, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Region = "ASIA")  # 合算したことがわかる地域名

set_Sv <- rgdx.set(str_c(ddir,'/main/merged_output.gdx'),'Sv',te=TRUE) %>% rename(GAMS.code=1,Template.code=2)
set_Su <- rgdx.set(str_c(ddir,'/main/merged_output.gdx'),'Su',te=TRUE) %>% rename(GAMS.code=1,Unit=2)

scen_mat <- read_csv(str_c(ddir,'/main/scenario_table.csv'))

plt <- list()
# source(str_c(cdir,'/',basedir,'/tools/Rplot/func_plot.R'))

# PSY region ------------------------------------------------------------
# 各国に対応する date（確定済み）
region_date <- tribble(
  ~Region, ~Date,
  "JPN", "2506021833",
  "CHN", "2506021646",
  "KOR", "2506021839",
  "IND", "2506022007",
  "IDN", "2506022011",
  "THA", "2506022015",
  "MYS", "2506022017",
  "VNM", "2506021952",
  "XSE", "2506021955",
  "XSA", "2506022000",
  "XEA", "2506022004"
) %>% deframe()

# 全データ格納リスト
df_reg_list <- list()

# 各国ごとに読み込み
for (reg_in in names(region_date)) {
  date <- region_date[reg_in]
  
  # ★ パスに使う用：Asia_ を付ける
  reg_folder <- paste0("Asia_", reg_in)
  ddir <- paste0(aimpsy, '/', scenario, '/', reg_folder, '/', date)
  
  message(glue::glue("読み込み中: {reg_in}（パス: {reg_folder}, date: {date}）"))
  
  # 読み込みと整形
  df_tmp <- rgdx.param(str_c(ddir, '/main/merged_output.gdx'), 'iamc_gdx') %>%
    rename(Scenario = 'Sc', Region = 'Sr', Variable = 'Sv', Year = 'Y5', Value = 'iamc_gdx') %>%
    mutate(Year = as.numeric(as.character(Year)),
           Region = as.character(Region),
           Scenario = as.character(Scenario)) %>%
    pivot_wider(names_from = Year, values_from = Value, values_fill = 0) %>%
    pivot_longer(cols = -c(Scenario, Variable, Region), names_to = 'Year', values_to = 'Value') %>%
    pivot_wider(names_from = Variable, values_from = Value, values_fill = 0) %>%
    pivot_longer(cols = -c(Scenario, Region, Year), names_to = 'Variable', values_to = 'Value') %>%
    filter(!is.na(Value)) %>%
    mutate(Year = as.numeric(Year), Value = as.numeric(Value)) %>% 
    filter(Region%in%Asia_list)
  
  df_tmp$Model <- "PSY_RE"
  df_reg_list[[reg_in]] <- df_tmp
}

# 統合
df_RE <- bind_rows(df_reg_list)

df_RE_tot <- df_RE %>%
  group_by(Scenario, Model, Variable, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Region = "ASIA")  # 合算したことがわかる地域名


# AIMTech ------------------------------------------------------------
aimpsy <- '//SY_3600_01/atfs02/Individual/hsakata/AIMEnduse/output'
scenario <- 'global2100_PSModel'

reg_in <- ''
date <- '2505211647'
ddir <- paste0(aimpsy,'/',scenario,'/',date)  # output path

df_AIMT <- rgdx.param(str_c(ddir,'/main/merged_output.gdx'),'iamc_gdx') %>% 
  rename(Scenario='Sc',Region='Sr',Variable='Sv',Year='Y5',Value='iamc_gdx') %>% 
  mutate(Year=as.numeric(as.character(Year)),Region=as.character(Region),Scenario=as.character(Scenario)) %>% 
  pivot_wider(names_from=Year,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Scenario,Variable,Region),names_to='Year',values_to='Value') %>% 
  pivot_wider(names_from=Variable,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Scenario,Region,Year),names_to='Variable',values_to='Value') %>% 
  filter(Value != 'N/A') %>% filter(!is.na(Value)) %>% 
  mutate(Year=as.numeric(Year), Value=as.numeric(Value), Model="AIMT") %>% 
  filter(Region%in%Asia_list)

df_AIMT_tot <- df_AIMT %>%
  group_by(Scenario, Model, Variable, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Region = "ASIA")  # 合算したことがわかる地域名


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

source('prog/dataframe.R')

year_all <- seq(2020,2050,by=5)


# power generation------------------------------------------------------------------------

plt <- set_plot(secele)

df_secele <- bind_rows(df_AA, df_RE,df_AIMT) %>%
  filter(Scenario == '300C') %>%
  filter(Variable %in% secele$Variable, Year == 2050) %>%
  mutate(
    Variable = factor(Variable, levels = rev(secele$Variable)),
    Region = factor(Region, levels = Asia_list)  # 並び順指定！
  )

df_secele_tot <- bind_rows(df_AA_tot, df_RE_tot, df_AIMT_tot) %>% 
  filter(Scenario == '300C') %>%
  filter(Variable %in% secele$Variable, Year == 2050) %>%
  mutate(
    Variable = factor(Variable, levels = rev(secele$Variable))
  )
  
g_secele <- bind_rows(df_secele, df_secele_tot) %>%
  ggplot() +
  geom_bar(aes(x=Model,y=Value,fill=Variable),stat='identity') +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='') +
  facet_wrap(vars(Region),nrow=3,scales="free_y") +
  guides(fill=guide_legend(reverse=FALSE)) +
  labs(y=expression(paste('Power generation (EJ ',{yr^-1},')'))) +
  MyTheme +
  theme(
    legend.position = "bottom", 
    legend.text = element_text(size = 10),
    legend.title = element_blank())

ggsave(filename='output/secele.png',plot=g_secele,width=150,height=200,units='mm',dpi=300,bg='white')
