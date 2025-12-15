
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
odir <- paste0(cdir,'/output/NorthAmerica')
basedir <- 'AIMPSY-master'
plot_year <- '2050'
#time_resol <- '12DAY'
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
gams_sys_dir <- system("which gams --skip-alias|xargs dirname", intern = TRUE)
igdx(gams_sys_dir)

options(bitmapType='cairo')

year_all <- seq(2020,2050,by=5)

# 1HOUR 
time_resol <- '1HOUR'
reg_in <- 'NorthAmerica'
date <- '2512131220'
ddir <- paste0(aimpsy,'/',scenario,'/',reg_in,'/',date)  # output path

df_1h <- rgdx.param(str_c(ddir,'/main/merged_output.gdx'),'iamc_gdx') %>% 
  rename(Scenario='Sc',Region='Sr',Variable='Sv',Year='Y5',Value='iamc_gdx') %>% 
  mutate(Year=as.numeric(as.character(Year)),Region=as.character(Region),Scenario=as.character(Scenario)) %>% 
  pivot_wider(names_from=Year,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Scenario,Variable,Region),names_to='Year',values_to='Value') %>% 
  pivot_wider(names_from=Variable,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Scenario,Region,Year),names_to='Variable',values_to='Value') %>% 
  filter(Value != 'N/A') %>% filter(!is.na(Value)) %>% 
  mutate(Year=as.numeric(Year), Value=as.numeric(Value), TR="1HOUR")

# 3HOUR 
time_resol <- '3HOUR'
reg_in <- 'NorthAmerica'
date <- '2512122002'
ddir <- paste0(aimpsy,'/',scenario,'/',reg_in,'/',date)  # output path

df_3h <- rgdx.param(str_c(ddir,'/main/merged_output.gdx'),'iamc_gdx') %>% 
  rename(Scenario='Sc',Region='Sr',Variable='Sv',Year='Y5',Value='iamc_gdx') %>% 
  mutate(Year=as.numeric(as.character(Year)),Region=as.character(Region),Scenario=as.character(Scenario)) %>% 
  pivot_wider(names_from=Year,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Scenario,Variable,Region),names_to='Year',values_to='Value') %>% 
  pivot_wider(names_from=Variable,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Scenario,Region,Year),names_to='Variable',values_to='Value') %>% 
  filter(Value != 'N/A') %>% filter(!is.na(Value)) %>% 
  mutate(Year=as.numeric(Year), Value=as.numeric(Value), TR="3HOUR")

# 4HOUR 
time_resol <- '4HOUR'
reg_in <- 'NorthAmerica'
date <- '2512120847'
ddir <- paste0(aimpsy,'/',scenario,'/',reg_in,'/',date)  # output path

df_4h <- rgdx.param(str_c(ddir,'/main/merged_output.gdx'),'iamc_gdx') %>% 
  rename(Scenario='Sc',Region='Sr',Variable='Sv',Year='Y5',Value='iamc_gdx') %>% 
  mutate(Year=as.numeric(as.character(Year)),Region=as.character(Region),Scenario=as.character(Scenario)) %>% 
  pivot_wider(names_from=Year,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Scenario,Variable,Region),names_to='Year',values_to='Value') %>% 
  pivot_wider(names_from=Variable,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Scenario,Region,Year),names_to='Variable',values_to='Value') %>% 
  filter(Value != 'N/A') %>% filter(!is.na(Value)) %>% 
  mutate(Year=as.numeric(Year), Value=as.numeric(Value), TR="4HOUR")

# 6HOUR 
time_resol <- '6HOUR'
reg_in <- 'NorthAmerica'
date <- '2512111017'
ddir <- paste0(aimpsy,'/',scenario,'/',reg_in,'/',date)  # output path

df_4h <- rgdx.param(str_c(ddir,'/main/merged_output.gdx'),'iamc_gdx') %>% 
  rename(Scenario='Sc',Region='Sr',Variable='Sv',Year='Y5',Value='iamc_gdx') %>% 
  mutate(Year=as.numeric(as.character(Year)),Region=as.character(Region),Scenario=as.character(Scenario)) %>% 
  pivot_wider(names_from=Year,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Scenario,Variable,Region),names_to='Year',values_to='Value') %>% 
  pivot_wider(names_from=Variable,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Scenario,Region,Year),names_to='Variable',values_to='Value') %>% 
  filter(Value != 'N/A') %>% filter(!is.na(Value)) %>% 
  mutate(Year=as.numeric(Year), Value=as.numeric(Value), TR="6HOUR")

# AIMTech 
model <- '//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output'
scenario <- 'global2100_PSModel'
time_resol <- 'AIM-T'
#reg_in <- 'NorthAmerica'
date <- '2512081616'
ddir <- paste0(model,'/',scenario,'/',date)  # output path

df_AIMT <- rgdx.param(str_c(ddir,'/main/merged_output.gdx'),'iamc_gdx') %>%   
  filter(Y5%in%c(2005:2050)) %>% 
  rename(Scenario='Sc',Region='Sr',Variable='Sv',Year='Y5',Value='iamc_gdx') %>% 
  mutate(Year=as.numeric(as.character(Year)),Region=as.character(Region),Scenario=as.character(Scenario)) %>% 
  pivot_wider(names_from=Year,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Scenario,Variable,Region),names_to='Year',values_to='Value') %>% 
  pivot_wider(names_from=Variable,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Scenario,Region,Year),names_to='Variable',values_to='Value') %>% 
  filter(Value != 'N/A') %>% filter(!is.na(Value)) %>% 
  mutate(Year=as.numeric(Year), Value=as.numeric(Value), TR="AIM-T")

# merge -------------------------------------------------------------------
TR_list <- c('AIM-T','1HOUR','3HOUR','4HOUR','6HOUR')
Region_list <- c("USA","CAN","MEX")

df_TR_individual <- bind_rows(df_AIMT,df_1h, df_3h,df_4h,df_6h) %>%
  filter(Scenario == '300C',Region %in% Region_list) %>% 
  mutate(TR = factor(TR, levels = TR_list))

df_TR_total <- df_TR_individual %>% 
  group_by(Scenario, Variable, Year,TR) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Region = "N.America")  # 合算したことがわかる地域名

df_TR_Region <- bind_rows(df_TR_individual, df_TR_total) %>%
  

set_Sv <- rgdx.set(str_c(ddir,'/main/merged_output.gdx'),'Sv',te=TRUE) %>% rename(GAMS.code=1,Template.code=2)
set_Su <- rgdx.set(str_c(ddir,'/main/merged_output.gdx'),'Su',te=TRUE) %>% rename(GAMS.code=1,Unit=2)

scen_mat <- read_csv(str_c(ddir,'/main/scenario_table.csv'))

plt <- list()
# source(str_c(cdir,'/',basedir,'/tools/Rplot/func_plot.R'))

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


# 世界全体の排出経路  ----------------------------

line_color<-c('black','indianred2','deepskyblue2')

g_emi_path<- df_AIMT %>% 
  filter(Sr %in% "World")%>% 
  select(-Sr) %>% 
  filter(Scenario%in%c('Baseline','300C','700C')) %>% 
  filter(Variable == 'Emi_CO2_Ene') %>% 
  ggplot() +
  geom_path(aes(x=Y5,y=Value/1000,color=Scenario,group=Scenario),
            linewidth=1) +
  scale_color_manual(values=line_color) +
  labs(y = expression(paste({CO[2]},'emissions (Gt-',{CO[2]},'',{yr^-1},')'))) +
  ylim(-10,50) +
  MyTheme +
  theme(legend.text=element_text(size=15),
        axis.title.y = element_text(size=15),
  )

g_emi_path_ele<- df_AIMT %>% 
  filter(Sr %in% "World")%>% 
  select(-Sr) %>% 
  filter(Scenario%in%c('Baseline','300C','700C')) %>% 
  filter(Variable == 'Emi_CO2_Ene_Sup_Ele') %>% 
  ggplot() +
  geom_path(aes(x=Y5,y=Value/1000,color=Scenario,group=Scenario),
            linewidth=1) +
  scale_color_manual(values=line_color) +
  labs(y = expression(paste({CO[2]},'emissions (Gt-',{CO[2]},'',{yr^-1},')'))) +
  ylim(-10,50) +
  MyTheme +
  theme(legend.text=element_text(size=15),
        axis.title.y = element_text(size=15),
  )

l_emi <- plot_grid(get_legend(
  g_emi_path+theme(legend.text = element_text(size = 9))
  +guides(fill = guide_legend(nrow = 1))),nrow=1)
path_ele <- plot_grid(g_emi_path+theme(legend.position='none'),g_emi_path_ele+theme(legend.position='none'),
                     nrow=1,rel_widths=c(1, 1),labels=c('a','b'),label_size=12)
finene_plot <- plot_grid(path_ele,l_emi,ncol=1,rel_heights=c(8,1))

ggsave(filename='output/Fig_emission_pathway',plot=finene_plot,width=160,height=120,units='mm',dpi=300,bg='white')


# power generation------------------------------------------------------------------------

plt <- set_plot(secele)

df_secele <- df_TR_Asia %>%
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

ggsave(filename='output/TR_Asia_secele.png',plot=g_secele,width=240,height=180,units='mm',dpi=300,bg='white')



# line_plot ---------------------------------------------------------------

plot_nested_tabset <- list()

# 変数リスト（Cap_Add/Invは除外）
var_list <- df_TR_Asia %>%
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

line_color <- c('black','indianred1','deepskyblue1','darkgoldenrod1','green1')
shape <- c(16,4,17,15)

# カテゴリ情報
category_info <- list(
  "Capacity" = list(folder = "Capacity", ylabel = expression(paste('GW'))),
  "Secondary Energy|Electricity" = list(folder = "Secene_Ele", ylabel = expression(paste('EJ/yr'))),
  "Secondary Energy|Hydrogen" = list(folder = "Secene_Hyd", ylabel = expression(paste('EJ/yr')))
)

# ファイル名用のサニタイズ
sanitize <- function(x) gsub("[/|\\\\: ]", "_", x)

# 地域一覧
regions <- df_TR_Asia %>% pull(Region) %>% unique() %>% sort()

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
      df_plot <- df_TR_Asia %>%
        filter(
          Region == reg,
          Variable == var,
          Scenario == "300C",
          Year %in% 2020:2050
        )
      if (nrow(df_plot) == 0) next
      
      p <- ggplot(df_plot) +
        geom_path(aes(x = Year, y = Value, color = TR, group = TR), size = 0.4) +
        geom_point(aes(x = Year, y = Value, color = TR, shape = TR), size = 2.5, stroke = 0.7) +
        scale_color_manual(values = line_color) +
        scale_shape_manual(values = shape) +
        labs(
          y = ylabel,
          title = paste0(var, " — ", reg)   # ← タイトルに地域名
        ) +
        MyTheme +
        theme(
          legend.position = "right",
          legend.text = element_text(size = 9),
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

# 対象の年
years <- seq(2020, 2050, 5)
scenario <- "300C"

# 時間解像度ごとのフォルダ名とラベルの対応
tr_map <- tibble(
  TR = c("3HOUR", "12DAY", "1DAY"),
  Folder = c("2507141634", "2507141451", "2507141442")
)

# すべてのTRに対して処理
df_exchange_cap <- map_dfr(1:nrow(tr_map), function(i) {
  tr_label <- tr_map$TR[i]
  folder <- tr_map$Folder[i]
  
  # 各年に対して処理
  map_dfr(years, function(year) {
    
    # GDXファイルパス
    file_path <- sprintf("//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output/global2050/China/%s/gams_output/gdx_secondary/prm2sec/%s/%d.gdx", 
                         folder, scenario, year)
    
    cap <- tryCatch({
      rgdx.param(file_path, "CAPACITY")
    }, error = function(e) {
      warning(paste("読み込み失敗:", file_path))
      return(NULL)
    })
    
    if (!is.null(cap)) {
      cap %>%
        mutate(across(where(is.factor), as.character)) %>%
        filter(TECH == "t2d_exchange") %>%
        mutate(Year = year, Scenario = scenario, TR = tr_label)
    } else {
      NULL
    }
  })
})

# カラーとマーカーの指定
line_color <- c('indianred1', 'deepskyblue1', 'darkgoldenrod1')  # 順に 3HOUR, 12DAY, 1DAY
shape <- c(16, 4, 17)

# 時間解像度の順序を明示的に指定
df_plot <- df_exchange_cap %>%
  mutate(YEAR = as.numeric(YEAR),
         TR = factor(TR, levels = c("3HOUR", "12DAY", "1DAY"))) %>%
  filter(TECH == "t2d_exchange", str_starts(REGION, "CHN"), Scenario == "300C") %>%
  group_by(YEAR, TR) %>%
  summarise(CAPACITY = sum(CAPACITY), .groups = "drop")

# プロット作成
g_exchange_china_300C_multi <- ggplot(df_plot) +
  geom_line(aes(x = YEAR, y = CAPACITY / 10^3, color = TR, group = TR), linewidth = 0.6) +
  geom_point(aes(x = YEAR, y = CAPACITY / 10^3, color = TR, shape = TR), size = 2) +
  geom_hline(yintercept = 0, linewidth = 0.2, color = 'grey60') +
  scale_color_manual(values = line_color) +
  scale_shape_manual(values = shape) +
  labs(
    x = "Year",
    y = "Interconnection capacity (GW)"  ) +
  MyTheme +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 11),
    legend.title = element_blank()
  )

# 保存
ggsave(filename = "output/g_exchange_china_300C.png",
       plot = g_exchange_china_300C_multi,
       width = 120, height = 100, units = "mm", dpi = 300, bg = "white")