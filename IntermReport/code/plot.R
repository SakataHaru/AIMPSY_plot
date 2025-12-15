
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

# import data -------------------------------------------------------------
# AIMTech 
model <- '//SY_3600_01/atfs02/Individual/hsakata/AIMEnduse/output'
scenario <- 'global2100_PSModel'
time_resol <- 'AIM-T'
#reg_in <- 'NorthAmerica'
date <- '2512081616'
ddir <- paste0(model,'/',scenario,'/',date)  # output path

df_AIMT <- rgdx.param(str_c(ddir,'/main/merged_output.gdx'),'iamc_gdx') %>%   
  filter(Y5%in%c(2020:2050)) %>% 
  rename(Scenario='Sc',Region='Sr',Variable='Sv',Year='Y5',Value='iamc_gdx') %>% 
  mutate(Year=as.numeric(as.character(Year)),Region=as.character(Region),Scenario=as.character(Scenario)) %>% 
  pivot_wider(names_from=Year,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Scenario,Variable,Region),names_to='Year',values_to='Value') %>% 
  pivot_wider(names_from=Variable,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Scenario,Region,Year),names_to='Variable',values_to='Value') %>% 
  filter(Value != 'N/A') %>% filter(!is.na(Value)) %>% 
  mutate(Year=as.numeric(Year), Value=as.numeric(Value), TR="AIM-T")

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

# gams_sys_dir <- system("which gams --skip-alias|xargs dirname", intern = TRUE)
# igdx(gams_sys_dir)

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
  mutate(Year=as.numeric(Year), Value=as.numeric(Value), TR="PSM-1H")

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
  mutate(Year=as.numeric(Year), Value=as.numeric(Value), TR="PSM-3H")

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
  mutate(Year=as.numeric(Year), Value=as.numeric(Value), TR="PSM-4H")

# 6HOUR 
time_resol <- '6HOUR'
reg_in <- 'NorthAmerica'
date <- '2512111017'
ddir <- paste0(aimpsy,'/',scenario,'/',reg_in,'/',date)  # output path

df_6h <- rgdx.param(str_c(ddir,'/main/merged_output.gdx'),'iamc_gdx') %>% 
  rename(Scenario='Sc',Region='Sr',Variable='Sv',Year='Y5',Value='iamc_gdx') %>% 
  mutate(Year=as.numeric(as.character(Year)),Region=as.character(Region),Scenario=as.character(Scenario)) %>% 
  pivot_wider(names_from=Year,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Scenario,Variable,Region),names_to='Year',values_to='Value') %>% 
  pivot_wider(names_from=Variable,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Scenario,Region,Year),names_to='Variable',values_to='Value') %>% 
  filter(Value != 'N/A') %>% filter(!is.na(Value)) %>% 
  mutate(Year=as.numeric(Year), Value=as.numeric(Value), TR="PSM-6H")

# merge -------------------------------------------------------------------
TR_list <- c('AIM-T','PSM-1H','PSM-3H','PSM-4H','PSM-6H')
Region_list <- c("USA","CAN","MEX")

df_TR_individual <- bind_rows(df_AIMT,df_1h,df_3h,df_4h,df_6h) %>%
  filter(Scenario == '300C',Region %in% Region_list) %>% 
  mutate(TR = factor(TR, levels = TR_list))

df_TR_total <- df_TR_individual %>% 
  group_by(Scenario, Variable, Year,TR) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Region = "NAR")  # 合算したことがわかる地域名

df_TR_Region <- bind_rows(df_TR_individual, df_TR_total) %>% 
  mutate(Region = factor(Region, levels = c("NAR","USA","CAN","MEX")))


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

df_secele <- df_TR_Region %>%
  filter(Scenario %in% c('Baseline','300C')) %>%
  filter(Variable %in% secele$Variable, Year == 2050) %>%
  mutate(
    Variable = factor(Variable, levels = rev(secele$Variable)))

g_secele <- df_secele %>%
  ggplot() +
  geom_bar(aes(x=TR,y=Value,fill=Variable),stat='identity') +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='') +
  facet_wrap(vars(Region),nrow=1,scales="free_y") +
  guides(fill=guide_legend(reverse=FALSE)) +
  labs(y=expression(paste('Power generation (EJ ',{yr^-1},')'))) +
  MyTheme +
  theme(
    title = element_text(size = 10),
    legend.position = "right", 
    legend.text = element_text(size = 10),
    legend.title = element_blank())

ggsave(filename='output/plot_power/NorthAmerica_secele.png',
       plot=g_secele,width=230,height=140,units='mm',dpi=300,bg='white')


# 設備容量の比較-------------------------------------------------------------------------

TR_list <- c("AIM-T","PSM-1H","PSM-3H","PSM-4H","PSM-6H")
Region_list <- c("NAR","USA","CAN","MEX")
line_color <- c("AIM-T"="black","PSM-1H"="indianred1","PSM-3H"="deepskyblue1","PSM-4H"="darkgoldenrod1","PSM-6H"="chartreuse2")
shape <- c("AIM-T"=16,"PSM-1H"=15,"PSM-3H"=17,"PSM-4H"=18,"PSM-6H"=4)
target_vars <- c("Cap_Ele_Solar","Cap_Ele_Win","Cap_Ele_Sto_Bat","Cap_Hyd_Ele")

df_na_facet <- df_TR_Region %>%
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

TR_list <- c("AIM-T","PSM-1H","PSM-3H","PSM-4H","PSM-6H")
Region_list <- c("NAR","USA","CAN","MEX")
line_color <- c("AIM-T"="black","PSM-1H"="indianred1","PSM-3H"="deepskyblue1","PSM-4H"="darkgoldenrod1","PSM-6H"="chartreuse2")
shape <- c("AIM-T"=16,"PSM-1H"=15,"PSM-3H"=17,"PSM-4H"=18,"PSM-6H"=4)
target_vars <- c("Sec_Ene_Ele_Cur","Sec_Ene_Hyd_Ele")

df_na_facet <- df_TR_Region %>%
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
var_list <- df_TR_Region %>%
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

# 地域一覧
regions <- df_TR_Region %>% 
  pull(Region) %>% 
  unique() %>% 
  sort()

for (reg in regions) {
  base_dir <- file.path("output", sanitize(reg))
  if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)gv
  
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
      df_plot <- df_TR_Region %>%
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

df_na_facet <- df_TR_Region %>%
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

df_na_bar <- df_TR_Region %>%
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
