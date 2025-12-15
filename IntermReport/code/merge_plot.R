
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


# setting -----------------------------------------------------------------

aimpsy <- '//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output'
scenario <- 'global2050'
date <- '2507071454'
reg_in <- 'China'
cdir <- getwd()
ddir <- paste0(aimpsy,'/',scenario,'/',reg_in,'/',date)  # output path
pdir <- paste0(cdir,'/prog/inc_prog')
odir <- paste0(cdir,'/output/TR_China')
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

# import scenario matrix
scen_mat_load <- read_csv(paste0(ddir,'/main/scenario_table.csv')) %>% 
  filter(Scenario %in% c('Baseline', '300C', '700C'))

# extract infeasible scenario
scen_inf <- scen_mat_load %>% filter(Status=='Infeasible')
scen_mat <- scen_mat_load %>% filter(Status!='Infeasible') 

# import AIM/PSY output
df_all_hr <- rgdx.param(paste0(ddir,'/main/hr/',plot_year,'.gdx'),'data_out_hr') %>% 
  mutate(Sc = as.character(Sc)) %>%
  filter(Sc %in% c('Baseline', '300C', '700C')) %>% 
  filter(Sc %in% scen_mat$Scenario) %>% 
  mutate(across(where(is.factor), ~as.character(.))) %>%
  rename(value = data_out_hr)

df_all_yr <- rgdx.param(paste0(ddir,'/main/merged_output.gdx'),'data_all_yr') %>% 
  mutate(Sc = as.character(Sc)) %>%
  filter(Sc %in% c('Baseline', '300C', '700C')) %>% 
  mutate(across(where(is.factor), ~as.character(.))) %>%
  mutate(Sy = as.numeric(Sy)) %>% 
  rename(value = data_all_yr) %>% 
  mutate(Y5 = ceiling(Sy - 2001) %/% 5 * 5 + 2005) %>% 
  group_by(Sc, Sv, Sr, Y5) %>% 
  reframe(value = mean(value)) %>%
  replace_na(list(value = 0)) %>% 
  pivot_wider(names_from = Y5, values_from = value, values_fill = 0) %>%
  pivot_longer(cols = -c(Sc, Sv, Sr), names_to = 'Y5', values_to = 'value', names_transform = as.numeric)

# scenario category
scen_cat <- scen_mat %>% 
  transmute(Sc=Scenario,cat1=scen_dpol,cat2=scen_tpol)

# timeslices
T <- rgdx.set(paste0(ddir,'/main/hr/2050.gdx'),'St')


# load modules ------------------------------------------------------------

# import variable definition
source(paste0(cdir,'/define/var.R'))

# import plot function
source(paste0(pdir,'/func_analysis.R'))


###########################################################################
# plot
###########################################################################


# Interconnection capacity ------------------------------------------------

df_lnkcap <- foreach (i=scen_mat_load$Scenario) %do% {
  tmp <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_lnk_cap') %>% 
    mutate(across(where(is.factor),~as.character(.))) %>%
    mutate(YEAR=as.numeric(YEAR)) %>% 
    filter(str_detect(PW_LNK,'.*-AC_.*-AC$')) %>% 
    mutate(Sc=i) %>% 
    mutate(Y5=ceiling(YEAR-2001)%/%5*5+2005) %>% 
    group_by(Sc,PW_LNK,Y5) %>% 
    reframe(value=mean(prm2sec_lnk_cap))
} %>% bind_rows()

df_lnkcap_agg <- df_lnkcap %>%
  separate(PW_LNK,sep='_',into=c('N0','N1'),remove=FALSE) %>% 
  mutate(Sv=case_when(str_detect(N0,'CN-')&str_detect(N1,'CN-')~'China domestic') %>% 
                      # str_detect(N0,'JP-')&str_detect(N1,'JP-')~'Japan domestic',
                      # str_detect(N0,'CN-')&str_detect(N1,'MN-')~'China/Mongolia',
                      # str_detect(N0,'CN-')&str_detect(N1,'KR-')~'China/Korea',
                      # str_detect(N0,'KR-')&str_detect(N1,'JP-')~'Korea/Japan')) %>% 
  group_by(Sc,Sv,Y5) %>% 
  reframe(value=sum(value)) %>% 
         Flag=case_when(str_detect(Sv,'domestic')~'domestic',
                        !str_detect(Sv,'domestic')~'international')) %>% 
  filter(Sc%in%c('Baseline', '300C', '700C'))

g_china_domestic_300C <- df_lnkcap_agg %>%
  filter(Sv == "China domestic", Sc == "300C") %>%
  ggplot() +
  geom_line(aes(x = Y5, y = value / 10^3), color = '#d6982f', linewidth = 0.6) +
  geom_point(aes(x = Y5, y = value / 10^3), color = '#d6982f', shape = 16, size = 2) +
  geom_hline(yintercept = 0, linewidth = 0.2, color = 'grey60') +
  labs(
    x = "Year",
    y = "Interconnection capacity (GW)",
    title = "China domestic transmission (Scenario: 300C)"
  ) +
  plot_theme +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 11)
  )

# プロット表示
plot(g_china_domestic_300C)

ggsave(filename='output/China_domestic_300C.png', 
       plot = g_china_domestic_300C, width = 100,height = 80,units = "mm",dpi = 300, bg = "white")

# -------------------------------------------------------------------------



g_Fig_2_1_0 <- df_lnkcap_agg %>%
  # mutate(Sv=factor(Sv,levels=c('China domestic',
  #                              'Japan domestic',
  #                              'China/Mongolia',
  #                              'China/Korea',
  #                              'Korea/Japan'))) %>% 
  # pivot_wider(names_from=Y5,values_from=value,values_fill=0) %>%
  # pivot_longer(cols=-c(Sc,Sv),names_to='Y5',values_to='value',names_transform=as.numeric) %>% 
  ggplot() +
  geom_line(aes(x=Y5,y=value/10^3,color=Sv,linetype=Sc),stat='identity',linewidth=0.3) +
  geom_point(aes(x=Y5,y=value/10^3,color=Sv,shape=Sc)) +
  geom_hline(yintercept=0,linewidth=0.2,color='grey60') +
  # scale_color_manual(values=c('China domestic'='#5ea6f2',
  #                            'Japan domestic'='#1a66b8',
  #                            'China/Mongolia'='#fcca3f',
  #                            'China/Korea'='#e6a73c',
  #                            'Korea/Japan'='#b85e1a')) +
  scale_color_manual(values=c('China domestic'='#d6982f',
                              'Japan domestic'='#a14d0e',
                              'China/Mongolia'='#5ea6f2',
                              'China/Korea'='#1a66b8',
                              'Korea/Japan'='grey20')) +
  scale_shape_manual(values=c(1,4)) +
  labs(x='',y='interconnection capacity (GW)',tag='a)') +
  plot_theme +
  theme(legend.position='bottom')
plot(g_Fig_2_1_0)

g_Fig_2_1_1 <- df_lnkcap_agg %>%
  filter(Flag=='domestic',Y5>=2015) %>% 
  # pivot_wider(names_from=Y5,values_from=value,values_fill=0) %>%
  # pivot_longer(cols=-c(Sc,Sv),names_to='Y5',values_to='value',names_transform=as.numeric) %>% 
  ggplot() +
  geom_line(aes(x=Y5,y=value/10^3,color=Sv,linetype=Sc),stat='identity',linewidth=0.3) +
  geom_point(aes(x=Y5,y=value/10^3,color=Sv,shape=Sc)) +
  geom_hline(yintercept=0,linewidth=0.2,color='grey60') +
  # scale_color_manual(values=c('China domestic'='#5ea6f2',
  #                            'Japan domestic'='#1a66b8',
  #                            'China/Mongolia'='#fcca3f',
  #                            'China/Korea'='#e6a73c',
  #                            'Korea/Japan'='#b85e1a')) +
  scale_color_manual(values=c('China domestic'='#d6982f',
                              'Japan domestic'='#a14d0e',
                              'China/Mongolia'='#5ea6f2',
                              'China/Korea'='#1a66b8',
                              'Korea/Japan'='grey20')) +
  scale_shape_manual(values=c(1,4)) +
  labs(x='',y='interconnection capacity\n(GW)',tag='a)') +
  guides(color='none') +
  plot_theme +
  theme(legend.position=c(0.7,0.5))
plot(g_Fig_2_1_1)



g_Fig_2_1_2 <- df_lnkcap_agg %>%
  filter(Flag=='international') %>% 
  group_by(Sv,Y5,Flag) %>% 
  complete(Sc=c('w/ grid','w/o grid'),fill=list(value=0)) %>% 
  ungroup() %>% 
  # pivot_wider(names_from=Y5,values_from=value,values_fill=0) %>%
  # pivot_longer(cols=-c(Sc,Sv),names_to='Y5',values_to='value',names_transform=as.numeric) %>% 
  ggplot() +
  geom_hline(yintercept=0,linewidth=0.2,color='grey60') +
  geom_line(aes(x=Y5,y=value/10^3,color=Sv,linetype=Sc),stat='identity',linewidth=0.3) +
  geom_point(aes(x=Y5,y=value/10^3,color=Sv,shape=Sc)) +
  # scale_color_manual(values=c('China domestic'='#5ea6f2',
  #                            'Japan domestic'='#1a66b8',
  #                            'China/Mongolia'='#fcca3f',
  #                            'China/Korea'='#e6a73c',
  #                            'Korea/Japan'='#b85e1a')) +
  scale_color_manual(values=c('China domestic'='#d6982f',
                              'Japan domestic'='#a14d0e',
                              'China/Mongolia'='#5ea6f2',
                              'China/Korea'='#1a66b8',
                              'Korea/Japan'='grey20')) +
  scale_shape_manual(values=c(1,4)) +
  scale_x_continuous(n.breaks=3) +
  labs(x='',y='',tag='') +
  plot_theme +
  theme(legend.position='none')
plot(g_Fig_2_1_2)

# g_Fig_2_1 <- ((g_Fig_2_1_1 + g_Fig_2_1_2 + plot_layout(width=c(2,1))) /
#   g_legend(g_Fig_2_1_0+guides(color=guide_legend(nrow=2),linetype='none',shape='none'))) +
#   plot_layout(heights = c(8,1))


ggsave(filename=paste0(odir,'/Fig2_1.png'),g_Fig_2_1,
       width=4,height=3.25)


# -------------------------------------------------------------------------


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

# 表示
plot(g_exchange_china_300C_multi)

# 保存
ggsave(filename = "output/g_exchange_china_300C.png",
       plot = g_exchange_china_300C_multi,
       width = 120, height = 100, units = "mm", dpi = 300, bg = "white")