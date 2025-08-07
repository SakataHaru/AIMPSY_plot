# library -----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(magrittr)
library(ggplot2)
library(cowplot)
library(readxl)
library(lemon)
library(patchwork)
library(gdxrrw)

# Theme & Function --------------------------------------------------------

MyTheme <- theme_bw() +
  theme(
    panel.border=element_blank(),
    panel.grid.minor = element_line(color = NA), 
    axis.line=element_line(colour="black"),
    panel.background=element_rect(fill = "white"),
    panel.grid.major=element_blank(),
    strip.background=element_rect(fill="white", colour="white"),
    strip.text.x = element_text(size=8, colour = "black", angle = 0,face="bold"),
    strip.text.y = element_text(size=8, colour = "black", angle = 270,face="bold"),
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

source('code/dataframe.R')

year_all <- seq(2020,2050,by=5)


# oshiro plotの使えそうなやつ -----------------------------------------------------
# 
# mytheme$theme_set1 <- theme_bw()+theme(plot.title=element_text(size=8.5),
#                                        panel.grid=element_blank(),
#                                        panel.border=element_blank(),
#                                        axis.line=element_line(color='black',linewidth=.3),
#                                        axis.ticks=element_line(color='black',linewidth=.3),
#                                        axis.text=element_text(size=8),
#                                        axis.title=element_text(size=9),
#                                        legend.title=element_text(size=9),
#                                        legend.text=element_text(size=8),
#                                        strip.background=element_blank())
# 
# p$l_tmp <- plot_grid(p$l_tmp1,ggplotGrob(p$l_rangeleg+theme(plot.margin=margin(50,0,50,0))),
#                      get_legend(p$share_cof),nrow=1,rel_widths=c(1.3,1,1))
# p$tmp1 <- plot_grid(p$sec_ene_ele,p$sec_ene_ele_fos+theme(legend.position='none'),
#                     nrow=1,rel_widths=c(1,.67),labels=c('a','b'),label_size=12)
# p$tmp2 <- plot_grid(p$share_cof+theme(legend.position='none'),p$share_cof_R5+theme(legend.position='none'),
#                     nrow=1,rel_widths=c(1,2),labels=c('c','d'),label_size=12)
# p$tmp3 <- plot_grid(p$cof_carpri+theme(legend.position='none'),p$l_tmp,
#                     nrow=1,rel_widths=c(1,1.4),labels=c('e',''),label_size=12)
# p$tmp <- plot_grid(p$tmp1,p$tmp2,p$tmp3,ncol=1,rel_heights=c(1,1.25,1))
# ggsave(filename='output/fig1.png',plot=p$tmp,width=180,height=200,units='mm',dpi=300,bg='white')
# ggsave(filename='output/fig1.eps',plot=p$tmp,width=180,height=200,units='mm')

# Month,Quarter,Season　ファイルの結合 -------------------------------------------------------------------------

Region<-'World'
timeslice<-c('2C_12ヶ月','2C_四半期','2C_半年',
             '1.5C_12ヶ月','1.5C_四半期','1.5C_半年',
             '1.5Cdec_12ヶ月','1.5Cdec_四半期','1.5Cdec_半年')

Month<-c('2C_12ヶ月','1.5C_12ヶ月','1.5Cdec_12ヶ月')
Season<-c('2C_半年','1.5C_半年','1.5Cdec_半年')

diff<-c('2C_12ヶ月','2C_四半期',
        '1.5C_12ヶ月','1.5C_四半期',
        '1.5Cdec_12ヶ月','1.5Cdec_四半期')

m_path<-"../../AIMTech_result/timeslice_tech/month/merged_output.gdx"
q_path<-"../../AIMTech_result/timeslice_tech/quarter/merged_output.gdx"
s_path<-"../../AIMTech_result/timeslice_tech/season/merged_output.gdx"

df_month <- rgdx.param(m_path,"iamc_gdx") %>% 
  filter(Sr %in% Region)%>% 
  filter(Sc !="historical") %>% 
  select(-Sr)

df_quarter <- rgdx.param(q_path,"iamc_gdx") %>% 
  filter(Sr %in% Region)%>% 
  filter(Sc !="historical") %>% 
  select(-Sr)

df_season <- rgdx.param(s_path,"iamc_gdx") %>% 
  filter(Sr %in% Region)%>% 
  filter(Sc !="historical") %>% 
  select(-Sr)


df <- full_join(df_month,df_quarter) %>%
  full_join(df_season) %>% 
  mutate(Sc=recode(Sc,
                   NPi1000m="2C_12ヶ月",
                   NPi1000q="2C_四半期",
                   NPi1000s="2C_半年",
                   NPi500m="1.5C_12ヶ月",
                   NPi500q="1.5C_四半期",
                   NPi500s="1.5C_半年",
                   NPi500m_DEC="1.5Cdec_12ヶ月",
                   NPi500q_DEC="1.5Cdec_四半期",
                   NPi500s_DEC="1.5Cdec_半年"),
         Y5 = as.numeric(as.character(Y5))) %>% 
  filter(Sc %in% timeslice) %>% 
  rename(Scenario=Sc,Variable=Sv,Value=iamc_gdx)

# 最終エネルギー需要　差分でグラフを作ってみた-------------------------------------------------------------------------

df_finene <- df %>% 
  filter(Variable %in% finene$Variable, Y5%in%year_all) %>% 
  mutate(Sb=case_when(
    str_detect(Variable,'Bio') ~ 'Biomass',
    TRUE ~ Variable
  )) %>% 
  group_by(Scenario,Sb,Y5) %>% 
  summarise(Value=sum(Value)) %>% 
  ungroup() %>% 
  rename(Variable=Sb) %>% 
  mutate(Variable=factor(Variable,levels=rev(c('Fin_Ene_Liq_Oil',
                                               'Fin_Ene_SolidsCoa',
                                               'Fin_Ene_Gas',
                                               'Biomass',
                                               'Fin_Ene_Solar',
                                               'Fin_Ene_Ele',
                                               'Fin_Ene_Heat',
                                               'Fin_Ene_Hyd',
                                               'Fin_Ene_Liq_Hyd_syn'))))

plt <- set_plot(finene %>% 
                  filter(!str_detect(Variable,'Bio')) %>%
                  bind_rows(data.frame(Variable='Biomass',Legend='Biomass',Color='darkolivegreen2')) %>%
                  mutate(Variable=as.character(Variable)) %>% 
                  mutate(Legend=factor(Legend,levels=c('Oil',
                                                       'Coal',
                                                       'Gas',
                                                       'Biomass',
                                                       'Solar',
                                                       'Electricity',
                                                       'Heat',
                                                       'Hydrogen',
                                                       'Synfuel'))) %>% 
                  arrange(Legend))

g_finarea<-df_finene %>% 
  filter(Scenario%in%Season) %>% 
  mutate(Scenario=factor(Scenario,levels=Season)) %>% 
  ggplot() +
  geom_area(aes(x=Y5,y=Value,fill=Variable,group=Variable)) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='') +
  facet_wrap(vars(Scenario),nrow=1) +
  guides(fill=guide_legend(nrow=1,reverse=FALSE)) +
  labs(y=expression(paste('Final energy (EJ ',{yr^-1},')'))) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 10)) +
  MyTheme +
  theme(legend.key.size=unit(5.5,'mm'),
        legend.text = element_text(size = 8),nrow=2)

#ggsave(paste0('output/energy/finene.png'),width=10,height=4)

diff<-c('2C_12ヶ月','2C_四半期',
        '1.5C_12ヶ月','1.5C_四半期',
        '1.5Cdec_12ヶ月','1.5Cdec_四半期')

#2050年の結果はSeasonからの差分で表示
g_finene2050<-df_finene %>% 
  filter(Scenario%in%timeslice) %>% 
  mutate(Scenario=factor(Scenario,levels=c(timeslice))) %>% 
  filter(Y5==2050) %>% 
  group_by(Variable) %>% 
  mutate(Value = case_when(
    Scenario %in% c('2C_12ヶ月', '2C_四半期', '2C_半年') ~ 
      Value - Value[Scenario == '2C_半年'],
    Scenario %in% c('1.5C_12ヶ月', '1.5C_四半期', '1.5C_半年') ~ 
      Value - Value[Scenario == '1.5C_半年'],
    Scenario %in% c('1.5Cdec_12ヶ月', '1.5Cdec_四半期', '1.5Cdec_半年') ~ 
      Value - Value[Scenario == '1.5Cdec_半年'],
    TRUE ~ Value
  )) %>% 
  ungroup() %>% 
  filter(Scenario%in%diff) %>% 
  ggplot() +
  geom_bar(aes(x=Scenario,y=Value,fill=Variable),stat='identity') +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='') +
  facet_wrap(vars(Y5),nrow=1) +
  guides(fill=guide_legend(reverse=FALSE)) +
  labs(y=expression(paste('Difference in final energy (EJ ',{yr^-1},')'))) +
  MyTheme 

g_finene_all<-(g_finarea + g_finene2050) +
  plot_layout(ncol = 2, widths = c(7, 3.5),guides="collect") & # グラフを横に並べ、幅を7:3に調整
  theme(
    plot.tag = element_text(size = 20, face = "bold"),
    legend.position = "bottom",  # 凡例をグラフ下部に配置
    legend.text = element_text(size = 8),
    legend.title = element_blank(), 
    legend.key.size=unit(5.5,'mm')) 

# グラフの保存
# ggsave(paste0('output/GEER_2/finene_all.png'),
#        plot = g_finene_all, width = 8 ,height = 6)

# セクター別　最終エネルギー需要　これも差分のグラフ -----------------------------------------------------

df_sector <- list(name1=c('Ind','Res_and_Com','Tra'),name2=list(finind,finbui,fintra))

df_finsec <- map2(df_sector$name1, df_sector$name2,~{
  df %>% filter(Variable%in%..2$Variable) %>% 
    mutate(Sb=case_when(
      str_detect(Variable,'Oil') ~ 'Fin_Ene_Liq_Oil',
      str_detect(Variable,'Coa') ~ 'Fin_Ene_SolidsCoa',
      str_detect(Variable,'Gas') ~ 'Fin_Ene_Gas',
      str_detect(Variable,'Bio') ~ 'Biomass',
      str_detect(Variable,'Solar') ~ 'Fin_Ene_Solar',
      str_detect(Variable,'Ele') ~ 'Fin_Ene_Ele',
      str_detect(Variable,'Heat') ~ 'Fin_Ene_Heat',
      str_detect(Variable,'Hyd')&!str_detect(Variable,'syn') ~ 'Fin_Ene_Hyd',
      str_detect(Variable,'syn') ~ 'Fin_Ene_Liq_Hyd_syn'
    )) %>% 
    group_by(Scenario,Sb,Y5) %>% 
    summarise(Value=sum(Value)) %>% 
    rename(Variable=Sb) %>%
    mutate(Variable=factor(Variable,levels=rev(c('Fin_Ene_Liq_Oil',
                                                 'Fin_Ene_SolidsCoa',
                                                 'Fin_Ene_Gas',
                                                 'Biomass',
                                                 'Fin_Ene_Solar',
                                                 'Fin_Ene_Ele',
                                                 'Fin_Ene_Heat',
                                                 'Fin_Ene_Hyd',
                                                 'Fin_Ene_Liq_Hyd_syn')))) %>% 
    mutate(Se=..1)
}) %>%
  bind_rows() %>% 
  mutate(Se=case_when(
    Se=='Ind'~'Industry',
    Se=='Tra'~'Transport',
    TRUE~'Buildings'
  )) %>%
  mutate(Se=factor(Se,levels=c('Industry','Buildings','Transport')))

plt <- set_plot(finene %>% 
                  filter(!str_detect(Variable,'Bio')) %>%
                  bind_rows(data.frame(Variable='Biomass',Legend='Biomass',Color='darkolivegreen2')) %>%
                  mutate(Variable=as.character(Variable)) %>% 
                  mutate(Legend=factor(Legend,levels=c('Oil',
                                                       'Coal',
                                                       'Gas',
                                                       'Biomass',
                                                       'Solar',
                                                       'Electricity',
                                                       'Heat',
                                                       'Hydrogen',
                                                       'Synfuel'))) %>% 
                  arrange(Legend))

df_finsec_diff <- df_finsec %>%
  filter(Scenario %in% timeslice) %>%
  mutate(Scenario = factor(Scenario, levels = c(timeslice))) %>%
  filter(Y5 == 2050) %>%
  group_by(Se, Variable) %>%
  mutate(Value = case_when(
    Scenario %in% c('2C_12ヶ月', '2C_四半期', '2C_半年') ~ 
      Value - ifelse(any(Scenario == '2C_半年'), Value[Scenario == '2C_半年'], 0),
    Scenario %in% c('1.5C_12ヶ月', '1.5C_四半期', '1.5C_半年') ~ 
      Value - ifelse(any(Scenario == '1.5C_半年'), Value[Scenario == '1.5C_半年'], 0),
    Scenario %in% c('1.5Cdec_12ヶ月', '1.5Cdec_四半期', '1.5Cdec_半年') ~ 
      Value - ifelse(any(Scenario == '1.5Cdec_半年'), Value[Scenario == '1.5Cdec_半年'], 0),
  )) %>%
  ungroup() %>%
  filter(Scenario %in% diff)

g_finbar<-df_finsec_diff %>% 
  ggplot() +
  geom_bar(aes(x=Scenario,y=Value,fill=Variable),stat='identity') +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='') +
  guides(fill=guide_legend(reverse=FALSE,nrow=1)) +
  labs(y=expression(paste('Difference in final energy (EJ ',{yr^-1},')'))) +
  facet_wrap(vars(Se),nrow=1)+
  MyTheme +
  theme(legend.title = element_text(),
        legend.position = 'none')

ggsave(paste0('output/GEER_2/finene_sector.png'),width=8,height=7)


l_finene <- plot_grid(get_legend(
  g_finarea+theme(legend.text = element_text(size = 9))+guides(fill = guide_legend(nrow = 2))),
  nrow=1)
finene_all <- plot_grid(g_finarea+theme(legend.position='none'),g_finene2050+theme(legend.position='none'),
                        nrow=1,rel_widths=c(8, 4),labels=c('a','b'),label_size=12)
finene_sec <- plot_grid(g_finbar+theme(legend.position='none'),
                    nrow=1,labels='c',label_size=12)
finene_plot <- plot_grid(finene_all,finene_sec,l_finene,ncol=1,rel_heights=c(5,4,1))
ggsave(filename='output/GEER_2/finene_plot.png',plot=finene_plot,width=120,height=200,units='mm',dpi=300,bg='white')


# 発電電力量　差分のグラフ(2050年) -----------------------------------------------------

diff<-c('2C_12ヶ月','2C_四半期',
        '1.5C_12ヶ月','1.5C_四半期',
        '1.5Cdec_12ヶ月','1.5Cdec_四半期')

df_secele <- df %>% 
  filter(Scenario!='Baseline') %>% 
  filter(Variable %in% secele$Variable, Y5%in%year_all) %>% 
  mutate(Variable=factor(Variable,levels=rev(secele$Variable)))

#2050年の結果はSeasonからの差分で表示
df_secele_diff<-df_secele %>% 
  filter(Scenario%in%timeslice) %>% 
  mutate(Scenario=factor(Scenario,levels=timeslice)) %>% 
  filter(Y5==2050) %>% 
  group_by(Variable, Y5) %>% 
  mutate(Value = case_when(
    Scenario %in% c('2C_12ヶ月', '2C_四半期', '2C_半年') ~ 
      Value - ifelse(any(Scenario == '2C_半年'), Value[Scenario == '2C_半年'], 0),
    Scenario %in% c('1.5C_12ヶ月', '1.5C_四半期', '1.5C_半年') ~ 
      Value - ifelse(any(Scenario == '1.5C_半年'), Value[Scenario == '1.5C_半年'], 0),
    Scenario %in% c('1.5Cdec_12ヶ月', '1.5Cdec_四半期', '1.5Cdec_半年') ~ 
      Value - ifelse(any(Scenario == '1.5Cdec_半年'), Value[Scenario == '1.5Cdec_半年'], 0),
    TRUE ~ Value
  ))%>% 
  ungroup() %>% 
  filter(Scenario%in%diff) 

plt <- set_plot(secele)

g_secele_diff <- df_secele_diff %>%  
  ggplot() +
  geom_bar(aes(x=Scenario,y=Value,fill=Variable),stat='identity') +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='') +
  facet_wrap(vars(Y5),nrow=1) +
  guides(fill=guide_legend(reverse=FALSE)) +
  labs(y=expression(paste('Difference in power generation (EJ ',{yr^-1},')'))) +
  MyTheme +
  theme(
    axis.text.y = element_text(size=12),
    strip.text.x = element_text(size = 12),
    legend.title = element_text(),
  )

ggsave(paste0('output/GEER_2/power_diff.png'),
       plot = g_secele_diff, width = 8 ,height = 6)

# 炭素回収-------------------------------------------------------------------------

df_carseq <- df %>% 
  filter(Scenario!='Baseline') %>% 
  filter(Variable %in% carseq$Variable, Y5%in%year_all) %>% 
  mutate(Variable=factor(Variable,levels=rev(carseq$Variable)))

plt <- set_plot(carseq)

g_carseq <- df_carseq %>%
  filter(Scenario%in%c('2C_半年','1.5C_半年','1.5Cdec_半年')) %>% 
  #mutate(Scenario=factor(Scenario,levels=timeslice)) %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=Value/1000,fill=Variable),stat='identity') +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='') +
  facet_wrap(vars(Scenario),nrow=1,scales='fixed') +
  labs(y=expression(paste('Carbon sequestration (Gt', {CO[2]}, ' ', {yr^-1}, ')'))) +
  MyTheme+
  theme(legend.key.size=unit(5.5,'mm'),
        legend.text = element_text(size = 8))

ggsave(paste0('output/GEER_2/carseq.png'),width=120,height=100,units='mm',dpi=300,bg='white')

df_carseq_diff <- df_carseq %>%
  filter(Scenario %in% timeslice) %>%
  mutate(Scenario = factor(Scenario, levels = c(timeslice))) %>%
  filter(Y5 == 2050) %>%
  group_by(Variable) %>%
  mutate(Value = case_when(
    Scenario %in% c('2C_12ヶ月', '2C_四半期', '2C_半年') ~ 
      Value - ifelse(any(Scenario == '2C_半年'), Value[Scenario == '2C_半年'], 0),
    Scenario %in% c('1.5C_12ヶ月', '1.5C_四半期', '1.5C_半年') ~ 
      Value - ifelse(any(Scenario == '1.5C_半年'), Value[Scenario == '1.5C_半年'], 0),
    Scenario %in% c('1.5Cdec_12ヶ月', '1.5Cdec_四半期', '1.5Cdec_半年') ~ 
      Value - ifelse(any(Scenario == '1.5Cdec_半年'), Value[Scenario == '1.5Cdec_半年'], 0),
  )) %>%
  ungroup() %>%
  filter(Scenario %in% diff)

g_carseq_diff<-df_carseq_diff %>% 
  ggplot() +
  geom_bar(aes(x=Scenario,y=Value/1000,fill=Variable),stat='identity') +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='') +
  guides(fill=guide_legend(reverse=FALSE,nrow=1)) +
  labs(y=expression(paste('Carbon sequestration (Gt', {CO[2]}, ' ', {yr^-1}, ')'))) +
  MyTheme +
  theme(legend.title = element_text(),
        legend.position = 'bottom')

ggsave(paste0('output/GEER_2/carseq_diff.png'),width=120,height=200,units='mm',dpi=300,bg='white')


# 電気分解　容量-----------------------------------------------------

line_color <- c('indianred1','indianred2','indianred3',
                'deepskyblue1','deepskyblue2','deepskyblue3', 
                'darkgoldenrod1', 'darkgoldenrod2', 'darkgoldenrod3')
shape <- c(16,4,17,16,4,17,16,4,17)


#電気分解による水素生成量
g_electrolysis <- df %>%
  filter(Variable=='Cap_Hyd_Ele',Scenario!='Baseline',Y5%in%c(2025:2050)) %>% 
  mutate(Scenario=factor(Scenario,levels=timeslice)) %>%
  ggplot() +
  geom_path(aes(x=Y5,y=Value/1000,color=Scenario,group=Scenario),linewidth=0.4) +
  geom_point(size=2,
             aes(x=Y5,y=Value/1000,color=Scenario,shape=Scenario),
             stroke=0.7) +
  scale_color_manual(values=line_color) +
  scale_shape_manual(values=shape) +
  labs(y=expression(paste('Capacity of electrolysis (TW)'))) +
  MyTheme

ggsave(paste0('output/GEER_2/electrolysis_cap.png'),width=7,height=7)


# 太陽光発電　容量-----------------------------------------------------

line_color <- c('indianred1','indianred2','indianred3',
                'deepskyblue1','deepskyblue2','deepskyblue3', 
                'darkgoldenrod1', 'darkgoldenrod2', 'darkgoldenrod3')
shape <- c(16,4,17,16,4,17,16,4,17)


#発電容量　総量
g_electrolysis <- df %>%
  filter(Variable=='Cap_Ele_Solar',Scenario!='Baseline',Y5%in%c(2025:2050)) %>% 
  mutate(Scenario=factor(Scenario,levels=timeslice)) %>%
  ggplot() +
  geom_path(aes(x=Y5,y=Value/1000,color=Scenario,group=Scenario),linewidth=0.4) +
  geom_point(size=2,
             aes(x=Y5,y=Value/1000,color=Scenario,shape=Scenario),
             stroke=0.7) +
  scale_color_manual(values=line_color) +
  scale_shape_manual(values=shape) +
  labs(y=expression(paste('Capacity of solarPV (TW)'))) +
  MyTheme

ggsave(paste0('output/GEER_2/solar_cap.png'),width=7,height=7)

#発電容量　追加総量
g_electrolysis <- df %>%
  filter(Variable=='Cap_Add_Ele_Solar',Scenario!='Baseline',Y5%in%c(2025:2050)) %>% 
  mutate(Scenario=factor(Scenario,levels=timeslice)) %>%
  ggplot() +
  geom_path(aes(x=Y5,y=Value/1000,color=Scenario,group=Scenario),linewidth=0.4) +
  geom_point(size=2,
             aes(x=Y5,y=Value/1000,color=Scenario,shape=Scenario),
             stroke=0.7) +
  scale_color_manual(values=line_color) +
  scale_shape_manual(values=shape) +
  labs(y=expression(paste('Additional capacity of solarPV (TW)'))) +
  MyTheme

ggsave(paste0('output/GEER_2/solar_cap_add.png'),width=7,height=7)

# Fig4.15-17 代表日ごとの24h電力需給-------------------------------------------------------------------------

Region<-"AUS"
Year<-2035

PW_list <- tribble(~PW,~Legend,~Color,
                   'Curtailment','Curtailment','gray',
                   'Electrolysis','Electrolysis','lightblue1',
                   'STR_CAS','CAES','lightseagreen',
                   'STR_SPHS','PHES (seasonal)','purple3',
                   'STR_DPHS','PHES (daily)','lightslateblue',
                   'STR_BAT','Battery','thistle2',
                   'SOL_PV','Solar','lightsalmon',
                   'WND','Wind','lightskyblue3',
                   'GEO','Geothermal','peru',
                   'BIM','Biomass w/o CCS','darkolivegreen2',
                   'BCS','Biomass w/ CCS','darkolivegreen4',
                   'OIL','Oil w/o CCS','sandybrown',
                   'GAS','Gas w/o CCS','lightgoldenrod',
                   'GAS_CCS','Gas w/ CCS','lightgoldenrod3',
                   'COL','Coal w/o CCS','grey30',
                   'COL_CCS','Coal w/ CCS','grey50',
                   'HYD','Hydro','lightsteelblue',
                   'NUC','Nuclear','moccasin')


# [Month]　24h power generation and demand-------------------------------------------------------------------------
plot_Sc <- '1.5C_Month_dec'


data_path<-"../../AIMTech_result/timeslice_tech/month/power_merged.gdx"
dir.create(path='output',showWarnings=F)

SS_list <- c('1月','2月','3月','4月','5月','6月',
             '7月','8月','9月','10月','11月','12月',
             'Q1 Low VRE','Q2 Low VRE',
             'Q3 Low VRE','Q4 Low VRE')

Sc_recode <- c(NPi500m="1.5C_Month",
               NPi500m_DEC="1.5C_Month_dec")

SS_recode <- c(  "Q111" = "1月", "Q121" = "2月", "Q131" = "3月", 
                 "Q211" = "4月", "Q221" = "5月", "Q231" = "6月",
                 "Q311" = "7月", "Q321" = "8月", "Q331" = "9月", 
                 "Q411" = "10月", "Q421" = "11月", "Q431" = "12月",
                 "Q1P"="Q1 Low VRE","Q2P"="Q2 Low VRE",
                 "Q3P"="Q3 Low VRE","Q4P"="Q4 Low VRE")

source('code_GEER/24h_ele_plot.R')

g_pw_24h_m <- ggplot(df_pw_24h) +
  geom_area(aes(x = TS, y = Value, fill = PW, group = PW)) +
  geom_path(data = df_pw_dem, aes(x = TS, y = Value), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
   geom_hline(yintercept = 100, linetype = "dashed", color = "black") +
   geom_hline(yintercept = 125, linetype = "dashed", color = "black") +
   geom_hline(yintercept = 150, linetype = "dashed", color = "black") +
  # geom_hline(yintercept = -100, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = seq(0, 24, by = 6), limits = c(0, 24))+
  # coord_cartesian(ylim = c(-250, 400)) +  # 範囲を正確に制御
  scale_fill_manual(
    values = setNames(PW_list$Color, PW_list$PW),
    labels = setNames(PW_list$Legend, PW_list$PW), 
    name = ''
  ) +
  facet_wrap(vars(SS),nrow=2) +
  labs(title=paste0('1.5Cdec_12ヶ月, Australia, ',Year),
       y=expression(paste('Power Genertion (GW)'))) +
  MyTheme 

# ggsave(paste0('output/GEER_2/24h_ele_dem&gen_',plot_Sc,'2030.png'),
#        width=16,height=7)


# [Quarter] 24h power generation and demand-------------------------------------------------------------------------
plot_Sc <- '1.5C_Quarter_dec'


data_path<-"../../AIMTech_result/timeslice_tech/quarter/power_merged.gdx"
dir.create(path='output',showWarnings=F)

SS_list <- c('Q11','Q12','Q1 Low VRE',
             'Q21','Q22','Q2 Low VRE',
             'Q31','Q32','Q3 Low VRE',
             'Q41','Q42','Q4 Low VRE')

Sc_recode <- c(NPi500q="1.5C_Quarter",
               NPi500q_DEC="1.5C_Quarter_dec")

SS_recode <- c("Q1P"="Q1 Low VRE","Q2P"="Q2 Low VRE",
               "Q3P"="Q3 Low VRE","Q4P"="Q4 Low VRE")

source('code_GEER/24h_ele_plot.R')

g_pw_24h_q <- ggplot(df_pw_24h) +
  geom_area(aes(x = TS, y = Value, fill = PW, group = PW)) +
  geom_path(data = df_pw_dem, aes(x = TS, y = Value), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 125, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 150, linetype = "dashed", color = "black") +
  # geom_hline(yintercept = -100, linetype = "dashed", color = "black") +
  # geom_hline(yintercept = -200, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = seq(0, 24, by = 6), limits = c(0, 24))+
  #coord_cartesian(ylim = c(-250, 400)) +  # 範囲を正確に制御
  scale_fill_manual(
    values = setNames(PW_list$Color, PW_list$PW),
    labels = setNames(PW_list$Legend, PW_list$PW), 
    name = ''
  ) +
  facet_wrap(vars(SS),nrow=2) +
  labs(title=paste0('1.5Cdec_四半期, Australia, ',Year),
       y=expression(paste('Power Genertion (GW)'))) +
  MyTheme

# ggsave(paste0('output/GEER_2/24h_ele_dem&gen_',plot_Sc,'2030.png'),
#        width=12,height=7)


# [season] 24h power generation and demand -------------------------------------------------------------------------
plot_Sc <- '1.5C_Season_dec'


data_path<-"../../AIMTech_result/timeslice_tech/season/power_merged.gdx"
dir.create(path='output',showWarnings=F)

SS_list <- c('S1','S2','S Low VRE',
             'W1','W2','W Low VRE')

Sc_recode <- c(NPi500s="1.5C_Season",
               NPi500s_DEC="1.5C_Season_dec")

SS_recode <- c("SP"="S Low VRE","WP"="W Low VRE")

source('code_GEER/24h_ele_plot.R')

g_pw_24h_s <- ggplot(df_pw_24h) +
  geom_area(aes(x = TS, y = Value, fill = PW, group = PW)) +
  geom_path(data = df_pw_dem, aes(x = TS, y = Value), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 125, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 150, linetype = "dashed", color = "black") +
  # geom_hline(yintercept = -100, linetype = "dashed", color = "black") +
  # geom_hline(yintercept = -200, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = seq(0, 24, by = 6), limits = c(0, 24))+
  #coord_cartesian(ylim = c(-250, 400)) +  # 範囲を正確に制御
  scale_fill_manual(
    values = setNames(PW_list$Color, PW_list$PW),
    labels = setNames(PW_list$Legend, PW_list$PW), 
    name = ''
  ) +
  facet_wrap(vars(SS),nrow=2) +
  labs(title=paste0('1.5Cdec_半年, Australia, ',Year),
       y=expression(paste('Power Genertion (GW)'))) +
  MyTheme 

# ggsave(paste0('output/GEER_2/24h_ele_dem&gen_',plot_Sc,'2030.png'),
#        width=4,height=7)


# -------------------------------------------------------------------------

s_q <- plot_grid(g_pw_24h_s+theme(legend.position='none')+guides(fill = guide_legend(ncol = 1)),
                 g_pw_24h_q+theme(legend.position='none')+guides(fill = guide_legend(ncol = 1)),
                 nrow=1,rel_widths=c(1.1, 2),labels=c('a','b'),label_size=12)
m <- plot_grid(g_pw_24h_m+theme(legend.position='right'),
               nrow=1,labels='c',label_size=12)

mqs_plot <- plot_grid(s_q,m,ncol=1,rel_heights=c(5,5))
ggsave(paste0('output/GEER_2/24h_mqs_plot',Year,'.png'),
       plot=mqs_plot,width=240,height=250,units='mm',dpi=300,bg='white')



