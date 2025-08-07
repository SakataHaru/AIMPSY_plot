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
    strip.text.x = element_text(size=12, colour = "black", angle = 0,face="bold"),
    strip.text.y = element_text(size=12, colour = "black", angle = 270,face="bold"),
    axis.text.x=element_text(size = 12,angle=45, vjust=1, hjust=1, margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")),
    axis.text.y=element_text(size = 12,margin = unit(c(t = 0, r = 0.3, b = 0, l = 0), "cm")),
    axis.title.x = element_blank(),
    legend.text = element_text(size=),
    legend.title = element_blank(),
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

year_all <- seq(2020,2050,5)



# Month,Quarter,Season　ファイルの結合 -------------------------------------------------------------------------


Region<-'World'
timeslice<-c('2C_12ヶ月','2C_四半期','2C_半年',
             '1.5C_12ヶ月','1.5C_四半期','1.5C_半年',
             '1.5Cdec_12ヶ月','1.5Cdec_四半期','1.5Cdec_半年')
Month<-c('2C_12ヶ月','1.5C_12ヶ月','1.5Cdec_12ヶ月')
Season<-c('2C_半年','1.5C_半年','1.5Cdec_半年')

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


# Fig4.13 世界全体の発電構成の変化------------------------------------------------------------------------

df_secele <- df %>% 
  filter(Scenario!='Baseline') %>% 
  filter(Variable %in% secele$Variable, Y5%in%year_all) %>% 
  mutate(Variable=factor(Variable,levels=rev(secele$Variable)))

plt <- set_plot(secele)

g_secele <- df_secele %>%
  filter(Scenario%in%c('2C_半年','1.5C_半年','1.5Cdec_半年')) %>% 
  #mutate(Scenario=factor(Scenario,levels=timeslice)) %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=Value,fill=Variable),stat='identity') +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='') +
  facet_wrap(vars(Scenario),nrow=1,scales='fixed') +
  labs(y=expression(paste('Power generation (EJ ',{yr^-1},')'))) +
  MyTheme+
  theme(legend.key.size=unit(5.5,'mm'),
        legend.text = element_text(size = 8))

#ggsave(paste0('output/power/ele_generation.png'),width=12,height=5)


g_secele2050<-df_secele %>% 
  filter(Y5==2050) %>%
  mutate(Scenario=factor(Scenario,levels=timeslice)) %>% 
  ggplot() +
  geom_bar(aes(x=Scenario,y=Value,fill=Variable),stat='identity') +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='') +
  facet_wrap(vars(Y5),nrow=1) +
  guides(fill=guide_legend(reverse=FALSE)) +
  labs(y='')+
  MyTheme +
  theme(
    axis.text.y = element_blank(),
    legend.title = element_text(),
    axis.line.y = element_blank(),  # y軸の線を消す
    axis.ticks.y = element_blank()  # y軸の目盛りを消す
  )

g_secele_all <- (g_secele + g_secele2050) +
  plot_layout(ncol = 2, widths = c(7, 3), guides = "collect") & 
  theme(
    legend.position = "bottom", 
    legend.text = element_text(size = 12), 
    legend.title = element_blank()
  ) &
  guides(fill = guide_legend(nrow = 3))

# グラフの保存
ggsave(paste0('output/GEER/ele_generation_all.png'),
       plot = g_secele_all, width = 8, height = 6)

# Fig4.14 余剰電力への対応策ごとの傾向の違い-----------------------------------------------------

line_color <- c('indianred1','indianred2','indianred3',
                'deepskyblue1','deepskyblue2','deepskyblue3', 
                'darkgoldenrod1', 'darkgoldenrod2', 'darkgoldenrod3')
shape <- c(16,4,17,16,4,17,16,4,17)


#電気分解による水素生成量
g_electrolysis <- df %>%
  filter(Variable=='Sec_Ene_Hyd_Ele',Scenario!='Baseline',Y5%in%c(2025:2050)) %>% 
  mutate(Scenario=factor(Scenario,levels=timeslice)) %>%
  ggplot() +
  geom_path(aes(x=Y5,y=Value,color=Scenario,group=Scenario),linewidth=0.4) +
  geom_point(size=2,
             aes(x=Y5,y=Value,color=Scenario,shape=Scenario),
             stroke=0.7) +
  scale_color_manual(values=line_color) +
  scale_shape_manual(values=shape) +
  labs(y=expression(paste('H2 production by electrolysis (EJ ',{yr^-1},')'))) +
  MyTheme

#ggsave(paste0('output/power/Fig_electrolysis_gengeration.png'),width=7,height=4)

#蓄電池への充電量
df_bat_1 <- df %>%
  filter(Variable == 'Sec_Ene_Ele_Sto_Dis_Bat', Scenario != 'Baseline', Y5 %in% c(2025:2050)) %>% 
  mutate(Scenario = factor(Scenario, levels = timeslice)) %>%
  pivot_wider(names_from = Variable, values_from = Value, values_fill = list(Value = 0)) %>%
  group_by(Scenario) %>%
  mutate(has_2025 = any(Y5 == 2025)) %>%
  ungroup()
# 2025年のデータがない Scenario を抽出し、追加用のデータフレームを作成
missing_2025 <- df_bat_1 %>%
  distinct(Scenario) %>%
  filter(!Scenario %in% df_bat_1$Scenario[df_bat_1$Y5 == 2025]) %>%
  mutate(Y5 = 2025, Sec_Ene_Ele_Sto_Dis_Bat = 0)  
# 不足している 2025 年のデータを追加
df_bat_1 <- bind_rows(df_bat_1, missing_2025) %>%
  select(-has_2025) %>%
  pivot_longer(cols = -c(Scenario, Y5), names_to = "Variable", values_to = "Value") %>% 
  arrange(Y5, Scenario)   # 年とシナリオで並び替え

g_dis_bat <-df_bat_1 %>% 
  ggplot() +
  geom_path(aes(x=Y5,y=Value,color=Scenario,group=Scenario),linewidth=0.4) +
  geom_point(size=2,
             aes(x=Y5,y=Value,color=Scenario,shape=Scenario),
             stroke=0.7) +
  scale_color_manual(values=line_color) +
  scale_shape_manual(values=shape) +
  labs(y=expression(paste('Charge of battery (EJ ',{yr^-1},')'))) +
  MyTheme +
  theme(legend.key.size=unit(5.5,'mm'),
        legend.text = element_text(size = 8))

ggsave(paste0('output/power/Fig_battery_discharge.png'),width=7,height=4)


#VRE出力抑制の発生量
g_ele_cur <- df %>%
  filter(Variable=='Sec_Ene_Ele_Cur',Scenario!='Baseline',Y5%in%c(2025:2050)) %>% 
  mutate(Scenario=factor(Scenario,levels=timeslice)) %>%
  ggplot() +
  geom_path(aes(x=Y5,y=Value,color=Scenario,group=Scenario),linewidth=0.4) +
  geom_point(size=2,
             aes(x=Y5,y=Value,color=Scenario,shape=Scenario),
             stroke=0.7) +
  scale_color_manual(values=line_color) +
  scale_shape_manual(values=shape) +
  labs(y=expression(paste('Electricity curtailment (EJ ',{yr^-1},')'))) +
  MyTheme 

#ggsave(paste0('output/power/Fig_ele_cur.png'),width=7,height=4)


#蓄電池、電気分解、出力抑制のグラフの結合
g_bat_H2_cur <- (g_electrolysis +g_dis_bat + g_ele_cur) +
 # plot_annotation(tag_levels = 'a', tag_suffix = ')') +
  plot_layout(guides="collect") &
  theme(#plot.tag = element_text(size = 15, face = "bold"),
        legend.position = "bottom",
        legend.text=element_text(size=12),
        legend.title = element_blank())&
  guides(color = guide_legend(nrow = 3))

ggsave(paste0('output/GEER/battery_electrolysis_curtailment.png'),width=8,height=7)

# 発電部門のごっついグラフ------------------------------------------------------------------------
l_secele <- plot_grid(get_legend(
  g_secele+theme(legend.text = element_text(size = 9))+guides(fill = guide_legend(nrow = 3))),
  nrow=1)
l_bat <- plot_grid(get_legend(
  g_dis_bat+theme(legend.text = element_text(size = 12))+guides(fill = guide_legend(ncol = 3,nrow=3))),
  nrow=1)
secele_all <- plot_grid(g_secele+theme(legend.position='none'),g_secele2050+theme(legend.position='none'),
                        nrow=1,rel_widths=c(7, 3.5),labels=c('a','b'),label_size=12)
H2_bat <- plot_grid(g_electrolysis+theme(legend.position='none'),
                        g_dis_bat+theme(legend.position='none'),
                        nrow=1,labels=c('c','d'),label_size=12)
cur_l <- plot_grid(g_ele_cur+theme(legend.position='none'),l_bat+theme(legend.position='none'),
                   nrow=1,labels=c('e',''),label_size=12)
ele_plot <- plot_grid(secele_all,l_secele,H2_bat,cur_l,ncol=1,rel_heights=c(6,2,6,6))
ggsave(filename='output/GEER_2/ele_plot.png',plot=ele_plot,width=120,height=300,units='mm',dpi=300,bg='white')

# Fig4.18 最終エネルギー需要　積み上げ面グラフ-------------------------------------------------------------------------

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
  filter(Scenario%in%Month) %>% 
  mutate(Scenario=factor(Scenario,levels=Month)) %>% 
  ggplot() +
  geom_area(aes(x=Y5,y=Value,fill=Variable,group=Variable)) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='') +
  facet_wrap(vars(Scenario),nrow=1) +
  guides(fill=guide_legend(nrow=1,reverse=FALSE)) +
  labs(y=expression(paste('Final energy (EJ ',{yr^-1},')'))) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 10)) +
  MyTheme 

#ggsave(paste0('output/energy/finene.png'),width=10,height=4)

g_finene2050<-df_finene %>% 
  filter(Scenario%in%timeslice) %>% 
  mutate(Scenario=factor(Scenario,levels=c(timeslice))) %>% 
  filter(Y5==2050) %>% 
  ggplot() +
  geom_bar(aes(x=Scenario,y=Value,fill=Variable),stat='identity') +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='') +
  facet_wrap(vars(Y5),nrow=1) +
  guides(fill=guide_legend(reverse=FALSE)) +
  labs(y='')+#expression(paste('Final energy (EJ ',{yr^-1},')'))) +
  MyTheme +
  theme(
    axis.text.y = element_blank(),
    strip.text.x = element_text(size = 12),
    legend.title = element_text(),
    axis.line.y = element_blank(),  # y軸の線を消す
    axis.ticks.y = element_blank()  # y軸の目盛りを消す
  )

g_finene_all<-(g_finarea + g_finene2050) +
  plot_layout(ncol = 2, widths = c(7, 3.5),guides="collect") & # グラフを横に並べ、幅を7:3に調整
  theme(
    plot.tag = element_text(size = 20, face = "bold"),
    legend.position = "bottom",  # 凡例をグラフ下部に配置
    legend.text = element_text(size = 8),
    legend.title = element_blank()  # 凡例タイトルを非表示
  ) 

# グラフの保存
ggsave(paste0('output/GEER/finene_all.png'),
       plot = g_finene_all, width = 8 ,height = 6)


# Fig4.19 セクター別　最終エネルギー需 -----------------------------------------------------

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

g_finbar <- df_finsec %>%
  filter(Y5==2050,Scenario!='Baseline') %>%
  mutate(Scenario = factor(Scenario, levels = timeslice)) %>%  # Scenario列を再定義
  ggplot() +
  geom_bar(aes(x=Scenario,y=Value,fill=Variable),stat='identity') +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='') +
  guides(fill=guide_legend(reverse=FALSE,nrow=1)) +
  labs(y=expression(paste('Final energy (EJ ',{yr^-1},')'))) +
  facet_wrap(vars(Se),nrow=1)+
  MyTheme +
  theme(legend.title = element_text(),
        legend.position = 'bottom',
        legend.text=element_text(size=10))

ggsave(paste0('output/GEER/finene_sector.png'),width=8,height=7)


# -------------------------------------------------------------------------
# グラフの統合（エネルギー需要）
# タグを個別に指定
g_finarea_tagged <- g_finarea + labs(tag = "a")
g_finene2050_tagged <- g_finene2050 + labs(tag = "b")
g_finbar_tagged <- g_finbar + labs(tag = "c")

# g_finarea と g_finene2050 の横並び
g_finene_combined <- (g_finarea_tagged + g_finene2050_tagged) +
  plot_layout(ncol = 2, widths = c(7, 3.5))  # 幅調整

# g_finene_combined と g_finbar を縦に並べる
g_ene_plots <- (g_finene_combined / g_finbar_tagged) +
  plot_layout(ncol = 1, heights = c(6, 6))

# グラフと凡例の統合 (legend_only にはタグをつけない)
g_ene_lege <- (g_ene_plots / legend_only) +
  plot_layout(ncol = 1,nrow=3, heights = c(6,6,1)) &  # 凡例を下に配置
  theme(
    plot.tag = element_text(size = 15, face = "bold"),  # タグのスタイル
    legend.position = "none"  # legend_only にはタグ不要
  )

# グラフの保存
ggsave("output/GEER/finene.png",
       plot = g_ene_lege, width = 8, height = 10)


# Fig4.22 運輸部門　詳細部門ごとのエネルギー需要  -----------------------------------------------------
df_finroa <- df %>%
  filter(Variable %in% finroa$Variable,
         Scenario!='Baseline',
         Y5==2050) %>%
  mutate(Variable=factor(Variable,levels=rev(finroa$Variable))) %>% 
  mutate(Scenario=factor(Scenario, levels=timeslice))
df_finroa$Detail <- "Road transport"
plt <- set_plot(finroa)
g_finroa <- df_finroa %>%
  ggplot() +
  geom_bar(aes(x=factor(Scenario),y=Value,fill=Variable),stat='identity') +
  # facet_wrap(vars(Y5),scales='free') +  # 年ごとにファセット
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='') +
  labs(title='Road',
       x='',
       y=expression(paste('Final energy (EJ ',{yr^-1},')'))) +
  MyTheme

df_finavi <- df %>%
  filter(Variable %in% finavi$Variable,
         Scenario!='Baseline',
         Y5==2050) %>%
  mutate(Variable=factor(Variable,levels=rev(finavi$Variable))) %>% 
  mutate(Scenario=factor(Scenario, levels=timeslice))
df_finavi$Detail <- "Aviation"
plt <- set_plot(finavi)
g_finavi <- df_finavi %>%
  ggplot() +
  geom_bar(aes(x=factor(Scenario),y=Value,fill=Variable),stat='identity') +
  # facet_wrap(vars(Y5),scales='free') +  # 年ごとにファセット
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='') +
  labs(title='Aviation',
       x='',
       y=expression(paste('Final energy (EJ ',{yr^-1},')'))) +
  MyTheme 

df_finshi <- df %>%
  filter(Variable %in% finshi$Variable,
         Scenario!='Baseline',
         Y5==2050) %>%
  mutate(Variable=factor(Variable,levels=rev(finshi$Variable))) %>% 
  mutate(Scenario=factor(Scenario, levels=timeslice))
df_finshi$Detail <- "Shipping"
plt <- set_plot(finshi)
g_finshi <- df_finshi %>%
  ggplot() +
  geom_bar(aes(x=factor(Scenario),y=Value,fill=Variable),stat='identity') +
  # facet_wrap(vars(Y5),scales='free') +  # 年ごとにファセット
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='') +
  labs(title='Shipping',
       x='',
       y=expression(paste('Final energy (EJ ',{yr^-1},')'))) +
  MyTheme 

df_finrai <- df %>%
  filter(Variable %in% finrai$Variable,
         Scenario!='Baseline',
         Y5==2050) %>%
  mutate(Variable=factor(Variable,levels=rev(finrai$Variable))) %>% 
  mutate(Scenario=factor(Scenario, levels=timeslice))
df_finrai$Detail <- "Rail"
plt <- set_plot(finrai)
g_finrai <- df_finrai %>%
  ggplot() +
  geom_bar(aes(x=factor(Scenario),y=Value,fill=Variable),stat='identity') +
  # facet_wrap(vars(Y5),scales='free') +  # 年ごとにファセット
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='') +
  labs(title='Rail',
       x='',
       y=expression(paste('Final energy (EJ ',{yr^-1},')'))) +
  MyTheme 

l_traene <- plot_grid(get_legend(
  g_finroa+theme(legend.text = element_text(size = 9))+guides(fill = guide_legend(nrow = 1))),
  nrow=1)
tra_avi <- plot_grid(g_finroa+theme(legend.position='none'),g_finavi+theme(legend.position='none'),
                        nrow=1,rel_widths=c(1, 1),labels=c('a','b'),label_size=12)
shi_rai <- plot_grid(g_finshi+theme(legend.position='none'),g_finrai+theme(legend.position='none'),
                     nrow=1,rel_widths=c(1, 1),labels=c('c','d'),label_size=12)
finene_plot <- plot_grid(tra_avi,shi_rai,l_traene,ncol=1,rel_heights=c(5,5,1))
ggsave(filename='output/GEER_2/traene_plot.png',plot=finene_plot,width=120,height=170,units='mm',dpi=300,bg='white')


# Fig4.24 Vehicle share pss & fre -----------------------------------------------------
#passengerにおける電気自動車の割合
df_EV_sha_pss <- df %>%
  filter(Variable %in% EVshapss$Variable,
         Scenario%in%timeslice,
         Y5==2050) %>%
  group_by(Scenario,Y5) %>% 
  mutate(Value=Value*100) %>% 
  mutate(Variable=factor(Variable,levels=EVshapss$Variable)) %>% 
  mutate(Scenario=factor(Scenario, levels=timeslice))

plt <- set_plot(EVshapss)

g_EV_sha_pss <- df_EV_sha_pss %>%
  ggplot() +
  geom_bar(aes(x=factor(Scenario),y=Value,fill=Variable),stat='identity') +
  facet_wrap(vars(Y5),scales='free') +  # 年ごとにファセット
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='') +
  labs(title='Passenger transport',
       x='',
       y=expression(paste('Share of vehicles (%)'))) +
  MyTheme +
  theme(plot.title = element_text(size = 10),
        axis.text.x=element_text(size = 9,angle=45, vjust=1, hjust=1, margin = unit(c(t = 0.2, r = 0, b = 0, l = 0), "cm")),
  )

# 保存
#ggsave('output/demand_response/EV_share_pss.png',width=12,height=6)


#freightにおける電気自動車の割合
df_EV_sha_fre <- df %>%
  filter(Variable %in% EVshafre$Variable,
         Scenario!='Baseline',
         Y5==2050) %>%
  group_by(Scenario,Y5) %>% 
  mutate(Value=Value*100) %>% 
  mutate(Variable=factor(Variable,levels=EVshafre$Variable)) %>% 
  mutate(Scenario=factor(Scenario, levels=timeslice))

plt <- set_plot(EVshafre)

g_EV_sha_fre <- df_EV_sha_fre %>%
  ggplot() +
  geom_bar(aes(x=factor(Scenario),y=Value,fill=Variable),stat='identity') +
  facet_wrap(vars(Y5),scales='free') +  # 年ごとにファセット
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='') +
  labs(title='Freight transport',
       x='',
       y=expression(paste(''))) +
  MyTheme +
  theme(plot.title = element_text(size = 10),
        axis.text.x=element_text(size = 9,angle=45, vjust=1, hjust=1, margin = unit(c(t = 0.2, r = 0, b = 0, l = 0), "cm")),
  )

# 
# # 保存
# #ggsave('output/demand_response/EV_share_fre.png',width=12,height=6)
# 
# g_legend <- g_EV_sha_pss + theme(legend.position = "bottom",
#                                 legend.text = element_text(size = 10))  # 凡例を表示させる設定
# legend_only <- g_legend %>% g_legend()  # 凡例だけを抽出
# 
# g_vehicle <- (g_EV_sha_pss + g_EV_sha_fre) +
#   plot_layout(nrow=1,guides = "collect")&
#   theme(legend.position = "none")  
# 
# g_vehicle_legend<-(g_vehicle / legend_only) +
#   plot_layout(ncol=1,nrow=2,height = c(9,1))
# 
# ggsave('output/GEER/EV_share.png',width=7,height=5)

l_ECshare <- plot_grid(get_legend(
  g_EV_sha_pss+theme(legend.text = element_text(size = 8))+guides(fill = guide_legend(nrow = 1))))
EV_FCEV <- plot_grid(g_EV_sha_pss+theme(legend.position='none'),g_EV_sha_fre+theme(legend.position='none'),
                     nrow=1,rel_widths=c(1.1, 1),labels=c('a','b'),label_size=12)
EV_FCEV_share <- plot_grid(EV_FCEV,l_ECshare,ncol=1,rel_heights=c(7,1))
ggsave(filename='output/GEER_2/EV_share.png',plot=EV_FCEV_share,width=120,height=110,units='mm',dpi=300,bg='white')


# Fig4.25 世界全体の部門別CO2排出量-------------------------------------------------------------------------

emisec <- tribble(~Variable,~Legend,~Color,
                  'Emi_CO2_Oth','DACCS','thistle2',
                  'Emi_CO2_Ene_Sup','Energy\nsupply','moccasin',
                  'Emi_CO2_Ene_Dem_Ind','Industry','salmon',
                  'Emi_CO2_Ene_Dem_Res_and_Com','Buildings','lightskyblue3',
                  'Emi_CO2_Ene_Dem_Tra','Transport','darkolivegreen2',
                  'Emi_CO2_Ene_Dem_Oth_Sec','Other\ndemand','grey',
                  # 'Emi_CO2_Ene_Dem_AFO','AFOFI','paleturquoise3',
                  'Emi_CO2_Ind_Pro','Industrial\nprocess','sandybrown')

# 2050年におけるCO2排出量
df_emi <- df %>%
  filter(Variable %in% emisec$Variable)

plt <- set_plot(emisec)

g_emi_2050 <- df_emi %>%
  filter(Y5==2050, Scenario != 'Baseline') %>%
  group_by(Scenario, Variable, Y5) %>%
  summarise(Value = sum(Value)) %>%
  mutate(Variable = factor(Variable, levels=rev(emisec$Variable))) %>%
  mutate(Scenario = factor(Scenario, levels = timeslice)) %>%
  ggplot() +
  geom_bar(aes(x = Scenario, y = Value / 1000, fill = Variable), stat = 'identity') +
  scale_fill_manual(values = plt$Color, labels = plt$Legend, name ='Variable') +  
  guides(fill = guide_legend(nrow = 1)) + 
  facet_wrap(vars(Y5)) +
  labs(y = expression(paste({CO[2]}, ' emissions (Gt', {CO[2]}, ' ', {yr^-1}, ')'))
  ) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey60', size = 0.5) +
  MyTheme +
  theme(legend.position = "bottom")&
  guides(color = guide_legend(nrow = 2))
    
    ggsave(paste0('output/GEER/Fig_CO2emission_2050.png'),width = 7, height =6)

# Fig4.27 炭素価格とエネルギーシステムコスト -------------------------------------------------------


line_color <- c('indianred1','indianred2','indianred3',
                'deepskyblue1','deepskyblue2','deepskyblue3', 
                'darkgoldenrod1', 'darkgoldenrod2', 'darkgoldenrod3')
    shape <- c(16,4,17,16,4,17,16,4,17)
    
g_prccar <- df %>%
  filter(Variable=='Prc_Car',Scenario!='Baseline',Y5%in%c(2020:2050)) %>% 
  mutate(Scenario=factor(Scenario,levels=timeslice)) %>%
  ggplot() +
  geom_path(aes(x=Y5,y=Value,color=Scenario,group=Scenario),size=0.4) +
  geom_point(size=2.5,aes(x=Y5,y=Value,color=Scenario,shape=Scenario),stroke=0.7) +
  scale_color_manual(values=line_color) +
  scale_shape_manual(values=shape) +
  labs(y=expression(paste('Carbon prices (US$2010 t-',{CO[2]^-1},')'))) +
  MyTheme +
  theme(legend.position = "bottom",
        legend.text=element_text(size=9),
        legend.title = element_blank())&
  guides(color = guide_legend(nrow = 9))

ggsave(paste0('output/GEER/carbonprice.png'),width=7,height=4)

df_totcos2050 <- df %>% 
  filter(Variable=='Pol_Cos_Add_Tot_Ene_Sys_Cos',Scenario!='Baseline') %>%
  filter(Y5 %in% c(2025:2050)) %>%
  mutate(discount=0.95^(Y5-2023)) %>% 
  mutate(Value=Value*discount*5/1000) %>% 
  group_by(Scenario) %>% 
  summarise(Value=sum(Value)) %>% 
  mutate(Scenario = factor(Scenario, levels = timeslice))  # Scenario列を再定義

g_totcos2050<-df_totcos2050 %>%   
  ggplot() +
  geom_bar(aes(x=Scenario,y=Value),stat='identity') +
  #scale_fill_manual(values=line_color) +
  labs(y='Cumulative energy system cost\n(trillion US$2010)') +
  MyTheme 

l_emi <- plot_grid(get_legend(
  g_emi_2050+theme(legend.text = element_text(size = 9))+guides(fill = guide_legend(nrow = 1))),
  nrow=1)
emi <- plot_grid(g_emi_2050+theme(legend.position='right')+guides(fill = guide_legend(ncol = 1)),
                        nrow=1,labels='a',label_size=12)
car <- plot_grid(g_prccar+theme(legend.position='right'),
                    nrow=1,labels='b',label_size=12)
cost <- plot_grid(g_totcos2050+theme(legend.position='none'),
                      nrow=1,labels='c',label_size=12)

emi_cost_plot <- plot_grid(emi,car,cost,ncol=1,rel_heights=c(5,5,5))
ggsave(filename='output/GEER_2/emi_cost_plot.png',plot=emi_cost_plot,width=120,height=270,units='mm',dpi=300,bg='white')

# Fig4.15-17 代表日ごとの24h電力需給-------------------------------------------------------------------------

Region<-"AUS"
Year<-2030

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
  scale_x_continuous(breaks = seq(0, 24, by = 6), limits = c(0, 24))+
  coord_cartesian(ylim = c(-250, 400)) +  # 範囲を正確に制御
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
  scale_x_continuous(breaks = seq(0, 24, by = 6), limits = c(0, 24))+
  coord_cartesian(ylim = c(-250, 400)) +  # 範囲を正確に制御
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
  scale_x_continuous(breaks = seq(0, 24, by = 6), limits = c(0, 24))+
  coord_cartesian(ylim = c(-250, 400)) +  # 範囲を正確に制御
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
ggsave(filename='output/GEER_2/24h_mqs_plot.png',plot=mqs_plot,width=240,height=250,units='mm',dpi=300,bg='white')



