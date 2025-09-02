
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
scenario <- 'asg2050'
reg_in <- 'Global'
date <- '2508191227'

cdir <- getwd()
pdir <- paste0(cdir,'/prog/inc_prog')
odir <- paste0(cdir,'/output/TR_China')
ddir <- paste0(aimpsy,'/',scenario,'/',reg_in,'/',date)  # output path

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

df_secele <- bind_rows(df_CNS_tot, df_ADV_tot) %>% 
  filter(Scenario %in% c('300C-','300C+')) %>%
  filter(Variable %in% secele$Variable, Year == 2050) %>%
  mutate(
    Variable = factor(Variable, levels = rev(secele$Variable)))

g_secele <- df_secele %>%
  ggplot() +
  geom_bar(aes(x=link,y=Value,fill=Variable),stat='identity') +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='') +
  #facet_wrap(vars(Region),nrow=1,scales="free_y") +
  guides(fill=guide_legend(reverse=FALSE)) +
  labs(y=expression(paste('Power generation (EJ ',{yr^-1},')')),
       title = 'ASIA') +
  MyTheme +
  theme(
    title = element_text(size = 10),
    legend.position = "right", 
    legend.text = element_text(size = 10),
    legend.title = element_blank())

ggsave(filename='output/TR_secele.png',plot=g_secele,width=100,height=100,units='mm',dpi=300,bg='white')



# line_plot ---------------------------------------------------------------

plot_nested_tabset <- list()

var_list <- df_link_ASIA %>%
  filter(!str_detect(Variable, "^Cap_Add|^Inv")) %>%
  pull(Variable) %>%
  unique() %>%
  sort()

var_class <- tibble(Variable=var_list) %>%
  left_join(set_Sv, by=c("Variable"="GAMS.code")) %>%
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

line_color <- c('indianred1','deepskyblue1','darkgoldenrod1')
shape <- c(16,4,17)

# カテゴリごとの出力フォルダとY軸ラベルの対応を定義
category_info <- list(
  "Capacity" = list(folder = "Capacity", ylabel = expression(paste('GW'))),
  "Secondary Energy|Electricity" = list(folder = "Secene_Ele", ylabel = expression(paste('EJ/yr'))),
  "Secondary Energy|Hydrogen" = list(folder = "Secene_Hyd", ylabel = expression(paste('EJ/yr')))
)

# ループでカテゴリごとに処理
for (cat in names(category_info)) {
  
  # カテゴリに属する変数一覧
  var_list_cat <- var_class %>%
    filter(Category == cat) %>%
    pull(Variable)
  
  # 出力先フォルダ名とy軸ラベルを取得
  folder <- paste0("output/", category_info[[cat]]$folder)
  ylabel <- category_info[[cat]]$ylabel
  
  # フォルダがなければ作成
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
  
  # 各変数ごとにグラフ作成＆保存
  for (var in var_list_cat) {
    
    df_plot <- df_link_ASIA %>%
      filter(Variable == var, Scenario %in% c('300C-','300C+'), Year %in% 2020:2050)
    
    p <- ggplot(df_plot) +
      geom_path(aes(x = Year, y = Value, color = link, group = link), size = 0.4) +
      geom_point(aes(x = Year, y = Value, color = link, shape = link), size = 2.5, stroke = 0.7) +
      scale_color_manual(values = line_color) +
      scale_shape_manual(values = shape) +
      labs(y = ylabel, title = var) +
      MyTheme +
      theme(
        legend.position = "right",
        legend.text = element_text(size = 9),
        legend.title = element_blank()
      ) &
      guides(color = guide_legend(nrow = 9))
    
    filename <- paste0(folder, "/", gsub("\\|", "_", var), ".png")
    ggsave(filename = filename, plot = p, width = 120, height = 100, units = 'mm', dpi = 300, bg = 'white')
  }
}


# 送電網プロット -----------------------------------------------------------------

library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)



# Asia  Interconnection capacity ------------------------------------------------

#300C_ADV
# === 1) ノード座標を用意 ===
g_nodes <- st_read("C:/Users/halra/OneDrive - Kyoto University/My work/AIM-PSY/nodes_centroid.geojson",
                   quiet = TRUE)

nodes_tbl <- g_nodes %>%
  st_drop_geometry() %>%
  transmute(N_PSY,
            lon = as.numeric(centroid_x),
            lat = as.numeric(centroid_y))

# === 2) エッジ(送電線)に座標を結合 ===
# N0/N1に "-AC" が残っている可能性があれば消す
ddir <- "C:/Users/halra/OneDrive - Kyoto University/My work/AIM-PSY/Asia_link/3HOUR/300C_ADV"
df_lnkcap_ADV <- rgdx.param(str_c(ddir,'/gdx_primary/300C+/2050.gdx'),'vfs_l') %>% 
  mutate(across(where(is.factor),~as.character(.))) %>%
  filter(str_detect(L,'.*-AC_.*-AC$'))%>% 
  rename(Value="vfs_l") %>% 
  separate(L,sep='_',into=c('N0','N1'),remove=FALSE) %>% 
  mutate(N0 = str_remove(N0, "-AC"), N1 = str_remove(N1, "-AC"))

edges <- df_lnkcap_ADV %>%
  mutate(N0 = str_remove_all(N0, "-AC"),
         N1 = str_remove_all(N1, "-AC")) %>%
  left_join(nodes_tbl, by = c("N0" = "N_PSY")) %>%
  rename(x0 = lon, y0 = lat) %>%
  left_join(nodes_tbl, by = c("N1" = "N_PSY")) %>%
  rename(x1 = lon, y1 = lat) %>%
  # 座標が欠ける行は落とす
  filter(!is.na(x0), !is.na(y0), !is.na(x1), !is.na(y1))

# === 3) 2点からLINESTRINGを作成（WGS84） ===
lines_sfc <- pmap(
  edges[, c("x0","y0","x1","y1")],
  ~ st_linestring(matrix(c(..1, ..2,  # (x0, y0)
                           ..3, ..4), # (x1, y1)
                         ncol = 2, byrow = TRUE))
)

edges_sf <- st_sf(edges, geometry = st_sfc(lines_sfc, crs = 4326))

# === 4) 背景地図（輪郭のみ）と重ねて描画（アジア域：西側少し広く） ===
world <- ne_countries(scale = "medium", returnclass = "sf")
if (is.na(st_crs(world))) world <- st_set_crs(world, 4326)

asia_bbox <- st_bbox(c(xmin = 40, xmax = 150, ymin = -10, ymax = 60), crs = st_crs(4326))
world_asia <- st_crop(world, asia_bbox)
edges_asia <- st_crop(edges_sf, asia_bbox)

p <- ggplot() +
  geom_sf(data = world_asia, fill = NA, color = "black", linewidth = 0.3) +
  geom_sf(data = edges_asia,
          aes(linewidth = Value), color = "indianred3", alpha = 0.8) +
  scale_linewidth_continuous(
    range = c(0.2, 2.2), 
    breaks = seq(0, ceiling(max(edges_asia$Value) / 25) * 25, 25)  # 25刻み
  ) +
  coord_sf(xlim = c(50, 150), ylim = c(-10, 60), expand = FALSE) +
  theme_void() +
  labs(title = "300C-ADV Transmission capacity (GW)")

print(p)

ggsave("output/link/300C-ADV_transmission_cap_asia.png", p,
       width = 180, height = 110, units = "mm", dpi = 300, bg = "white")


#300C_CNS
# === 1) ノード座標を用意 ===
g_nodes <- st_read("C:/Users/halra/OneDrive - Kyoto University/My work/AIM-PSY/nodes_centroid.geojson",
                   quiet = TRUE)

nodes_tbl <- g_nodes %>%
  st_drop_geometry() %>%
  transmute(N_PSY,
            lon = as.numeric(centroid_x),
            lat = as.numeric(centroid_y))

# === 2) エッジ(送電線)に座標を結合 ===
# N0/N1に "-AC" が残っている可能性があれば消す
ddir <- "C:/Users/halra/OneDrive - Kyoto University/My work/AIM-PSY/Asia_link/3HOUR/300C_CNS"
df_lnkcap_CNS <- rgdx.param(str_c(ddir,'/gdx_primary/300C-/2050.gdx'),'vfs_l') %>% 
  mutate(across(where(is.factor),~as.character(.))) %>%
  filter(str_detect(L,'.*-AC_.*-AC$'))%>% 
  rename(Value="vfs_l") %>% 
  separate(L,sep='_',into=c('N0','N1'),remove=FALSE) %>% 
  mutate(N0 = str_remove(N0, "-AC"), N1 = str_remove(N1, "-AC"))

edges <- df_lnkcap_CNS %>%
  mutate(N0 = str_remove_all(N0, "-AC"),
         N1 = str_remove_all(N1, "-AC")) %>%
  left_join(nodes_tbl, by = c("N0" = "N_PSY")) %>%
  rename(x0 = lon, y0 = lat) %>%
  left_join(nodes_tbl, by = c("N1" = "N_PSY")) %>%
  rename(x1 = lon, y1 = lat) %>%
  # 座標が欠ける行は落とす
  filter(!is.na(x0), !is.na(y0), !is.na(x1), !is.na(y1))

# === 3) 2点からLINESTRINGを作成（WGS84） ===
lines_sfc <- pmap(
  edges[, c("x0","y0","x1","y1")],
  ~ st_linestring(matrix(c(..1, ..2,  # (x0, y0)
                           ..3, ..4), # (x1, y1)
                         ncol = 2, byrow = TRUE))
)

edges_sf <- st_sf(edges, geometry = st_sfc(lines_sfc, crs = 4326))

# === 4) 背景地図（輪郭のみ）と重ねて描画（アジア域：西側少し広く） ===
world <- ne_countries(scale = "medium", returnclass = "sf")
if (is.na(st_crs(world))) world <- st_set_crs(world, 4326)

asia_bbox <- st_bbox(c(xmin = 40, xmax = 150, ymin = -10, ymax = 60), crs = st_crs(4326))
world_asia <- st_crop(world, asia_bbox)
edges_asia <- st_crop(edges_sf, asia_bbox)

p <- ggplot() +
  geom_sf(data = world_asia, fill = NA, color = "black", linewidth = 0.3) +
  geom_sf(data = edges_asia,
          aes(linewidth = Value), color = "#738ac8", alpha = 0.8) +
  scale_linewidth_continuous(
    range = c(0.2, 2.2), 
    breaks = seq(0, ceiling(max(edges_asia$Value) / 25) * 25, 25)  # 25刻み
  ) +
  coord_sf(xlim = c(50, 150), ylim = c(-10, 60), expand = FALSE) +
  theme_void() +
  labs(title = "300C-CNS Transmission capacity (GW)")

print(p)

ggsave("output/link/300C-CNS_transmission_cap_asia.png", p,
       width = 180, height = 110, units = "mm", dpi = 300, bg = "white")
# -------------------------------------------------------------------------
# 
# # 1) 読み込み
# g <- st_read("C:/Users/halra/OneDrive - Kyoto University/My work/AIM-PSY/nodes_centroid.geojson", quiet = TRUE)
# 
# # 2) properties の経緯度から POINT を作成（WGS84）
# pts <- g %>%
#   st_drop_geometry() %>%
#   st_as_sf(coords = c("centroid_x", "centroid_y"), crs = 4326)
# 
# # 3) 世界地図（WGS84に統一）
# world <- ne_countries(scale = "medium", returnclass = "sf")
# if (is.na(st_crs(world))) world <- st_set_crs(world, 4326)
# pts  <- st_transform(pts, 4326)
# world <- st_transform(world, 4326)
# 
# # 4) アジア範囲で切り出し（任意：描画を軽くする）
# asia_bbox <- st_bbox(c(xmin = 40, xmax = 150, ymin = -20, ymax = 60), crs = st_crs(4326))
# pts_asia   <- st_crop(pts,   asia_bbox)
# world_asia <- st_crop(world, asia_bbox)
# 
# # 5) プロット（投影は変えず、度のまま xlim/ylim を指定）
# p <- ggplot() +
#   geom_sf(data = world_asia, fill = NA, color = "gray80", linewidth = 0.3) +
#   geom_sf(data = pts_asia, size = 0.6, alpha = 0.9) +  # 点を小さく
#   coord_sf(xlim = c(60, 150), ylim = c(-10, 60), expand = FALSE) +
#   theme_void() +
#   labs(title = "Transmission Capacity (GW)")
# 
# print(p)
# 
# ggsave("output/link/points_asia.png", p,
#        width = 130, height = 100, units = "mm", dpi = 300, bg = "white")



# Global interconnection capacity ------------------------------------------------

#300C_ADV
# === 1) ノード座標を用意 ===
g_nodes <- st_read("C:/Users/Haru Sakata/OneDrive - Kyoto University/My work/AIM-PSY/nodes_centroid.geojson",
                   quiet = TRUE)

nodes_tbl <- g_nodes %>%
  st_drop_geometry() %>%
  transmute(N_PSY,
            lon = as.numeric(centroid_x),
            lat = as.numeric(centroid_y))

# === 2) エッジ(送電線)に座標を結合 ===
# N0/N1に "-AC" が残っている可能性があれば消す
ddir <- "C:/Users/halra/OneDrive - Kyoto University/My work/AIM-PSY/Global"
df_lnkcap_ADV <- rgdx.param(str_c(ddir,'/gdx_primary/300C+/2050.gdx'),'vfs_l') %>% 
  mutate(across(where(is.factor),~as.character(.))) %>%
  filter(str_detect(L,'.*-AC_.*-AC$'))%>% 
  rename(Value="vfs_l") %>% 
  separate(L,sep='_',into=c('N0','N1'),remove=FALSE) %>% 
  mutate(N0 = str_remove(N0, "-AC"), N1 = str_remove(N1, "-AC"))

edges <- df_lnkcap_ADV %>%
  mutate(N0 = str_remove_all(N0, "-AC"),
         N1 = str_remove_all(N1, "-AC")) %>%
  left_join(nodes_tbl, by = c("N0" = "N_PSY")) %>%
  rename(x0 = lon, y0 = lat) %>%
  left_join(nodes_tbl, by = c("N1" = "N_PSY")) %>%
  rename(x1 = lon, y1 = lat) %>%
  # 座標が欠ける行は落とす
  filter(!is.na(x0), !is.na(y0), !is.na(x1), !is.na(y1))

# === 3) 2点からLINESTRINGを作成（WGS84） ===
lines_sfc <- pmap(
  edges[, c("x0","y0","x1","y1")],
  ~ st_linestring(matrix(c(..1, ..2,  # (x0, y0)
                           ..3, ..4), # (x1, y1)
                         ncol = 2, byrow = TRUE))
)

edges_sf <- st_sf(edges, geometry = st_sfc(lines_sfc, crs = 4326))

# === 4) 背景地図（世界全体）と重ねて描画 ===
world  <- ne_countries(scale = "medium", returnclass = "sf")
if (is.na(st_crs(world))) world <- st_set_crs(world, 4326)

# 背景地図 & プロット
p <- ggplot() +
  geom_sf(data = world, fill = NA, color = "black", linewidth = 0.3) +
  geom_sf(data = edges_sf,
          aes(linewidth = Value), color = "indianred3", alpha = 0.8) +
  scale_linewidth_continuous(
    range  = c(0.2, 2.2),
    breaks = seq(0, ceiling(max(edges_sf$Value) / 25) * 25, 25)
  ) +
  coord_sf(
    ylim = c(-60, 75),  # ←⭐ここで緯度制限（極域をカット）
    expand = FALSE
  ) +
  theme_void() +
  labs(title = "300C-CNS Transmission capacity (GW)")

print(p)

# サイズを大きくする（幅×高さをmm単位で拡大）
ggsave("output/link/global_300C-ADV_transmission_cap.png", p,
       width = 300, height = 130, units = "mm", dpi = 300, bg = "white")


#300C_CNS
# === 1) ノード座標を用意 ===
g_nodes <- st_read("C:/Users/halra/OneDrive - Kyoto University/My work/AIM-PSY/nodes_centroid.geojson",
                   quiet = TRUE)

nodes_tbl <- g_nodes %>%
  st_drop_geometry() %>%
  transmute(N_PSY,
            lon = as.numeric(centroid_x),
            lat = as.numeric(centroid_y))

# === 2) エッジ(送電線)に座標を結合 ===
# N0/N1に "-AC" が残っている可能性があれば消す
ddir <- "C:/Users/halra/OneDrive - Kyoto University/My work/AIM-PSY/global"
df_lnkcap_CNS <- rgdx.param(str_c(ddir,'/gdx_primary/300C-/2050.gdx'),'vfs_l') %>% 
  mutate(across(where(is.factor),~as.character(.))) %>%
  filter(str_detect(L,'.*-AC_.*-AC$'))%>% 
  rename(Value="vfs_l") %>% 
  separate(L,sep='_',into=c('N0','N1'),remove=FALSE) %>% 
  mutate(N0 = str_remove(N0, "-AC"), N1 = str_remove(N1, "-AC"))

edges <- df_lnkcap_CNS %>%
  mutate(N0 = str_remove_all(N0, "-AC"),
         N1 = str_remove_all(N1, "-AC")) %>%
  left_join(nodes_tbl, by = c("N0" = "N_PSY")) %>%
  rename(x0 = lon, y0 = lat) %>%
  left_join(nodes_tbl, by = c("N1" = "N_PSY")) %>%
  rename(x1 = lon, y1 = lat) %>%
  # 座標が欠ける行は落とす
  filter(!is.na(x0), !is.na(y0), !is.na(x1), !is.na(y1))

# === 3) 2点からLINESTRINGを作成（WGS84） ===
lines_sfc <- pmap(
  edges[, c("x0","y0","x1","y1")],
  ~ st_linestring(matrix(c(..1, ..2,  # (x0, y0)
                           ..3, ..4), # (x1, y1)
                         ncol = 2, byrow = TRUE))
)

edges_sf <- st_sf(edges, geometry = st_sfc(lines_sfc, crs = 4326))

# === 4) 背景地図（世界全体）と重ねて描画 ===
world  <- ne_countries(scale = "medium", returnclass = "sf")
if (is.na(st_crs(world))) world <- st_set_crs(world, 4326)

# 背景地図 & プロット
p <- ggplot() +
  geom_sf(data = world, fill = NA, color = "black", linewidth = 0.3) +
  geom_sf(data = edges_sf,
          aes(linewidth = Value), color = "royalblue4", alpha = 0.8) +
  scale_linewidth_continuous(
    range  = c(0.2, 2.2),
    breaks = seq(0, ceiling(max(edges_sf$Value) / 25) * 25, 25)
  ) +
  coord_sf(
    ylim = c(-60, 75),  # ←⭐ここで緯度制限（極域をカット）
    expand = FALSE
  ) +
  theme_void() +
  labs(title = "300C-CNS Transmission capacity (GW)")

print(p)

# サイズを大きくする（幅×高さをmm単位で拡大）
ggsave("output/link/global_300C-CNS_transmission_cap.png", p,
       width = 300, height = 130, units = "mm", dpi = 300, bg = "white")


# linkの点線プロット -------------------------------------------------------------

# === 1) ノード座標を用意 ===
g_nodes <- st_read("C:/Users/Haru Sakata/OneDrive - Kyoto University/My work/AIM-PSY/nodes_centroid.geojson",
                   quiet = TRUE)

nodes_tbl <- g_nodes %>%
  st_drop_geometry() %>%
  transmute(N_PSY,
            lon = as.numeric(centroid_x),
            lat = as.numeric(centroid_y))

# === 2) エッジ(送電線)に座標を結合（text=='Y' のみ使う） ===
# ddir <- "C:/Users/halra/OneDrive - Kyoto University/My work/AIM-PSY/global"
df_lnkcap_CNS <- rgdx.set(str_c(ddir,'/gams_output/gdx_primary/300C+/2050.gdx'),'L') %>% 
  mutate(across(where(is.factor), ~ as.character(.))) %>%
  filter(str_detect(L_ALL, '.*-AC_.*-AC$')) %>% 
  separate(L_ALL, sep = '_', into = c('N0','N1'), remove = FALSE) %>% 
  mutate(N0 = str_remove_all(N0, "-AC"),
         N1 = str_remove_all(N1, "-AC"))
edges <- df_lnkcap_CNS %>%
  left_join(nodes_tbl, by = c("N0" = "N_PSY")) %>%
  rename(x0 = lon, y0 = lat) %>%
  left_join(nodes_tbl, by = c("N1" = "N_PSY")) %>%
  rename(x1 = lon, y1 = lat) %>%
  filter(!is.na(x0), !is.na(y0), !is.na(x1), !is.na(y1))

# === 3) 2点から LINESTRING を作成（WGS84） ===
lines_sfc <- pmap(
  edges[, c("x0","y0","x1","y1")],
  ~ st_linestring(matrix(c(..1, ..2,   # (x0, y0)
                           ..3, ..4),  # (x1, y1)
                         ncol = 2, byrow = TRUE))
)
edges_sf <- st_sf(edges, geometry = st_sfc(lines_sfc, crs = 4326))

# === 4) 背景地図（世界全体、極域カット）と重ねて描画 ===
world <- ne_countries(scale = "medium", returnclass = "sf")
if (is.na(st_crs(world))) world <- st_set_crs(world, 4326)

# 背景地図 & プロット
p <- ggplot() +
  geom_sf(data = world, fill = NA, color = "black", linewidth = 0.3) +
  geom_sf(data = edges_sf,
          color = "grey50",        # グレー（やや中間色）
          linewidth = 0.3,         # 細めに修正
          linetype = "dotted",     # 点線
          alpha = 0.9) +
  coord_sf(ylim = c(-60, 75), expand = FALSE) +  # 極域を表示しない
  theme_void() +
  labs(title = "300C-CNS Transmission links (dotted grey, thin)")

print(p)

# 保存
ggsave("output/link/global_300C-CNS_transmission_links_dotted.png", p,
       width = 300, height = 130, units = "mm", dpi = 300, bg = "white")

# 点線＋送電網容量プロット-------------------------------------------------------------------------

# linkの点線プロット 
# === 1) ノード座標を用意 ===
g_nodes <- st_read("C:/Users/Haru Sakata/OneDrive - Kyoto University/My work/AIM-PSY/nodes_centroid.geojson",
                   quiet = TRUE)

nodes_tbl <- g_nodes %>%
  st_drop_geometry() %>%
  transmute(N_PSY,
            lon = as.numeric(centroid_x),
            lat = as.numeric(centroid_y))

# === 2) エッジ(送電線)に座標を結合（text=='Y' のみ使う） ===
ddir <- "C:/Users/Haru Sakata/OneDrive - Kyoto University/My work/AIM-PSY/Global"
df_lnkcap_CNS <- rgdx.set(str_c(ddir,'/gams_output/gdx_primary/300C+/2050.gdx'),'L') %>% 
  mutate(across(where(is.factor), ~ as.character(.))) %>%
  filter(str_detect(L_ALL, '.*-AC_.*-AC$')) %>% 
  separate(L_ALL, sep = '_', into = c('N0','N1'), remove = FALSE) %>% 
  mutate(N0 = str_remove_all(N0, "-AC"),
         N1 = str_remove_all(N1, "-AC"))
edges <- df_lnkcap_CNS %>%
  left_join(nodes_tbl, by = c("N0" = "N_PSY")) %>%
  rename(x0 = lon, y0 = lat) %>%
  left_join(nodes_tbl, by = c("N1" = "N_PSY")) %>%
  rename(x1 = lon, y1 = lat) %>%
  filter(!is.na(x0), !is.na(y0), !is.na(x1), !is.na(y1))

# === 3) 2点から LINESTRING を作成（WGS84） ===
lines_sfc <- pmap(
  edges[, c("x0","y0","x1","y1")],
  ~ st_linestring(matrix(c(..1, ..2,   # (x0, y0)
                           ..3, ..4),  # (x1, y1)
                         ncol = 2, byrow = TRUE))
)
edges_sf_dot <- st_sf(edges, geometry = st_sfc(lines_sfc, crs = 4326))

# # === 4) 背景地図（世界全体、極域カット）と重ねて描画 ===
# world <- ne_countries(scale = "medium", returnclass = "sf")
# if (is.na(st_crs(world))) world <- st_set_crs(world, 4326)
# 
# # 背景地図 & プロット
# p <- ggplot() +
#   geom_sf(data = world, fill = NA, color = "black", linewidth = 0.3) +
#   geom_sf(data = edges_sf_dot,
#           color = "grey50",        # グレー（やや中間色）
#           linewidth = 0.3,         # 細めに修正
#           linetype = "dotted",     # 点線
#           alpha = 0.9) +
#   coord_sf(ylim = c(-60, 75), expand = FALSE) +  # 極域を表示しない
#   theme_void() +
#   labs(title = "300C-CNS Transmission links (dotted grey, thin)")
# 
# print(p)
# 
# # 保存
# ggsave("output/link/global_300C-CNS_transmission_links_dotted.png", p,
#        width = 300, height = 130, units = "mm", dpi = 300, bg = "white")


#300C_ADV
# === 1) ノード座標を用意 ===
g_nodes <- st_read("C:/Users/Haru Sakata/OneDrive - Kyoto University/My work/AIM-PSY/nodes_centroid.geojson",
                   quiet = TRUE)

nodes_tbl <- g_nodes %>%
  st_drop_geometry() %>%
  transmute(N_PSY,
            lon = as.numeric(centroid_x),
            lat = as.numeric(centroid_y))

# === 2) エッジ(送電線)に座標を結合 ===
# N0/N1に "-AC" が残っている可能性があれば消す
ddir <- "C:/Users/Haru Sakata/OneDrive - Kyoto University/My work/AIM-PSY/Global"
df_lnkcap_ADV <- rgdx.param(str_c(ddir,'/gams_output/gdx_primary/300C+/2050.gdx'),'vfs_l') %>% 
  mutate(across(where(is.factor),~as.character(.))) %>%
  filter(str_detect(L,'.*-AC_.*-AC$'))%>% 
  rename(Value="vfs_l") %>% 
  separate(L,sep='_',into=c('N0','N1'),remove=FALSE) %>% 
  mutate(N0 = str_remove(N0, "-AC"), N1 = str_remove(N1, "-AC"))

edges <- df_lnkcap_ADV %>%
  mutate(N0 = str_remove_all(N0, "-AC"),
         N1 = str_remove_all(N1, "-AC")) %>%
  left_join(nodes_tbl, by = c("N0" = "N_PSY")) %>%
  rename(x0 = lon, y0 = lat) %>%
  left_join(nodes_tbl, by = c("N1" = "N_PSY")) %>%
  rename(x1 = lon, y1 = lat) %>%
  # 座標が欠ける行は落とす
  filter(!is.na(x0), !is.na(y0), !is.na(x1), !is.na(y1))

# === 3) 2点からLINESTRINGを作成（WGS84） ===
lines_sfc <- pmap(
  edges[, c("x0","y0","x1","y1")],
  ~ st_linestring(matrix(c(..1, ..2,  # (x0, y0)
                           ..3, ..4), # (x1, y1)
                         ncol = 2, byrow = TRUE))
)

edges_sf_ADV <- st_sf(edges, geometry = st_sfc(lines_sfc, crs = 4326))

# === 4) 背景地図（世界全体）と重ねて描画 ===
world  <- ne_countries(scale = "medium", returnclass = "sf")
if (is.na(st_crs(world))) world <- st_set_crs(world, 4326)

# 背景地図 & プロット

# --- 範囲とブレーク（必要に応じて vmin を 25 に） ---
vmin <- 0
vmax <- ceiling(max(edges_sf_CNS$Value, na.rm = TRUE) / 25) * 25
brks <- seq(max(25, vmin), vmax, by = 25)

# 青系の段階色（ブレーク数に合わせて）
cols <- colorRampPalette(c("indianred3", "lightsalmon3", "lightsalmon"))(length(brks))

# --- 値を範囲 [vmin, vmax] にクランプ（NA も除去） ---
edges_plot <- edges_sf_ADV %>%
  filter(!is.na(Value)) %>%
  mutate(Value2 = pmin(pmax(Value, vmin), vmax))

p <- ggplot() +
  geom_sf(data = world,      fill = NA, color = "gray70", linewidth = 0.3) +
  geom_sf(data = edges_sf_dot, color = "grey40", linewidth = 0.3,
          linetype = "dotted", alpha = 0.9) +
  # 太さも色も同じ値（クランプ済み Value2）にマッピング
  geom_sf(data = edges_plot, aes(linewidth = Value2, color = Value2), alpha = 0.9) +
  
  # 太さスケール（legend として表示）
  scale_linewidth_continuous(
    range  = c(0.5, 3.0),
    breaks = brks,
    limits = c(vmin, vmax),
    name   = "Capacity (GW)",
    guide  = guide_legend()
  ) +
  # 色スケール（25GW刻みの段階色、legend 方式）
  scale_color_stepsn(
    colors = cols,
    breaks = brks,
    limits = c(vmin, vmax),
    name   = "Capacity (GW)",
    guide  = guide_legend()
  ) +
  
  coord_sf(ylim = c(-60, 75), expand = FALSE) +
  theme_void() +
  theme(
    # 位置はお好みで。古い ggplot2 なら数値、3.5+ なら legend.position.inside を使用
    legend.position      = c(0.14, 0.26),
    legend.direction     = "vertical",
    legend.background    = element_rect(fill = "white", color = "grey85"),
    legend.key.width     = unit(7, "mm"),
    legend.key.height    = unit(4, "mm"),
    plot.title = element_text(hjust = 0.5) 
  ) +
  # “1つ” の凡例に統合（同じ name/breaks/limits を共有）
  guides(
    color     = guide_legend(ncol = 1, order = 1),
    linewidth = guide_legend(ncol = 1, order = 1)
  ) +
  labs(title = "300C-ADV Transmission capacity (GW)")

print(p)

# サイズを大きくする（幅×高さをmm単位で拡大）
ggsave("output/link_global/global_300C-ADV_transmission_cap_dot.png", p,
       width = 280, height = 120, units = "mm", dpi = 300, bg = "white")


# ADV 容量の値を表示してみる ---------------------------------------------------------

# 1) 中点ポイントを作成（lon/lat は単純平均）
midpoints <- edges_sf_ADV %>%
  st_drop_geometry() %>%
  transmute(
    lon = (x0 + x1)/2,
    lat = (y0 + y1)/2,
    Value,         # 元の容量
    Value2 = pmin(pmax(Value, vmin), vmax)  # 図の線幅・色に合わせたクランプ後
  ) %>%
  # 50GW以下に絞る（ゼロやNAは除外）
  filter(!is.na(Value), Value > 0, Value <= 25) %>%
  mutate(
    # ラベル表記：25GW刻みに合わせたいなら accuracy = 25
    lbl = label_number(accuracy = 0.1)(Value)
  ) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# 2) ラベルを描画（重なりは check_overlap で軽減）
p2 <- p +
  geom_sf_text(
    data = midpoints,
    aes(label = lbl),
    size = 2.6,
    color = "grey15",
    check_overlap = TRUE
  )

# サイズを大きくする（幅×高さをmm単位で拡大）
ggsave("output/link_global/global_300C-ADV_transmission_realvalue.png", p2,
       width = 280, height = 120, units = "mm", dpi = 300, bg = "white")

# -------------------------------------------------------------------------

#300C_CNS
#  1) ノード座標を用意 
g_nodes <- st_read("C:/Users/Haru Sakata/OneDrive - Kyoto University/My work/AIM-PSY/nodes_centroid.geojson",
                   quiet = TRUE)

nodes_tbl <- g_nodes %>%
  st_drop_geometry() %>%
  transmute(N_PSY,
            lon = as.numeric(centroid_x),
            lat = as.numeric(centroid_y))

# 2) エッジ(送電線)に座標を結合 
# N0/N1に "-AC" が残っている可能性があれば消す
ddir <- "C:/Users/Haru Sakata/OneDrive - Kyoto University/My work/AIM-PSY/Global"
df_lnkcap_CNS <- rgdx.param(str_c(ddir,'/gams_output/gdx_primary/300C-/2050.gdx'),'vfs_l') %>% 
  mutate(across(where(is.factor),~as.character(.))) %>%
  filter(str_detect(L,'.*-AC_.*-AC$'))%>% 
  rename(Value="vfs_l") %>% 
  separate(L,sep='_',into=c('N0','N1'),remove=FALSE) %>% 
  mutate(N0 = str_remove(N0, "-AC"), N1 = str_remove(N1, "-AC"))

edges <- df_lnkcap_CNS %>%
  mutate(N0 = str_remove_all(N0, "-AC"),
         N1 = str_remove_all(N1, "-AC")) %>%
  left_join(nodes_tbl, by = c("N0" = "N_PSY")) %>%
  rename(x0 = lon, y0 = lat) %>%
  left_join(nodes_tbl, by = c("N1" = "N_PSY")) %>%
  rename(x1 = lon, y1 = lat) %>%
  # 座標が欠ける行は落とす
  filter(!is.na(x0), !is.na(y0), !is.na(x1), !is.na(y1))

# 3) 2点からLINESTRINGを作成（WGS84）
lines_sfc <- pmap(
  edges[, c("x0","y0","x1","y1")],
  ~ st_linestring(matrix(c(..1, ..2,  # (x0, y0)
                           ..3, ..4), # (x1, y1)
                         ncol = 2, byrow = TRUE))
)

edges_sf_CNS <- st_sf(edges, geometry = st_sfc(lines_sfc, crs = 4326))

# 4) 背景地図（世界全体）と重ねて描画 
world  <- ne_countries(scale = "medium", returnclass = "sf")
if (is.na(st_crs(world))) world <- st_set_crs(world, 4326)

# 背景地図 & プロット

#  範囲とブレーク（必要に応じて vmin を 25 に） 
vmin <- 0
vmax <- ceiling(max(edges_sf_CNS$Value, na.rm = TRUE) / 25) * 25
brks <- seq(max(25, vmin), vmax, by = 25)

# 青系の段階色（ブレーク数に合わせて）
cols <- colorRampPalette(c("#214d9a", "#6da1f3", "#6da1d9"))(length(brks))

#  値を範囲 [vmin, vmax] にクランプ（NA も除去） 
edges_plot <- edges_sf_CNS %>%
  filter(!is.na(Value)) %>%
  mutate(Value2 = pmin(pmax(Value, vmin), vmax))

p <- ggplot() +
  geom_sf(data = world,      fill = NA, color = "gray70", linewidth = 0.3) +
  geom_sf(data = edges_sf_dot, color = "grey40", linewidth = 0.3,
          linetype = "dotted", alpha = 0.9) +
  # 太さも色も同じ値（クランプ済み Value2）にマッピング
  geom_sf(data = edges_plot, aes(linewidth = Value2, color = Value2), alpha = 0.9) +
  
  # 太さスケール（legend として表示）
  scale_linewidth_continuous(
    range  = c(0.5, 3.0),
    breaks = brks,
    limits = c(vmin, vmax),
    name   = "Capacity (GW)",
    guide  = guide_legend()
  ) +
  # 色スケール（25GW刻みの段階色、legend 方式）
  scale_color_stepsn(
    colors = cols,
    breaks = brks,
    limits = c(vmin, vmax),
    name   = "Capacity (GW)",
    guide  = guide_legend()
  ) +
  
  coord_sf(ylim = c(-60, 75), expand = FALSE) +
  theme_void() +
  theme(
    # 位置はお好みで。古い ggplot2 なら数値、3.5+ なら legend.position.inside を使用
    legend.position      = c(0.08, 0.40),
    legend.direction     = "vertical",
    legend.background    = element_rect(fill = "white", color = "grey85"),
    legend.title = element_text(size = 12),   # タイトル文字サイズ
    legend.text  = element_text(size = 10),   # ラベル文字サイズ
    legend.key.width  = unit(10, "mm"),       # 凡例キー幅
    legend.key.height = unit(6, "mm"),         # 凡例キー高さ
    plot.title = element_text(hjust = 0.5) 
  ) +
  # “1つ” の凡例に統合（同じ name/breaks/limits を共有）
  guides(
    color     = guide_legend(ncol = 1, order = 1),
    linewidth = guide_legend(ncol = 1, order = 1)
  ) +
  labs(title = "300C-CNS Transmission capacity (GW)")

#  容量1GW以下だけ抽出 
edges_lowcap <- edges_plot %>%
  filter(Value <= 1)

#  元の図にオレンジの上書きを追加 
p2 <- ggplot() +
  geom_sf(data = world,      fill = NA, color = "gray70", linewidth = 0.3) +
  geom_sf(data = edges_sf_dot, color = "grey40", linewidth = 0.3,
          linetype = "dotted", alpha = 0.9) +
  # 太さも色も同じ値（クランプ済み Value2）にマッピング
  geom_sf(data = edges_plot, aes(linewidth = Value2, color = Value2), alpha = 0.9) +
  
  # 太さスケール（legend として表示）
  scale_linewidth_continuous(
    range  = c(0.5, 3.0),
    breaks = brks,
    limits = c(vmin, vmax),
    name   = "Capacity (GW)",
    guide  = guide_legend()
  ) +
  # 色スケール（25GW刻みの段階色、legend 方式）
  scale_color_stepsn(
    colors = cols,
    breaks = brks,
    limits = c(vmin, vmax),
    name   = "Capacity (GW)",
    guide  = guide_legend()
  ) +
  geom_sf(
    data = edges_lowcap,
    color = "#FFB347",
    linewidth = 0.8,    # 少し強調する太さ（お好みで）
    alpha = 0.9
  ) +
  coord_sf(ylim = c(-60, 75), expand = FALSE) +
  theme_void() +
  theme(
    # 位置はお好みで。古い ggplot2 なら数値、3.5+ なら legend.position.inside を使用
    legend.position      = c(0.08, 0.40),
    legend.direction     = "vertical",
    legend.background    = element_rect(fill = "white", color = "grey85"),
    legend.title = element_text(size = 12),   # タイトル文字サイズ
    legend.text  = element_text(size = 10),   # ラベル文字サイズ
    legend.key.width  = unit(10, "mm"),       # 凡例キー幅
    legend.key.height = unit(6, "mm"),         # 凡例キー高さ
    plot.title = element_text(hjust = 0.5) 
  ) +
  # “1つ” の凡例に統合（同じ name/breaks/limits を共有）
  guides(
    color     = guide_legend(ncol = 1, order = 1),
    linewidth = guide_legend(ncol = 1, order = 1)
  ) +
  labs(title = "300C-CNS Transmission capacity (GW)")


# サイズを大きくする（幅×高さをmm単位で拡大）
ggsave("output/link_global/global_300C-CNS_transmission_cap_dot.png", p,
       width = 280, height = 120, units = "mm", dpi = 300, bg = "white")

# サイズを大きくする（幅×高さをmm単位で拡大）
ggsave("output/link_global/global_300C-CNS_transmission_cap_1GW.png", p2,
       width = 280, height = 120, units = "mm", dpi = 300, bg = "white")



# CNS 容量の値を表示してみる ---------------------------------------------------------

# 1) 中点ポイントを作成（lon/lat は単純平均）
midpoints <- edges_sf_CNS %>%
  st_drop_geometry() %>%
  transmute(
    lon = (x0 + x1)/2,
    lat = (y0 + y1)/2,
    Value
  ) %>%
  filter(!is.na(Value), Value > 0, Value <= 25) %>%
  mutate(
    lbl = number(Value, accuracy = 0.1),        # 表示用ラベル
    group = if_else(Value <= 1, "small", "normal")  # 色分け用カテゴリ
  ) %>%
  st_as_sf(coords = c("lon","lat"), crs = 4326)

# 2) ラベルを描画（重なりは check_overlap で軽減）
p2 <- p +
  geom_sf_text(
    data = midpoints,
    aes(label = lbl),
    size = 2.6,
    color = "grey15",
    check_overlap = TRUE
  )

# サイズを大きくする（幅×高さをmm単位で拡大）
ggsave("output/link_global/global_300C-CNS_transmission_realvalue.png", p2,
       width = 280, height = 120, units = "mm", dpi = 300, bg = "white")
