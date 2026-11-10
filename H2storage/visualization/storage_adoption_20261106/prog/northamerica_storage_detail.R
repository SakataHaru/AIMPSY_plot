# PLOT SETTINGS ---------------------------------------------------------------
PSM_SCENARIO <- 'H2_storage_compressor'
PSM_REGION <- 'NorthAmerica'
PSM_DATE <- '2610291832'
target_scenarios <- c('500C-H2storage')
target_year <- 2045L
timeresol <- '6HOUR'

# PACKAGES, PATHS AND DRAWING SETTINGS ----------------------------------------
.libPaths(c('C:/Users/Haru Sakata/AppData/Local/R/win-library/4.4',.libPaths()))
suppressPackageStartupMessages({library(gdxrrw);library(dplyr);library(tidyr)
  library(ggplot2);library(cowplot);library(sf)})
gdxrrw::igdx('C:/GAMS/win64/26.1')
script <- sub('^--file=','',commandArgs()[grepl('^--file=',commandArgs())])[1]
project <- dirname(dirname(normalizePath(script,winslash='/')))
workspace <- normalizePath(file.path(project,'../../../..'),winslash='/')
source_root <- '//SY_3600_01/atfs02/Individual/hsakata/AIMPSY_2nd/output'
run <- file.path(source_root,PSM_SCENARIO,PSM_REGION,PSM_DATE)
output <- file.path(project,'output',paste0(PSM_REGION,'_',PSM_DATE))
timeresol <- toupper(trimws(timeresol))
weights <- c('12DAY'=ceiling(365/12*1000)/1000,'6HOUR'=6,'4HOUR'=4,'3HOUR'=3,'1HOUR'=1)
stopifnot(timeresol %in% names(weights),length(target_scenarios)==1,nzchar(target_scenarios))
primary_weight <- unname(weights[timeresol])
countries <- c('USA','CAN','MEX')
stores <- c('h2_tank','h2_cavern')
type_labels <- c('Hydrogen tank','Salt cavern')
inventory_colors <- setNames(c('#A6CEE3','#FDBF6F'),type_labels)
map_colors <- setNames(c('#0072B2','#E69F00'),type_labels)
mapping <- read.csv(file.path(workspace,'AIMPSY_plot/IntermReport/data/M_R272.csv')) %>%
  filter(R31 %in% countries) %>% select(N=N_IND,Country=R31)
stopifnot(!anyDuplicated(mapping$N))
CARRIER_COLORS <- c(Synfuel='mediumorchid1')
source(file.path(workspace,'AIMPSY_plot/IntermReport/code/fig3/fig3_common.R'))
geography <- st_read(file.path(workspace,'AIMPSY_plot/IntermReport/data/nodes_centroid.geojson'),quiet=TRUE)

# PRIMARY INVENTORY, CAPACITY AND DAILY BALANCE CHECKS ------------------------
readpar <- function(path,symbol) {
  z <- rgdx.param(path,symbol)
  z[] <- lapply(z,function(x)if(is.factor(x))as.character(x)else x)
  names(z)[ncol(z)] <- 'Value';stopifnot(all(is.finite(z$Value)));z
}
primary <- file.path(run,'gams_output/gdx_primary',target_scenarios,paste0(target_year,'.gdx'))
con <- file(file.path(run,'gams_output/log',target_scenarios,paste0(target_year,'.lst')),'r')
model <- solver <- character()
repeat {
  txt <- readLines(con,n=2000,warn=FALSE);if(!length(txt))break
  model <- c(model,grep('^\\*\\*\\*\\* MODEL STATUS',txt,value=TRUE))
  solver <- c(solver,grep('^\\*\\*\\*\\* SOLVER STATUS',txt,value=TRUE))
  if(length(model)&&length(solver))break
}
close(con)
stopifnot(length(model)==1,length(solver)==1,grepl('STATUS\\s+1\\s+Optimal',model),
  grepl('STATUS\\s+1\\s+Normal',solver),as.numeric(rgdx(primary,list(name='h2_time_version'))$val)==4)
wt <- readpar(primary,'wt')
stopifnot(all(wt$Value[grepl('^d[0-9]{3}$',wt$T)]==1),
  all(wt$Value[grepl('^d[0-9]{3}t',wt$T)]==primary_weight))
cap <- readpar(primary,'ves_l') %>% filter(N %in% mapping$N,S %in% stores)
stopifnot(!anyDuplicated(cap[c('N','I','S')]),all(cap$Value>=-1e-6))
daily <- crossing(cap %>% rename(Capacity=Value),Day=1:365)
for(symbol in c('ve_l','vh_l')) {
  z <- rgdx(primary,list(name=symbol,uels=list(unique(cap$N),unique(cap$I),stores,sprintf('d%03d',1:365))))
  d <- as_tibble(setNames(lapply(1:4,function(j)z$uels[[j]][z$val[,j]]),c('N','I','S','T')))
  d$Value <- z$val[,5];d$Day <- as.integer(sub('d','',d$T));d$T <- NULL
  stopifnot(!anyDuplicated(d[c('N','I','S','Day')]),all(is.finite(d$Value)))
  stopifnot(all(abs(anti_join(d,cap,by=c('N','I','S'))$Value)<1e-5))
  names(d)[names(d)=='Value'] <- symbol
  daily <- left_join(daily,d,by=c('N','I','S','Day'))
  daily[[symbol]] <- replace_na(daily[[symbol]],0)
}
eta <- readpar(primary,'eeta') %>% rename(Retention=Value)
daily <- daily %>% left_join(eta,by=c('N','I','S')) %>% group_by(N,I,S) %>% arrange(Day,.by_group=TRUE) %>%
  mutate(Previous=lag(ve_l,default=last(ve_l)),Residual=ve_l-Retention^24*Previous+vh_l,
    Tolerance=pmax(1e-4,1e-6*Capacity)) %>% ungroup()
stopifnot(!anyNA(daily$Retention),all(daily$ve_l>=-daily$Tolerance),
  all(daily$ve_l<=daily$Capacity+daily$Tolerance),all(abs(daily$Residual)<=daily$Tolerance))
message('Maximum daily balance residual GWh: ',max(abs(daily$Residual)))
inv <- daily %>% left_join(mapping,by='N') %>% group_by(Country,Day,S) %>%
  summarise(Value=sum(pmax(ve_l,0))/1000,.groups='drop') %>%
  mutate(Type=factor(type_labels[match(S,stores)],levels=type_labels),Country=factor(Country,levels=countries))
total <- inv %>% group_by(Country,Day) %>% summarise(Value=sum(Value),.groups='drop')
stopifnot(nrow(total)==365*length(countries),abs(sum(inv$Value)-sum(total$Value))<1e-8)
message('Capacity TWh by R31 / type:')
print(cap %>% left_join(mapping,by='N') %>% group_by(Country,S) %>% summarise(TWh=sum(Value)/1000,.groups='drop'))

# R31 STACKED DAILY INVENTORY, CAVERN BELOW TANK ------------------------------
daily_plot <- function(z,t,title) {
  ggplot(z,aes(Day,Value,fill=Type))+
    geom_area(stat='identity',position=position_stack(reverse=FALSE))+
    geom_line(data=t,aes(Day,Value,color='Total'),inherit.aes=FALSE,linewidth=.45)+
    scale_fill_manual(values=inventory_colors,limits=type_labels,name=NULL)+
    scale_color_manual(values=c(Total='black'),name=NULL)+
    scale_x_continuous(breaks=c(1,60,120,180,240,300,365),limits=c(1,365),expand=c(0,0))+
    scale_y_continuous(limits=c(0,NA),expand=expansion(mult=c(0,.05)))+
    labs(title=title,x='Day of year',y='End-of-day inventory (TWh)')+
    guides(fill=guide_legend(order=1,nrow=1),color=guide_legend(order=2,nrow=1))+
    theme_classic(base_size=11)+theme(plot.title=element_text(face='bold'),
      axis.text=element_text(size=12),axis.title.y=element_text(size=14,angle=90,color='black',margin=margin(r=6)),
      plot.margin=margin(5.5,12,5.5,20,'pt'),
      legend.position='bottom',legend.box='horizontal',legend.key.size=grid::unit(5,'mm'))
}
for(country in countries) {
  p <- daily_plot(filter(inv,Country==country),filter(total,Country==country),
    paste0(country,': daily hydrogen inventory | ',target_year))
  ggsave(file.path(output,paste0('15_daily_inventory_',country,'_',target_year,'.png')),p,
    width=230,height=120,units='mm',dpi=300,bg='white')
}
if('--daily-only' %in% commandArgs(trailingOnly=TRUE)) quit(save='no',status=0)

# SINGLE MAP WITH INSTALLED TANK AND CAVERN CAPACITY PIES ---------------------
points <- cap %>% group_by(N,S) %>% summarise(Value=sum(Value)/1000,.groups='drop') %>%
  mutate(Category=factor(type_labels[match(S,stores)],levels=type_labels),Order=as.integer(Category),Region=N) %>%
  left_join(st_drop_geometry(geography) %>% select(N=N_PSY,lon=centroid_x,lat=centroid_y),by='N')
node_total <- points %>% group_by(N) %>% summarise(Total=sum(Value),.groups='drop')
stopifnot(!anyNA(points$lon),!anyNA(points$lat),abs(sum(points$Value)-sum(cap$Value)/1000)<1e-8)
maximum <- max(node_total$Total)
offmap <- sum(filter(points,lon< -179|lon> -50|lat<5|lat>84)$Value)
message('Total installed capacity TWh: ',sum(points$Value),' | outside map: ',offmap)
map <- ggplot()+geom_sf(data=filter(geography,N_PSY %in% mapping$N),
    fill='#F4F4F4',color='#B0B0B0',linewidth=.2)+
  fig3_pie_layer(points,maximum,max_radius_mm=7.5)+
  scale_fill_manual(values=map_colors,limits=type_labels,drop=FALSE,name=NULL)+
  coord_sf(xlim=c(-179,-50),ylim=c(5,84),expand=FALSE)+
  labs(title=paste0('North America | H2 storage | Total: ',formatC(sum(points$Value),digits=2,format='f'),' TWh'),
    subtitle=paste0(target_year,' | Tank + cavern case'),x=NULL,y=NULL)+
  theme_minimal(base_size=11)+theme(panel.grid=element_blank(),legend.position='bottom',
    plot.title=element_text(face='bold'),axis.text=element_text(size=8))
legend <- cowplot::get_legend(gtable::gtable_filter(ggplotGrob(map),'^guide-box-bottom$'))
br <- scales::breaks_pretty(n=3)(c(0,maximum));br <- br[br>0&br<=maximum]
sizeplot <- ggplot(tibble(Value=br),aes(1,Value,size=Value))+geom_point(shape=21,fill='white',color='grey40')+
  scale_size_area(max_size=15,limits=c(0,maximum),breaks=br,name='Installed capacity (TWh)')+
  guides(size=guide_legend(nrow=1,title.position='left'))+theme_void()+theme(legend.position='bottom')
sizelegend <- cowplot::get_legend(gtable::gtable_filter(ggplotGrob(sizeplot),'^guide-box-bottom$'))
note <- ggdraw()+draw_label(paste0('Circle AREA = installed tank + cavern energy capacity; slices show their shares.\n',
  'Capacity is not daily inventory. Same map window as figure 07. Run ',PSM_DATE,'.\n',
  'Outside-window capacity: ',formatC(offmap,digits=3,format='fg'),' TWh (included in total).'),x=.03,hjust=0,size=8)
# One geographic panel / shared type legend / circle-area key / definition.
map_figure <- plot_grid(map+theme(legend.position='none'),legend,sizelegend,note,ncol=1,
  rel_heights=c(1,.05,.07,.1))
ggsave(file.path(output,paste0('16_storage_capacity_pie_map_',target_year,'.png')),map_figure,
  width=230,height=240,units='mm',dpi=300,bg='white')
