# PLOT SETTINGS ---------------------------------------------------------------
PSM_SCENARIO <- 'global_H2_Transport'
PSM_REGION <- 'Global'
PSM_DATE <- '2611061840'
REFERENCE_DATE <- '2610281619'
PSM_COMMIT <- 'd73f5795dc70b8c5fad8c83ecc27c3d7823f8fd7'
YEAR <- 2050L
timeresol <- '6HOUR'
reference_timeresol <- '12DAY'
target_scenarios <- c('500C-H2_Pipeline_Truck-NH3_Rail_Truck_Ship')
FLOW_THRESHOLD <- 0.001 # EJ/year: absolute difference displayed on maps only.

# PACKAGES, SHARED READERS AND RUN PATHS ---------------------------------------
suppressPackageStartupMessages({
  library(gdxrrw); library(dplyr); library(tidyr); library(ggplot2)
  library(jsonlite); library(cowplot)
})
options(warn=1,pillar.sigfig=6)
script <- sub('^--file=','',commandArgs(FALSE)[grepl('^--file=',commandArgs(FALSE))])
CODE_DIR <- dirname(normalizePath(script[[1]],winslash='/'))
PROJECT_DIR <- normalizePath(file.path(CODE_DIR,'..'),winslash='/')
WORKSPACE <- normalizePath(file.path(PROJECT_DIR,'../../../..'),winslash='/')
PLOT_REPO <- file.path(WORKSPACE,'AIMPSY_plot')
GEO_DIR <- file.path(PLOT_REPO,'IntermReport/data')
PSM_OUTPUT_ROOT <- '//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output'
OUTPUT_DIR <- file.path(PROJECT_DIR,'output',paste(PSM_SCENARIO,PSM_REGION,
  PSM_DATE,'vs',REFERENCE_DATE,'difference',YEAR,sep='_'))
if(length(target_scenarios)!=1L || anyNA(target_scenarios) ||
   !nzchar(trimws(target_scenarios))) stop('Select one nonblank scenario.')
if(dir.exists(OUTPUT_DIR) && length(list.files(OUTPUT_DIR,pattern='[.]png$')) &&
   !'--replace-generated' %in% commandArgs(TRUE)) stop('Output exists; use --replace-generated explicitly.')
source(file.path(PLOT_REPO,'IntermReport/code/data_import.R'))
source(file.path(CODE_DIR,'transport_summary.R'))
source(file.path(CODE_DIR,'transport_network_functions.R'))
MODE_TYPES <- c('H2 pipeline'='solid','H2 road'='dotted','NH3 rail'='solid',
  'NH3 road'='dotted','NH3 ship'='solid','Synfuel ship'='solid')
SIGN_COLORS <- c('12DAY lower than 6HOUR'='#C43C39','12DAY higher than 6HOUR'='#286CB0')
run_paths <- c(setNames(file.path(PSM_OUTPUT_ROOT,PSM_SCENARIO,PSM_REGION,PSM_DATE),timeresol),
  setNames(file.path(PSM_OUTPUT_ROOT,PSM_SCENARIO,PSM_REGION,REFERENCE_DATE),reference_timeresol))
gdxrrw::igdx('C:/GAMS/win64/26.1')
geo <- read_geodata()

# SOURCE CHECKS AND ANNUAL FLOWS WITHOUT THE MAP NOISE CUTOFF -------------------
directional <- list()
for(res in names(run_paths)) {
  RUN_DIR <- run_paths[[res]]
  TIME_RESOL <- res
  TIME_WEIGHT <- primary_time_weight_from_timeresol(res)
  version <- readLines(file.path(RUN_DIR,'modelstat/systemlog/systemlog.txt'),warn=FALSE)
  if(!any(grepl(PSM_COMMIT,version,fixed=TRUE))) stop('Unexpected producer commit.')
  status <- read.csv(file.path(RUN_DIR,'main/scenario_table.csv'))
  row <- filter(status,Scenario %in% target_scenarios)
  if(nrow(row)!=1L || row$Status!='Optimal' || row$calc_end_year<YEAR) stop('Incomplete run.')
  wt <- gdxrrw::rgdx.param(file.path(RUN_DIR,'data/input',paste0(target_scenarios,'.gdx')),'wt')
  if(any(abs(wt$wt-TIME_WEIGHT)>1e-10) || !'y00' %in% as.character(wt$T)) stop('Unexpected saved wt.')
  # annual retains every classified raw flow; gross/shipping have a noise cutoff.
  annual <- read_case(target_scenarios[[1]],geo)$annual
  z <- annual %>% filter(!is.na(Mode)) %>% link_endpoints() %>%
    mutate(Point0=physical_point(Point0),Point1=physical_point(Point1)) %>%
    filter(Point0!=Point1)
  sea <- z$Mode=='NH3 ship'
  z$Point0[sea] <- unname(geo$ports[z$Point0[sea]])
  z$Point1[sea] <- unname(geo$ports[z$Point1[sea]])
  if(anyNA(z$Point0)||anyNA(z$Point1)) stop('Unmapped shipping gateway.')
  directional[[res]] <- z %>% mutate(
    Origin=ifelse(Direction=='vff_l',Point0,Point1),
    Destination=ifelse(Direction=='vff_l',Point1,Point0),
    Carrier=case_when(grepl('^H2',Mode)~'Hydrogen',grepl('^NH3',Mode)~'Ammonia',TRUE~'Synfuel')) %>%
    group_by(Carrier,Mode,Origin,Destination) %>% summarise(FlowEJ=sum(FlowEJ),.groups='drop')
  if(any(directional[[res]]$FlowEJ<0)) stop('Negative directional flow.')
  message(res,': annualized with wt=',TIME_WEIGHT,' including y00.')
}

# DIRECTIONAL DIFFERENCES AND THE FRACTION OF 6HOUR FLOW MATCHED BY 12DAY ---------
key <- c('Carrier','Mode','Origin','Destination')
delta <- full_join(rename(directional[[reference_timeresol]],Flow12=FlowEJ),
  rename(directional[[timeresol]],Flow6=FlowEJ),by=key) %>%
  mutate(Flow12=replace_na(Flow12,0),Flow6=replace_na(Flow6,0),
    Delta=Flow12-Flow6,AbsDelta=abs(Delta),
    Sign=ifelse(Delta<0,names(SIGN_COLORS)[1],names(SIGN_COLORS)[2]),
    Sea=grepl('ship$',Mode),Start=pmin(Origin,Destination),End=pmax(Origin,Destination))
if(any(!is.finite(delta$Delta))) stop('Nonfinite difference.')
metrics <- delta %>% group_by(Carrier) %>% summarise(
  Total12=sum(Flow12),Total6=sum(Flow6),Matched=sum(pmin(Flow12,Flow6)),
  Shortfall=sum(pmax(-Delta,0)),Excess=sum(pmax(Delta,0)),
  MatchedPercent=100*Matched/Total6,
  ReferenceArrows=sum(Flow6>=FLOW_THRESHOLD),
  SharedArrows=sum(Flow6>=FLOW_THRESHOLD & Flow12>=FLOW_THRESHOLD),
  RouteCoveragePercent=100*SharedArrows/ReferenceArrows,
  VisibleArrows=sum(AbsDelta>=FLOW_THRESHOLD),
  HiddenAbsDifference=sum(AbsDelta[AbsDelta<FLOW_THRESHOLD]),.groups='drop')
if(any(abs(metrics$Matched+metrics$Shortfall-metrics$Total6)>1e-9) ||
   any(abs(metrics$Matched+metrics$Excess-metrics$Total12)>1e-9)) stop('Overlap accounting failed.')
message('\nDirected-link + mode overlap, without display threshold:')
print(metrics,n=Inf,width=Inf)
message('\nLargest directional differences, 12DAY minus 6HOUR (EJ/year):')
print(delta %>% arrange(desc(AbsDelta)) %>% select(all_of(key),Flow12,Flow6,Delta) %>% head(18),width=Inf)
audit <- delta %>% group_by(Mode) %>% summarise(NetDifference=sum(Delta),.groups='drop')
expected <- full_join(directional[[reference_timeresol]] %>% group_by(Mode) %>%
  summarise(A=sum(FlowEJ),.groups='drop'),directional[[timeresol]] %>% group_by(Mode) %>%
  summarise(B=sum(FlowEJ),.groups='drop'),by='Mode') %>% mutate(Expected=replace_na(A,0)-replace_na(B,0))
if(any(abs(left_join(audit,expected,by='Mode') %>% transmute(Error=NetDifference-Expected) %>% pull(Error))>1e-9))
  stop('Directional differences do not reconcile with gross totals.')

# LAND PATHS WITH ARROWS IN THE PHYSICAL TRANSPORT DIRECTION --------------------
visible <- filter(delta,AbsDelta>=FLOW_THRESHOLD)
coords <- function(z,point,axis,sea) {
  lookup <- if(sea) geo$shipping_coordinates else geo$coordinates
  lookup[[axis]][match(z[[point]],lookup$Point)]
}
land <- visible %>% filter(!Sea) %>% group_by(Start,End) %>%
  arrange(match(Mode,names(MODE_TYPES)),Origin,.by_group=TRUE) %>%
  mutate(Lane=(n()+1)/2-row_number()) %>% ungroup()
land$lon0 <- coords(land,'Start','lon',FALSE);land$lat0 <- coords(land,'Start','lat',FALSE)
land$lon1 <- coords(land,'End','lon',FALSE);land$lat1 <- coords(land,'End','lat',FALSE)
sea <- visible %>% filter(Sea)
sea$lon0 <- coords(sea,'Origin','lon',TRUE);sea$lat0 <- coords(sea,'Origin','lat',TRUE)
sea$lon1 <- coords(sea,'Destination','lon',TRUE);sea$lat1 <- coords(sea,'Destination','lat',TRUE)
for(z in list(land,sea)) if(any(!is.finite(as.matrix(z[,c('lon0','lat0','lon1','lat1')]))))
  stop('Unmapped difference-map endpoint.')
paths <- bind_rows(lapply(seq_len(nrow(land)),function(i) {
  z <- land[i,]
  tt <- seq(0,1,length.out=101)
  end <- z$lon0+((z$lon1-z$lon0+180)%%360)-180
  scale <- max(cos((z$lat0+z$lat1)/2*pi/180),.25)
  dx <- (end-z$lon0)*scale;dy <- z$lat1-z$lat0;len <- sqrt(dx^2+dy^2)
  if(len<1e-6) stop('Coincident land coordinates.')
  taper <- pmin(1,tt/.15,(1-tt)/.15);taper <- taper^2*(3-2*taper)
  shift <- .08*len*tt*(1-tt)+.9*z$Lane*taper
  lon <- z$lon0+(end-z$lon0)*tt-shift*dy/len/scale
  lat <- z$lat0+(z$lat1-z$lat0)*tt+shift*dx/len
  if(z$Origin!=z$Start) {lon<-rev(lon);lat<-rev(lat)}
  tibble(Carrier=z$Carrier,Mode=z$Mode,Sign=z$Sign,AbsDelta=z$AbsDelta,
    PathID=i,Step=seq_along(tt),lon=(lon+180)%%360-180,lat=lat)
})) %>% group_by(PathID) %>% arrange(Step,.by_group=TRUE) %>%
  mutate(Piece=paste(PathID,cumsum(abs(lon-lag(lon,default=first(lon)))>180),sep='_')) %>% ungroup()
# Arrowheads only on the final segment, not on a dateline split.
heads <- paths %>% group_by(PathID) %>% arrange(Step,.by_group=TRUE) %>%
  mutate(lon0=lag(lon),lat0=lag(lat)) %>% filter(Step==max(Step)) %>% ungroup()
if(any(abs(heads$lon-heads$lon0)>180)) stop('Arrowhead segment crosses the dateline.')
FLOW_MAX <- ceiling(max(visible$AbsDelta)*10)/10
FLOW_BREAKS <- c(.01,.1,.5,1,2,5)
FLOW_BREAKS <- FLOW_BREAKS[FLOW_BREAKS<=FLOW_MAX]

# CARRIER MAPS: RED SHORTFALL, BLUE EXCESS, NO NODE PIES -------------------------
make_difference_map <- function(carrier) {
  pth <- filter(paths,Carrier==carrier)
  end <- filter(heads,Carrier==carrier)
  ship <- sea_curve_rows(filter(sea,Carrier==carrier))
  modes <- switch(carrier,Hydrogen=c('H2 pipeline','H2 road'),
    Ammonia=c('NH3 rail','NH3 road','NH3 ship'),Synfuel='Synfuel ship')
  p <- ggplot() + geom_polygon(data=geo$borders,aes(lon,lat,group=Group),
    fill='grey97',color='grey65',linewidth=.22) +
    geom_curve(data=ship,aes(x=lon0,y=lat0,xend=lon1,yend=lat1,
      color=Sign,linewidth=AbsDelta,linetype=Mode),curvature=.12,alpha=.9,
      arrow=grid::arrow(length=grid::unit(1.8,'mm'),type='closed'),show.legend=FALSE) +
    geom_path(data=pth,aes(lon,lat,group=Piece),color='white',linewidth=.65,show.legend=FALSE) +
    geom_path(data=pth,aes(lon,lat,group=Piece,color=Sign,linewidth=AbsDelta,linetype=Mode),
      alpha=.9,show.legend=TRUE) +
    geom_segment(data=end,aes(x=lon0,y=lat0,xend=lon,yend=lat,color=Sign,linewidth=AbsDelta),
      arrow=grid::arrow(length=grid::unit(1.8,'mm'),type='closed'),show.legend=FALSE) +
    scale_color_manual(values=SIGN_COLORS,limits=names(SIGN_COLORS),drop=FALSE,name=NULL) +
    scale_linetype_manual(values=MODE_TYPES,limits=modes,drop=FALSE,name='Mode') +
    scale_linewidth_continuous(limits=c(0,FLOW_MAX),breaks=FLOW_BREAKS,range=c(.15,3),
      trans='sqrt',name='Absolute difference (EJ/yr)') +
    coord_fixed(ratio=1.12,xlim=c(-180,180),ylim=c(-60,85),expand=FALSE) +
    annotate('label',x=-178,y=83,hjust=0,vjust=1,
      label=paste0(if(carrier=='Synfuel') 'Synthetic fuels' else carrier,' | ',YEAR,
        '\n12DAY minus 6HOUR'),size=3.5,linewidth=0,fill=scales::alpha('white',.85)) +
    guides(color=guide_legend(order=1,nrow=1,override.aes=list(linewidth=1.1,linetype='solid',alpha=1)),
      linetype=guide_legend(order=2,nrow=1,override.aes=list(linewidth=1,color='grey30')),
      linewidth=guide_legend(order=3,nrow=1,title.position='left',override.aes=list(color='grey25',linetype='solid'))) +
    theme_void() + theme(plot.background=element_rect(fill='white',color=NA),
      legend.position='bottom',legend.box='vertical',legend.direction='horizontal',
      legend.title=element_text(size=12),legend.text=element_text(size=12),
      legend.key.width=grid::unit(9,'mm'),legend.key.height=grid::unit(4,'mm'),
      legend.spacing.y=grid::unit(1,'mm'),legend.margin=margin(1,1,1,1,unit='mm'),
      plot.margin=margin(0,0,0,0))
  legend <- cowplot::get_legend(gtable::gtable_filter(ggplotGrob(p),'^guide-box-bottom$'))
  z <- filter(metrics,Carrier==carrier)
  note <- ggdraw()+draw_label(sprintf(
    'Matched volume: %.1f%% of 6HOUR (same directed link + mode). Route coverage: %d/%d (%.1f%%).\nShortfall: %.3f; excess: %.3f EJ/yr. Route coverage uses flows >= %.3f EJ/yr in both runs.\nArrow direction = physical transport direction. Map display: |12DAY - 6HOUR| >= %.3f EJ/yr.',
    z$MatchedPercent,z$SharedArrows,z$ReferenceArrows,z$RouteCoveragePercent,
    z$Shortfall,z$Excess,FLOW_THRESHOLD,FLOW_THRESHOLD),size=9,x=.02,hjust=0)
  # Map / dedicated legend band / coverage definition. No legends on geography.
  cowplot::plot_grid(p+theme(legend.position='none'),legend,note,ncol=1,rel_heights=c(135,30,15))
}

# GENERATE AND SAVE THREE CARRIERS AND A COMBINED PANEL -------------------------
dir.create(OUTPUT_DIR,recursive=TRUE,showWarnings=FALSE)
plots <- setNames(lapply(c('Hydrogen','Ammonia','Synfuel'),make_difference_map),
  c('Hydrogen','Ammonia','Synfuel'))
for(carrier in names(plots)) {
  ggsave(file.path(OUTPUT_DIR,paste0('difference_',tolower(carrier),'_',YEAR,'.png')),plots[[carrier]],
    device=ragg::agg_png,width=300,height=180,units='mm',dpi=300,bg='white')
}
# [Hydrogen] / [Ammonia] / [Synthetic fuels], common absolute-difference scale.
combined <- cowplot::plot_grid(plotlist=plots,ncol=1)
ggsave(file.path(OUTPUT_DIR,paste0('difference_carriers_',YEAR,'.png')),combined,
  device=ragg::agg_png,width=300,height=540,units='mm',dpi=300,bg='white')
message('Completed: 4 PNGs; original maps and source GDX unchanged.')
