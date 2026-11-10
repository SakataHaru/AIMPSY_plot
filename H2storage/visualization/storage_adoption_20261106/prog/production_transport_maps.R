# PLOT SETTINGS ---------------------------------------------------------------
PSM_SCENARIO <- 'H2_storage_compressor'
PSM_RUNS <- data.frame(Region=c('Asia','NorthAmerica'),Date=c('2610301310','2610291832'))
target_scenarios <- c('500C','500C-H2storage','500C-H2sto_only_tank','500C-H2sto_only_cavern')
scenario_labels <- c('No storage','Tank + cavern','Tank only','Cavern only')
timeresol <- '6HOUR'
target_year <- 2045L
flow_cutoff_ej <- 0.001

# PACKAGES, DERIVED SETTINGS AND SHARED GEOMETRY -------------------------------
.libPaths(c('C:/Users/Haru Sakata/AppData/Local/R/win-library/4.4',.libPaths()))
suppressPackageStartupMessages({library(gdxrrw);library(dplyr);library(tidyr)
  library(ggplot2);library(cowplot);library(sf)})
gdxrrw::igdx('C:/GAMS/win64/26.1')
script <- sub('^--file=','',commandArgs()[grepl('^--file=',commandArgs())])[1]
project <- dirname(dirname(normalizePath(script,winslash='/')))
workspace <- normalizePath(file.path(project,'../../../..'),winslash='/')
source_root <- '//SY_3600_01/atfs02/Individual/hsakata/AIMPSY_2nd/output'
timeresol <- toupper(trimws(timeresol))
weights <- c('12DAY'=ceiling(365/12*1000)/1000,'6HOUR'=6,'4HOUR'=4,'3HOUR'=3,'1HOUR'=1)
stopifnot(timeresol %in% names(weights),length(target_scenarios)==length(scenario_labels),
  !anyDuplicated(target_scenarios),all(nzchar(target_scenarios)))
primary_weight <- unname(weights[timeresol])
CARRIER_COLORS <- c(Synfuel='mediumorchid1')
source(file.path(workspace,'AIMPSY_plot/IntermReport/code/fig3/fig3_common.R'))
# Only function definitions are sourced; no reference-run reads or plot execution.
source(file.path(workspace,'topics/NH3transport/visualization/transport_visualization/code/transport_network_functions.R'))
MODE_COLORS <- c('H2 pipeline'='thistle3','H2 road'='#88618E','NH3 rail'='#8E5EA2',
  'NH3 road'='#C3A3CF','NH3 ship'='#553768','Synfuel ship'='mediumorchid1')
production_colors <- c(Hydrogen='thistle3',Ammonia='#8E5EA2',Synfuel='mediumorchid1')
tech_modes <- c(pipeline_h2='H2 pipeline',truck_h2='H2 road',rail_nh3='NH3 rail',
  truck_nh3='NH3 road',ship_nh3='NH3 ship',ship_liq_synliq='Synfuel ship')
conversion_carriers <- c(electrolysis='Hydrogen',haber_bosch='Ammonia',
  fischer_tropsch='Synfuel',methanation='Synfuel')
regions <- list(Asia=c('CHN','IDN','IND','JPN','KOR','MYS','THA','VNM','XEA','XSA','XSE'),
  NorthAmerica=c('USA','CAN','MEX'))
mapping <- read.csv(file.path(workspace,'AIMPSY_plot/IntermReport/data/M_R272.csv'))
geo_dir <- file.path(workspace,'AIMPSY_plot/IntermReport/data')
geography <- st_read(file.path(geo_dir,'nodes_centroid.geojson'),quiet=TRUE)
coordinates <- st_drop_geometry(geography) %>% transmute(Point=N_PSY,lon=centroid_x,lat=centroid_y)
features <- jsonlite::fromJSON(file.path(geo_dir,'searoute_r31_representative.geojson'),simplifyVector=FALSE)$features
aggregate_coords <- bind_rows(lapply(features,function(f)tibble(
  Point=paste0('R31-',sub('^R31-','',f$properties$R31)),
  lon=as.numeric(f$properties$Longitude),lat=as.numeric(f$properties$Latitude))))
coordinates <- bind_rows(coordinates,aggregate_coords)
stopifnot(!anyDuplicated(coordinates$Point))

# FILTERED PRIMARY READS, TIME WEIGHTS AND MODEL STATUS ------------------------
readpar <- function(path,symbol) {
  z <- rgdx.param(path,symbol)
  z[] <- lapply(z,function(x)if(is.factor(x))as.character(x)else x)
  names(z)[ncol(z)] <- 'Value';stopifnot(all(is.finite(z$Value)));z
}
read_sparse <- function(path,symbol,uels,columns) {
  z <- rgdx(path,list(name=symbol,uels=uels))
  stopifnot(z$dim==length(columns))
  out <- as_tibble(setNames(lapply(seq_along(columns),function(j)z$uels[[j]][z$val[,j]]),columns))
  out$Value <- z$val[,length(columns)+1];stopifnot(all(is.finite(out$Value)));out
}
check_year <- function(path) {
  con <- file(path,'r');on.exit(close(con));model <- solver <- character()
  repeat {
    z <- readLines(con,n=2000,warn=FALSE);if(!length(z))break
    model <- c(model,grep('^\\*\\*\\*\\* MODEL STATUS',z,value=TRUE))
    solver <- c(solver,grep('^\\*\\*\\*\\* SOLVER STATUS',z,value=TRUE))
    if(length(model)&&length(solver))break
  }
  stopifnot(length(model)==1,length(solver)==1,
    grepl('STATUS\\s+1\\s+Optimal',model),grepl('STATUS\\s+1\\s+Normal',solver))
}
physical_point <- function(x)sub('-(H2-ROAD|NH3-RAIL|H2TRD|NH3TRD|SYFTRD|SYMTRD|AC2|AC|H2|NH3|SYF|SYM)$','',x)
endpoints <- function(z) {
  ends <- strsplit(z$Link,'_',fixed=TRUE);stopifnot(all(lengths(ends)==2))
  z$Bus0 <- vapply(ends,`[[`,character(1),1L);z$Bus1 <- vapply(ends,`[[`,character(1),2L)
  z$Point0 <- physical_point(z$Bus0);z$Point1 <- physical_point(z$Bus1);z
}
annual_flow <- function(path,symbol,links,wt) {
  x <- read_sparse(path,symbol,list(links,wt$T),c('Link','Time')) %>% left_join(wt,by=c('Time'='T'))
  stopifnot(!anyNA(x$Weight),all(x$Value>=-1e-8))
  x %>% mutate(Value=pmax(Value,0)) %>% group_by(Link) %>% summarise(EJ=sum(Value*Weight)*3.6e-6,.groups='drop')
}

# PRODUCTION AT THE PHYSICAL MANUFACTURING NODE, AND TRANSPORT LINKS ----------
productions <- networks <- list()
for(row in seq_len(nrow(PSM_RUNS))) {
  area <- PSM_RUNS$Region[row];token <- PSM_RUNS$Date[row]
  run <- file.path(source_root,PSM_SCENARIO,area,token)
  nodes <- mapping$N_IND[mapping$R31 %in% regions[[area]]]
  valid_nodes <- c(nodes,paste0('R31-',regions[[area]]))
  version <- readLines(file.path(run,'modelstat/systemlog/systemlog.txt'),warn=FALSE)
  stopifnot(any(grepl('8002fe88e99fce839140c97bead693b3ac0295ce',version,fixed=TRUE)))
  prods <- nets <- list()
  for(s in target_scenarios) {
    message('READ ',area,' ',s)
    input <- file.path(run,'data/input',paste0(s,'.gdx'))
    primary <- file.path(run,'gams_output/gdx_primary',s,paste0(target_year,'.gdx'))
    check_year(file.path(run,'gams_output/log',s,paste0(target_year,'.lst')))
    wt <- readpar(input,'wt') %>% rename(Weight=Value)
    stopifnot(wt$Weight[wt$T=='y00']==primary_weight,
      sum(grepl('^d[0-9]{3}$',wt$T))==365,all(wt$Weight[grepl('^d[0-9]{3}$',wt$T)]==1),
      all(wt$Weight[grepl('^d[0-9]{3}t',wt$T)]==primary_weight),
      as.numeric(rgdx(primary,list(name='h2_time_version'))$val)==4)
    ll <- rgdx.set(input,'M_LL2');active <- as.character(rgdx.set(input,'L')[[1]])
    links <- tibble(Link=as.character(ll[[1]]),Technology=as.character(ll[[2]])) %>% filter(Link %in% active)
    stopifnot(!anyDuplicated(links$Link))
    message('Hydrogen-related active conversion types: ',paste(unique(links$Technology[grepl('elec|bosch|tropsch|methan|crack',links$Technology)]),collapse=', '))
    cnv <- endpoints(filter(links,Technology %in% names(conversion_carriers))) %>% mutate(
      I0=sub('.*-','',Bus0),I1=sub('.*-','',Bus1),Carrier=unname(conversion_carriers[Technology]))
    stopifnot(all(cnv$Point0 %in% nodes))
    efficiency <- read_sparse(input,'feta',list(cnv$Link,unique(cnv$I0),unique(cnv$I1)),c('Link','I0','I1')) %>% rename(Efficiency=Value)
    cnv <- left_join(cnv,efficiency,by=c('Link','I0','I1'))
    stopifnot(!anyNA(cnv$Efficiency),all(cnv$Efficiency>0),all(cnv$Efficiency<=1))
    message('Conversion efficiencies:');print(distinct(cnv,Technology,Efficiency))
    forward <- annual_flow(primary,'vff_l',cnv$Link,wt)
    backward <- annual_flow(primary,'vfb_l',cnv$Link,wt)
    stopifnot(sum(backward$EJ)<1e-5)
    # Link origin is the detailed plant/H2-feed node. NH3/SYF destination is an
    # R31 balance pool, not an independently located manufacturing plant.
    cp <- left_join(cnv,forward,by='Link') %>% mutate(EJ=coalesce(EJ,0)*Efficiency) %>%
      transmute(Point=Point0,Carrier,EJ)
    vg <- read_sparse(primary,'vg_l',list(nodes,'H2',c('h2_coal','h2_coal_ccs','h2_smr','h2_smr_ccs'),wt$T),
      c('Point','I','Technology','Time')) %>% left_join(wt,by=c('Time'='T'))
    stopifnot(all(vg$Value>=-1e-8),!anyNA(vg$Weight),all(grepl('^d[0-9]{3}$',vg$Time)))
    gp <- vg %>% group_by(Point) %>% summarise(EJ=sum(pmax(Value,0)*Weight)*3.6e-6,.groups='drop') %>% mutate(Carrier='Hydrogen')
    prods[[s]] <- bind_rows(cp,gp) %>% group_by(Point,Carrier) %>% summarise(Value=sum(EJ),.groups='drop') %>% mutate(Scenario=s)
    transport <- endpoints(filter(links,Technology %in% names(tech_modes))) %>% filter(Point0!=Point1) %>%
      mutate(Mode=unname(tech_modes[Technology]),Start=pmin(Point0,Point1),End=pmax(Point0,Point1))
    stopifnot(all(transport$Point0 %in% valid_nodes),all(transport$Point1 %in% valid_nodes),
      !anyDuplicated(transport[c('Mode','Start','End')]))
    f <- annual_flow(primary,'vff_l',transport$Link,wt) %>% mutate(Direction='forward')
    b <- annual_flow(primary,'vfb_l',transport$Link,wt) %>% mutate(Direction='backward')
    net <- bind_rows(f,b) %>% left_join(transport,by='Link') %>% mutate(
      From=ifelse(Direction=='forward',Point0,Point1),To=ifelse(Direction=='forward',Point1,Point0),Scenario=s,
      Carrier=case_when(grepl('^H2',Mode)~'Hydrogen',grepl('^NH3',Mode)~'Ammonia',TRUE~'Synfuel'))
    # Pipelines: two-way gross; shipping: directed flows and arrows.
    nets[[s]] <- bind_rows(net %>% filter(!grepl('ship',Mode)) %>% group_by(Scenario,Carrier,Mode,Start,End) %>%
      summarise(FlowEJ=sum(EJ),.groups='drop') %>% mutate(From=Start,To=End),
      net %>% filter(grepl('ship',Mode)) %>% transmute(Scenario,Carrier,Mode,Start,End,From,To,FlowEJ=EJ))
  }
  productions[[area]] <- bind_rows(prods) %>% left_join(coordinates,by='Point') %>%
    mutate(Category=factor(Carrier,levels=names(production_colors)),Order=as.integer(Category),Region=Point)
  networks[[area]] <- bind_rows(nets) %>%
    left_join(coordinates %>% rename(From=Point,lon0=lon,lat0=lat),by='From') %>%
    left_join(coordinates %>% rename(To=Point,lon1=lon,lat1=lat),by='To')
  stopifnot(!anyNA(productions[[area]]$lon),!anyNA(networks[[area]]$lon0),!anyNA(networks[[area]]$lon1))
  message('PRODUCTION EJ/YEAR ',area);print(as.data.frame(bind_rows(prods) %>% group_by(Scenario,Carrier) %>%
    summarise(EJ=sum(Value),.groups='drop') %>% pivot_wider(names_from=Carrier,values_from=EJ)),digits=9)
  message('TRANSPORT EJ/YEAR ',area);print(as.data.frame(networks[[area]] %>% group_by(Scenario,Carrier) %>%
    summarise(EJ=sum(FlowEJ),.groups='drop') %>% pivot_wider(names_from=Carrier,values_from=EJ)),digits=9)
}

# REGIONAL MAPS WITH COMMON FOUR-SCENARIO SCALES -------------------------------
for(row in seq_len(nrow(PSM_RUNS))) {
  area <- PSM_RUNS$Region[row];token <- PSM_RUNS$Date[row]
  output <- file.path(project,'output',paste0(area,'_',token))
  bounds <- switch(area,Asia=c(60,150,-12,57),NorthAmerica=c(-179,-50,5,84))
  nodes <- mapping$N_IND[mapping$R31 %in% regions[[area]]]
  pol <- geography %>% filter(N_PSY %in% nodes)
  z <- productions[[area]];net <- networks[[area]]
  total <- z %>% group_by(Scenario,Point) %>% summarise(Total=sum(Value),.groups='drop')
  z <- left_join(z,total,by=c('Scenario','Point'))
  maximum <- max(total$Total);flow_max <- max(net$FlowEJ)
  flow_breaks <- scales::breaks_pretty(n=3)(c(0,flow_max));flow_breaks <- flow_breaks[flow_breaks>0 & flow_breaks<=flow_max]
  pipeline <- net %>% filter(!grepl('ship',Mode),FlowEJ>=flow_cutoff_ej)
  paths <- make_paths(pipeline)
  ship <- net %>% filter(grepl('ship',Mode),FlowEJ>=flow_cutoff_ej)
  displayed_modes <- names(MODE_COLORS)[names(MODE_COLORS) %in% net$Mode]
  hubs <- aggregate_coords %>% filter(Point %in% paste0('R31-',regions[[area]]))
  piesize <- ggplot(tibble(Value=c(maximum/4,maximum/2,maximum)),aes(1,Value,size=Value))+
    geom_point(shape=21,fill='white',color='grey40',stroke=.3)+
    scale_size_area(max_size=12,limits=c(0,maximum),breaks=c(maximum/4,maximum/2,maximum),
      labels=function(x)formatC(x,digits=2,format='fg'),name='Gross production (EJ/yr)')+
    guides(size=guide_legend(nrow=1,title.position='left'))+theme_void()+
    theme(legend.position='bottom',legend.direction='horizontal',legend.text=element_text(size=9))
  sizeleg <- cowplot::get_legend(gtable::gtable_filter(ggplotGrob(piesize),'^guide-box-bottom$'))
  maps <- list();legend <- NULL
  for(s in target_scenarios) {
    a <- sea_curve_rows(filter(ship,Scenario==s,Mode=='NH3 ship'))
    f <- sea_curve_rows(filter(ship,Scenario==s,Mode=='Synfuel ship'))
    p <- ggplot()+geom_sf(data=pol,fill='#F4F4F4',color='#B0B0B0',linewidth=.2)+
      geom_curve(data=a,aes(lon0,lat0,xend=lon1,yend=lat1,color=Mode,linewidth=FlowEJ),
        curvature=.12,alpha=.8,arrow=grid::arrow(length=grid::unit(1.5,'mm'),type='closed'))+
      geom_curve(data=f,aes(lon0,lat0,xend=lon1,yend=lat1,color=Mode,linewidth=FlowEJ),
        curvature=-.12,alpha=.85,arrow=grid::arrow(length=grid::unit(1.5,'mm'),type='closed'))+
      geom_path(data=filter(paths,Scenario==s),aes(lon,lat,group=PathPiece,color=Mode,linewidth=FlowEJ),alpha=.9)+
      geom_point(data=hubs,aes(lon,lat),shape=21,fill='white',color='grey25',size=1.8,stroke=.4)+
      geom_text(data=hubs,aes(lon,lat,label=sub('R31-','',Point)),size=2.2,vjust=-1,color='grey25',check_overlap=TRUE)+
      fig3_pie_layer(filter(z,Scenario==s,Total>1e-8),max_value=maximum,max_radius_mm=6)+
      scale_fill_manual(values=production_colors,limits=names(production_colors),drop=FALSE,name='Production')+
      scale_color_manual(values=MODE_COLORS,limits=displayed_modes,drop=FALSE,name='Transport')+
      scale_linewidth_continuous(limits=c(0,flow_max),range=c(.12,2.4),trans='sqrt',breaks=flow_breaks,
        name='Transport (EJ/yr)')+
      coord_sf(xlim=bounds[1:2],ylim=bounds[3:4],expand=FALSE)+
      labs(title=scenario_labels[match(s,target_scenarios)],x=NULL,y=NULL)+
      guides(color=guide_legend(order=1,nrow=1,override.aes=list(linewidth=1)),
        linewidth=guide_legend(order=2,nrow=1),fill=guide_legend(order=3,nrow=1))+
      theme_minimal(base_size=10)+theme(panel.grid=element_blank(),axis.text=element_text(size=7),
        plot.title=element_text(face='bold',size=13),legend.position='bottom',legend.box='vertical',
        legend.key.height=grid::unit(3,'mm'),legend.key.width=grid::unit(8,'mm'),
        legend.text=element_text(size=9),legend.title=element_text(size=9),plot.margin=margin(3,4,3,4,'mm'))
    if(is.null(legend))legend <- cowplot::get_legend(gtable::gtable_filter(ggplotGrob(p),'^guide-box-bottom$'))
    maps[[s]] <- p+theme(legend.position='none')
    omitted <- sum(filter(net,Scenario==s,FlowEJ<flow_cutoff_ej)$FlowEJ)
    offmap <- sum(filter(z,Scenario==s,lon<bounds[1]|lon>bounds[2]|lat<bounds[3]|lat>bounds[4])$Value)
    message(area,' ',s,' omitted links EJ=',signif(omitted,8),' off-window production EJ=',signif(offmap,8))
    header <- ggdraw()+draw_label(paste0(area,' | Hydrogen-based fuel production & transport | ',target_year),
      x=.03,hjust=0,size=15,fontface='bold')
    note <- ggdraw()+draw_label(paste0(
      'Circle area: gross carrier output; H2 feedstock and downstream NH3 / synfuel outputs are separate production stages.\n',
      'H2: electrolysis + coal / SMR; excludes imports, storage release and NH3 cracking. Synfuel: liquid + methane.\n',
      'Lines: H2 two-way gross; ships directional. R31 hollow nodes: balance-pool representative locations, not plant / port locations.\n',
      'Same map window as storage map 07. Four-case common scales. Links < ',flow_cutoff_ej,' EJ/yr hidden (',
      formatC(omitted,digits=3,format='fg'),' EJ/yr). Outside-window production: ',
      formatC(offmap,digits=3,format='fg'),' EJ/yr. Run ',token,'.'),x=.03,hjust=0,size=8)
    # Regional map / transport and production keys / physical-circle size key / definitions.
    composed <- plot_grid(header,maps[[s]],legend,sizeleg,note,ncol=1,rel_heights=c(.07,1,.17,.07,.16))
    ggsave(file.path(output,paste0('13_production_transport_',target_year,'_',s,'.png')),composed,
      width=300,height=285,units='mm',dpi=300,bg='white')
  }
  header <- ggdraw()+draw_label(paste0(area,' | Hydrogen-based fuel production & transport | ',target_year),
    x=.03,hjust=0,size=17,fontface='bold')
  note <- ggdraw()+draw_label(paste0(
    'Circle area = gross carrier output (EJ/yr), including separate H2-feedstock and downstream production stages; not final energy.\n',
    'H2: electrolysis + coal / SMR. Synfuel: liquid + methane. Imports, storage release and NH3 cracking excluded.\n',
    'H2 lines: two-way gross; shipping arrows: directed. Hollow R31 hubs: aggregate balance-pool representatives, not physical plants / ports.\n',
    'Same map window as storage map 07; common circle and line scales across all four cases. Links < ',flow_cutoff_ej,
    ' EJ/yr hidden. Run ',token,'.'),x=.03,hjust=0,size=9)
  # Header / 2 x 2 scenario map grid / shared keys / circle size key / definitions.
  comparison <- plot_grid(header,plot_grid(plotlist=maps,ncol=2,labels=c('a','b','c','d')),
    legend,sizeleg,note,ncol=1,rel_heights=c(.06,1,.12,.05,.12))
  ggsave(file.path(output,paste0('14_production_transport_scenarios_',target_year,'.png')),comparison,
    width=420,height=380,units='mm',dpi=300,bg='white')
}
