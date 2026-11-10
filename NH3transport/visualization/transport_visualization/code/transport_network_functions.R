# GEOGRAPHY AND REPRESENTATIVE PORT COORDINATES --------------------------------
read_geodata <- function() {
  # Land uses R272 centroids; shipping uses the actual representative port points.
  # Read GeoJSON directly, avoiding any package installation or data conversion.
  features <- jsonlite::fromJSON(file.path(GEO_DIR,'nodes_centroid.geojson'),
                                simplifyVector=FALSE)$features
  nodes <- bind_rows(lapply(features,function(f) data.frame(
    Point=as.character(f$properties$N_PSY),
    lon=as.numeric(f$properties$centroid_x),lat=as.numeric(f$properties$centroid_y))))
  borders <- bind_rows(lapply(seq_along(features),function(i) {
    g <- features[[i]]$geometry
    polygons <- if(g$type=='Polygon') list(g$coordinates) else g$coordinates
    bind_rows(lapply(seq_along(polygons),function(j) {
      # Outer rings form the existing regional backdrop; no inferred routes.
      a <- matrix(unlist(polygons[[j]][[1]]),ncol=2,byrow=TRUE)
      piece <- cumsum(c(FALSE,abs(diff(a[,1]))>180))
      data.frame(lon=a[,1],lat=a[,2],Group=paste(i,j,piece,sep='_'))
    }))
  }))
  features <- jsonlite::fromJSON(file.path(GEO_DIR,'searoute_r31_representative.geojson'),
                                simplifyVector=FALSE)$features
  regions <- bind_rows(lapply(features,function(f) data.frame(
    Point=paste0('R31-',sub('^R31-','',f$properties$R31)),
    lon=as.numeric(f$properties$Longitude),lat=as.numeric(f$properties$Latitude))))
  # Read the mapping at the exact producer commit, rather than today's model tree.
  lines <- system2('git',c('-C',shQuote(file.path(WORKSPACE,'AIMPSY_AI')),'show',
                         paste0(PSM_COMMIT,':data/set/M_NH3_PORT.gms')),stdout=TRUE)
  lines <- lines[grepl('^R31-',lines)]
  parts <- strsplit(trimws(lines),'[[:space:]]+')
  ports <- setNames(vapply(parts,function(x)sub('^\\.','',x[[2]]),character(1)),
                   vapply(parts,`[[`,character(1),1L))
  if(length(ports)!=31L || anyNA(match(unname(ports),nodes$Point)))
    stop('Producer NH3 gateway mapping does not match the coordinate table.')
  port_nodes <- vapply(features,function(f) as.character(f$properties$N),character(1))
  if(any(port_nodes!=unname(ports[regions$Point])) || anyDuplicated(unname(ports)))
    stop('Representative port points do not match the producer gateway mapping.')
  # Preserve physical node IDs for flow aggregation; change only ship drawing coordinates.
  shipping_coordinates <- bind_rows(
    regions %>% mutate(Point=unname(ports[Point])),regions)
  message('Shipping endpoints: 31 representative port points, matched to producer gateways.')
  list(nodes=nodes,regions=regions,borders=borders,coordinates=bind_rows(nodes,regions),
       shipping_coordinates=shipping_coordinates,ports=ports)
}

# PRIMARY FLOWS AND ANNUAL ENERGY ----------------------------------------------
read_case <- function(scenario, geo) {
  path <- file.path(RUN_DIR,'gams_output/gdx_primary',scenario,paste0(YEAR,'.gdx'))
  if(!file.exists(path)) stop('Missing primary GDX: ',path)
  read_set <- function(name) {
    x <- gdxrrw::rgdx.set(path,name)
    if(is.null(x) || nrow(x)==0) character() else as.character(x[[1]])
  }
  sets <- list(h2road=read_set('L_H2ROAD'),nh3road=read_set('L_ROAD'),rail=read_set('L_RAIL'))
  mode_for <- function(link) case_when(
    link %in% sets$h2road ~ 'H2 road',link %in% sets$nh3road ~ 'NH3 road',
    link %in% sets$rail ~ 'NH3 rail',
    grepl('-(H2|H2TRD)_.*-(H2|H2TRD)$',link) ~ 'H2 pipeline',
    grepl('^R31-[A-Z0-9]+-(NH3|NH3TRD)_R31-[A-Z0-9]+-(NH3|NH3TRD)$',link) ~ 'NH3 ship',
    grepl('^R31-[A-Z0-9]+-(SYF|SYFTRD|SYM|SYMTRD)_R31-[A-Z0-9]+-(SYF|SYFTRD|SYM|SYMTRD)$',link) ~ 'Synfuel ship',
    TRUE ~ NA_character_)
  all_flows <- list()
  read_flow <- function(symbol) {
    raw <- gdxrrw::rgdx.param(path,symbol)
    out <- tibble(Link=as.character(raw$L),Time=as.character(raw$T),Value=raw[[symbol]]) %>%
      mutate(Mode=mode_for(Link))
    if(any(!is.finite(out$Value)) || any(out$Value < -1e-8)) stop('Invalid flow values.')
    days <- if(TIME_RESOL=='12DAY') c(1,32,60,91,121,152,182,213,244,274,305,335) else 1:365
    slots <- if(TIME_RESOL=='12DAY') 24L else as.integer(24/TIME_WEIGHT)
    valid_times <- c('y00',sprintf('d%03dt%02d',rep(days,each=slots),rep(0:(slots-1),length(days))))
    if(any(!out$Time %in% valid_times)) stop('Unexpected primary timeslice in transport data.')
    message(symbol, ': ',nrow(out),' records; ',length(unique(out$Time)),
            ' time labels; multiplier=',TIME_WEIGHT,' hours, including y00.')
    if(symbol=='vff_l') print(out %>% filter(!is.na(Mode)) %>% group_by(Mode) %>%
      summarise(TimeLabels=n_distinct(Time),OnlyY00=all(Time=='y00'),.groups='drop'))
    # Match producer wt(T), including the scaled annual-domain y00 records.
    annual <- out %>% group_by(Link,Mode) %>% summarise(FlowEJ=sum(Value)*TIME_WEIGHT*3.6e-6,.groups='drop') %>%
      mutate(Direction=symbol)
    all_flows[[symbol]] <<- annual %>% mutate(Scenario=scenario)
    annual %>% filter(!is.na(Mode))
  }
  links <- bind_rows(read_flow('vff_l'),read_flow('vfb_l'))
  # Apply the noise cutoff to gross link flow, preserving its two directions.
  retained <- links %>% group_by(Link,Mode) %>%
    summarise(GrossEJ=sum(FlowEJ),.groups='drop') %>% filter(GrossEJ>1e-8)
  links <- semi_join(links,retained,by=c('Link','Mode'))
  endpoints <- strsplit(links$Link,'_',fixed=TRUE)
  if(any(lengths(endpoints)!=2)) stop('Unexpected transport link label.')
  core <- function(x) sub('-(H2-ROAD|NH3-RAIL|H2TRD|NH3TRD|SYFTRD|SYMTRD|H2|NH3|SYF|SYM)$','',x)
  links$Point0 <- core(vapply(endpoints,`[[`,character(1),1L))
  links$Point1 <- core(vapply(endpoints,`[[`,character(1),2L))
  sea <- links$Mode=='NH3 ship'
  links$Point0[sea] <- unname(geo$ports[links$Point0[sea]])
  links$Point1[sea] <- unname(geo$ports[links$Point1[sea]])
  links <- links %>% filter(Point0!=Point1) %>% mutate(
    Scenario=scenario,Carrier=case_when(grepl('^H2',Mode)~'Hydrogen',
      grepl('^NH3',Mode)~'Ammonia',TRUE~'Synfuel'),
    Start=pmin(Point0,Point1),End=pmax(Point0,Point1))
  shipping <- links %>% filter(grepl('ship$',Mode)) %>% mutate(
    Origin=ifelse(Direction=='vff_l',Point0,Point1),
    Destination=ifelse(Direction=='vff_l',Point1,Point0)) %>%
    group_by(Scenario,Carrier,Mode,Origin,Destination) %>%
    summarise(FlowEJ=sum(FlowEJ),.groups='drop')
  shipping$lon0 <- geo$shipping_coordinates$lon[match(shipping$Origin,geo$shipping_coordinates$Point)]
  shipping$lat0 <- geo$shipping_coordinates$lat[match(shipping$Origin,geo$shipping_coordinates$Point)]
  shipping$lon1 <- geo$shipping_coordinates$lon[match(shipping$Destination,geo$shipping_coordinates$Point)]
  shipping$lat1 <- geo$shipping_coordinates$lat[match(shipping$Destination,geo$shipping_coordinates$Point)]
  links <- links %>%
    group_by(Scenario,Carrier,Mode,Start,End) %>% summarise(FlowEJ=sum(FlowEJ),.groups='drop')
  links$lon0 <- geo$coordinates$lon[match(links$Start,geo$coordinates$Point)]
  links$lat0 <- geo$coordinates$lat[match(links$Start,geo$coordinates$Point)]
  links$lon1 <- geo$coordinates$lon[match(links$End,geo$coordinates$Point)]
  links$lat1 <- geo$coordinates$lat[match(links$End,geo$coordinates$Point)]
  if(any(!is.finite(as.matrix(links[,c('lon0','lat0','lon1','lat1')]))))
    stop('Unmapped transport endpoint; do not silently drop links.')
  message(scenario,': annual gross transport (EJ/y, sum across network links):')
  print(links %>% group_by(Mode) %>% summarise(Links=n(),Visible=sum(FlowEJ>=FLOW_THRESHOLD),
    Total=sum(FlowEJ),Maximum=max(FlowEJ),.groups='drop'))
  if(any(!is.finite(as.matrix(shipping[,c('lon0','lat0','lon1','lat1')]))))
    stop('Unmapped directional shipping endpoint.')
  if(abs(sum(shipping$FlowEJ)-sum(links$FlowEJ[grepl('ship$',links$Mode)]))>1e-8)
    stop('Directional and gross shipping totals differ.')
  capacity <- gdxrrw::rgdx.param(path,'vfs_l')
  capacity <- tibble(Scenario=scenario,Link=as.character(capacity$L),CapacityGW=capacity$vfs_l) %>%
    filter(grepl('-AC_.*-AC$',Link))
  list(gross=links,shipping=shipping,annual=bind_rows(all_flows),capacity=capacity)
}

# STABLE LAND LANES AND DATELINE WRAPPING --------------------------------------
make_paths <- function(links) {
  # Adapted from NH3_demand/code/transport.R: canonical endpoint order,
  # stable lanes from the union of ALL scenarios, tapered endpoint offsets.
  lanes <- links %>% distinct(Start,End,Mode) %>% group_by(Start,End) %>%
    arrange(match(Mode,names(MODE_COLORS)),.by_group=TRUE) %>%
    mutate(Lane=(n()+1)/2-row_number()) %>% ungroup()
  x <- left_join(links,lanes,by=c('Start','End','Mode'))
  tt <- seq(0,1,length.out=101)
  taper <- pmin(1,tt/.15,(1-tt)/.15);taper <- taper^2*(3-2*taper)
  out <- bind_rows(lapply(seq_len(nrow(x)),function(i) {
    z <- x[i,]; x1 <- z$lon0+((z$lon1-z$lon0+180)%%360)-180
    scale <- max(cos((z$lat0+z$lat1)/2*pi/180),.25)
    dx <- (x1-z$lon0)*scale;dy <- z$lat1-z$lat0;length <- sqrt(dx^2+dy^2)
    if(length<1e-6) stop('Different endpoint IDs share an identical coordinate.')
    shift <- .08*length*tt*(1-tt)+z$Lane*.9*taper
    tibble(Scenario=z$Scenario,Carrier=z$Carrier,Mode=z$Mode,FlowEJ=z$FlowEJ,
      PathID=paste(z$Scenario,z$Start,z$End,z$Mode,sep='__'),Step=seq_along(tt),
      lon=z$lon0+(x1-z$lon0)*tt-shift*dy/length/scale,
      lat=z$lat0+(z$lat1-z$lat0)*tt+shift*dx/length)
  })) %>% mutate(lon=(lon+180)%%360-180) %>% group_by(PathID) %>%
    arrange(Step,.by_group=TRUE) %>%
    mutate(PathPiece=paste(PathID,cumsum(abs(lon-lag(lon,default=first(lon)))>180),sep='__')) %>% ungroup()
  attr(out,'shared_corridors') <- sum(table(paste(lanes$Start,lanes$End))>1)
  out
}

# Keep short dateline curves; duplicate only the wrapped piece for clipping.
sea_curve_rows <- function(links) {
  links <- links %>% mutate(lon1=lon0+((lon1-lon0+180)%%360)-180)
  copies <- links %>% filter(lon1 < -180 | lon1 > 180) %>%
    mutate(Shift=ifelse(lon1 < -180,360,-360),lon0=lon0+Shift,lon1=lon1+Shift) %>%
    select(-Shift)
  bind_rows(links,copies)
}

# FIGURE 3-B STYLE WITH ADDITIONAL TRANSPORT MODES ------------------------------
make_map <- function(scenario, scope, paths, shipping, geo, scales, node_values) {
  modes <- if(scope=='all') names(MODE_COLORS) else
    switch(scope,Hydrogen=names(MODE_COLORS)[1:2],Ammonia=names(MODE_COLORS)[3:5],Synfuel=names(MODE_COLORS)[6])
  draws <- paths %>% filter(Scenario==scenario,FlowEJ>=FLOW_THRESHOLD,Mode %in% modes)
  sea <- shipping %>% filter(Scenario==scenario,FlowEJ>=FLOW_THRESHOLD,Mode %in% modes)
  nh3 <- sea_curve_rows(sea %>% filter(Mode=='NH3 ship'))
  synfuel <- sea_curve_rows(sea %>% filter(Mode=='Synfuel ship'))
  scope_title <- switch(scope,all='Hydrogen-based fuels',Hydrogen='Hydrogen',Ammonia='Ammonia',Synfuel='Synthetic fuels')
  p <- ggplot() + geom_polygon(data=geo$borders,aes(lon,lat,group=Group),
       fill='grey97',color='grey65',linewidth=.22) +
    geom_point(data=geo$regions,aes(lon,lat),shape=21,size=2.1,stroke=.45,
       color='grey25',fill='white',show.legend=FALSE) +
    geom_curve(data=nh3,aes(x=lon0,y=lat0,xend=lon1,yend=lat1,linewidth=FlowEJ,color=Mode),
       curvature=.12,alpha=.85,lineend='round',
       arrow=grid::arrow(length=grid::unit(1.8,'mm'),type='closed'),show.legend=FALSE) +
    geom_curve(data=synfuel,aes(x=lon0,y=lat0,xend=lon1,yend=lat1,linewidth=FlowEJ,color=Mode),
       curvature=-.12,alpha=.85,lineend='round',
       arrow=grid::arrow(length=grid::unit(1.8,'mm'),type='closed'),show.legend=FALSE) +
    geom_path(data=draws,aes(lon,lat,group=PathPiece),
       color='white',linewidth=.65,alpha=.9,lineend='round',show.legend=FALSE) +
    geom_path(data=draws,aes(lon,lat,group=PathPiece,linewidth=FlowEJ,color=Mode,linetype=Mode),
       alpha=.9,lineend='round',
       show.legend=c(colour=TRUE,linetype=TRUE,linewidth=TRUE)) +
    fig3_pie_layer(node_values %>% filter(Scenario==scenario,Total>=NODE_THRESHOLD),
       max_value=scales$node,max_radius_mm=6.2) +
    scale_fill_manual(values=FIG3_H2_NODE_COLORS,name='H2 allocation',drop=FALSE,guide='none') +
    scale_color_manual(values=MODE_COLORS,limits=modes,drop=FALSE,name=NULL) +
    scale_linetype_manual(values=MODE_TYPES,limits=modes,drop=FALSE,name=NULL) +
    scale_linewidth_continuous(limits=c(0,scales$flow),range=c(.15,3),trans='sqrt',
       breaks=scales$flow_breaks,name='Transport volume (EJ/yr)') +
    coord_fixed(ratio=1.12,xlim=c(-180,180),ylim=c(-60,85),expand=FALSE) +
    annotate('label',x=-178,y=83,hjust=0,vjust=1,
      label=paste0(scope_title,' | ',YEAR,'\n',unname(SCENARIOS[[scenario]])),
      size=3.5,linewidth=0,fill=scales::alpha('white',.82),label.padding=grid::unit(1,'mm')) +
    guides(color=guide_legend(order=1,nrow=1,override.aes=list(linewidth=1.1,alpha=1)),
       linetype=guide_legend(order=1,nrow=1),
       linewidth=guide_legend(order=2,nrow=1,title.position='left')) +
    theme_void(base_size=10,base_family='sans') + theme(
      plot.background=element_rect(fill='white',color=NA),
      panel.background=element_rect(fill='white',color=NA),
      legend.position='bottom',
      legend.justification='center',legend.direction='horizontal',legend.box='vertical',
      legend.background=element_rect(fill='white',color=NA),
      legend.title=element_text(size=16),legend.text=element_text(size=16),
      legend.key.width=grid::unit(9,'mm'),legend.key.height=grid::unit(4,'mm'),
      legend.spacing.y=grid::unit(0,'mm'),
      legend.margin=margin(1,2,1,2,unit='mm'),plot.margin=margin(0,0,0,0))
  if(!nrow(draws) && !nrow(sea)) p <- p+annotate('text',x=0,y=-35,
     label='No transport above display threshold',size=4,color='grey35')
  # Geographic panel / external transport legend / external node-pie legend.
  # Preserve 135 mm for the map and reserve 40 mm below it for the legends.
  legend_source <- ggplotGrob(p)
  transport_legend <- cowplot::get_legend(
    gtable::gtable_filter(legend_source,'^guide-box-bottom$'))
  cowplot::plot_grid(p+theme(legend.position='none'),transport_legend,
    node_legend_panel(scales$node),ncol=1,rel_heights=c(135,18,22))
}

