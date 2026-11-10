# PLOT SETTINGS ---------------------------------------------------------------
PSM_SCENARIO <- 'H2_storage_compressor'
PSM_RUNS <- data.frame(Region=c('Asia','NorthAmerica'),Date=c('2610301310','2610291832'))
target_scenarios <- c('500C','500C-H2storage','500C-H2sto_only_tank','500C-H2sto_only_cavern')
scenario_axis_labels <- c('No\nstorage','Tank +\ncavern','Tank\nonly','Cavern\nonly')
timeresol <- '6HOUR'
target_year <- 2045L

# PACKAGES, PATHS AND REFERENCE STYLE -----------------------------------------
.libPaths(c('C:/Users/Haru Sakata/AppData/Local/R/win-library/4.4',.libPaths()))
suppressPackageStartupMessages({library(gdxrrw);library(dplyr);library(tidyr)
  library(ggplot2);library(cowplot)})
gdxrrw::igdx('C:/GAMS/win64/26.1')
script <- sub('^--file=','',commandArgs()[grepl('^--file=',commandArgs())])[1]
project <- dirname(dirname(normalizePath(script,winslash='/')))
workspace <- normalizePath(file.path(project,'../../../..'),winslash='/')
source_root <- '//SY_3600_01/atfs02/Individual/hsakata/AIMPSY_2nd/output'
region_arg <- sub('^--region=','',grep('^--region=',commandArgs(trailingOnly=TRUE),value=TRUE))
active_rows <- if(length(region_arg))which(PSM_RUNS$Region %in% region_arg)else seq_len(nrow(PSM_RUNS))
stopifnot(length(active_rows)>0)
source(file.path(workspace,'AIMPSY_plot/IntermReport/code/plot_common.R'))
timeresol <- toupper(trimws(timeresol))
weights <- c('12DAY'=ceiling(365/12*1000)/1000,'6HOUR'=6,'4HOUR'=4,'3HOUR'=3,'1HOUR'=1)
stopifnot(timeresol %in% names(weights),length(target_scenarios)==length(scenario_axis_labels),
  !anyDuplicated(target_scenarios),all(nzchar(target_scenarios)))
primary_weight <- unname(weights[timeresol])
mode_colors <- c('H2 pipeline'='thistle3','H2 road'='#88618E','NH3 rail'='#8E5EA2',
  'NH3 road'='#C3A3CF','NH3 ship'='#553768','Synfuel ship'='mediumorchid1')
tech_modes <- c('pipeline_h2'='H2 pipeline','truck_h2'='H2 road',
  'rail_nh3'='NH3 rail','truck_nh3'='NH3 road','ship_nh3'='NH3 ship','ship_liq_synliq'='Synfuel ship')
carriers <- c('Hydrogen','Ammonia','Synfuel')
mapping <- read.csv(file.path(workspace,'AIMPSY_plot/IntermReport/data/M_R272.csv'))
regions <- list(Asia=c('CHN','IDN','IND','JPN','KOR','MYS','THA','VNM','XEA','XSA','XSE'),
  NorthAmerica=c('USA','CAN','MEX'))

# NATIVE PRIMARY READS AND SUCCESSFUL MODEL-YEAR CHECKS ------------------------
readpar <- function(path,symbol) {
  z <- rgdx.param(path,symbol)
  z[] <- lapply(z,function(x)if(is.factor(x))as.character(x)else x)
  names(z)[ncol(z)] <- 'Value'
  stopifnot(all(is.finite(z$Value)))
  z
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
read_flow <- function(path,symbol,links,times,wt) {
  z <- rgdx(path,list(name=symbol,uels=list(links,times)))
  stopifnot(z$dim==2)
  out <- tibble(Link=z$uels[[1]][z$val[,1]],Time=z$uels[[2]][z$val[,2]],Value=z$val[,3])
  stopifnot(all(is.finite(out$Value)),all(out$Value>=-1e-8),!anyDuplicated(out[c('Link','Time')]))
  out <- out %>% left_join(wt,by=c('Time'='T'))
  stopifnot(!anyNA(out$Weight))
  out %>% mutate(Value=pmax(Value,0),EnergyGWh=Value*Weight,Direction=symbol)
}
physical_point <- function(x)sub('-(H2-ROAD|NH3-RAIL|H2TRD|NH3TRD|SYFTRD|SYMTRD|H2|NH3|SYF|SYM)$','',x)

# LINK TECHNOLOGIES, PHYSICAL CORRIDORS AND ANNUAL THROUGHPUT ------------------
results <- list();audits <- list();available <- list()
for(row in active_rows) {
  area <- PSM_RUNS$Region[row];token <- PSM_RUNS$Date[row]
  run <- file.path(source_root,PSM_SCENARIO,area,token)
  message('REGION ',area,' | run ',run,' | year ',target_year)
  version <- readLines(file.path(run,'modelstat/systemlog/systemlog.txt'),warn=FALSE)
  stopifnot(any(grepl('8002fe88e99fce839140c97bead693b3ac0295ce',version,fixed=TRUE)))
  case_totals <- list();case_audits <- list()
  valid_nodes <- c(mapping$N_IND[mapping$R31 %in% regions[[area]]],paste0('R31-',regions[[area]]))
  for(s in target_scenarios) {
    check_year(file.path(run,'gams_output/log',s,paste0(target_year,'.lst')))
    input <- file.path(run,'data/input',paste0(s,'.gdx'))
    primary <- file.path(run,'gams_output/gdx_primary',s,paste0(target_year,'.gdx'))
    wt <- readpar(input,'wt') %>% rename(Weight=Value)
    stopifnot(wt$Weight[wt$T=='y00']==primary_weight,
      all(wt$Weight[grepl('^d[0-9]{3}$',wt$T)]==1),
      sum(grepl('^d[0-9]{3}$',wt$T))==365,
      all(wt$Weight[grepl('^d[0-9]{3}t',wt$T)]==primary_weight))
    stopifnot(as.numeric(rgdx(primary,list(name='h2_time_version'))$val)==4)
    ll <- rgdx.set(input,'M_LL2')
    active_links <- as.character(rgdx.set(input,'L')[[1]])
    candidates <- tibble(Link=as.character(ll[[1]]),Technology=as.character(ll[[2]])) %>%
      filter(Link %in% active_links,Technology %in% names(tech_modes)) %>% mutate(Mode=unname(tech_modes[Technology]))
    stopifnot(nrow(candidates)>0,!anyDuplicated(candidates$Link))
    ends <- strsplit(candidates$Link,'_',fixed=TRUE)
    stopifnot(all(lengths(ends)==2))
    candidates$Point0 <- physical_point(vapply(ends,`[[`,character(1),1L))
    candidates$Point1 <- physical_point(vapply(ends,`[[`,character(1),2L))
    candidates <- candidates %>% filter(Point0!=Point1) %>%
      mutate(Start=pmin(Point0,Point1),End=pmax(Point0,Point1))
    available[[area]] <- union(available[[area]],candidates$Mode)
    if(any(!candidates$Point0 %in% valid_nodes | !candidates$Point1 %in% valid_nodes))
      print(candidates %>% filter(!Point0 %in% valid_nodes | !Point1 %in% valid_nodes) %>% head(8))
    stopifnot(all(candidates$Point0 %in% valid_nodes),all(candidates$Point1 %in% valid_nodes),
      !anyDuplicated(candidates[c('Mode','Start','End')]))
    # Only the selected physical transport links reach R; no full vff/vfb load.
    flow <- bind_rows(read_flow(primary,'vff_l',candidates$Link,wt$T,wt),
      read_flow(primary,'vfb_l',candidates$Link,wt$T,wt)) %>%
      left_join(candidates,by='Link')
    stopifnot(!anyNA(flow$Mode))
    pipeline <- flow %>% filter(Technology=='pipeline_h2')
    stopifnot(all(grepl('^d[0-9]{3}$',pipeline$Time)),all(pipeline$Weight==1))
    annual <- flow %>% group_by(Link,Mode) %>% summarise(EJ=sum(EnergyGWh)*3.6e-6,.groups='drop')
    summary <- annual %>% group_by(Mode) %>% summarise(Value=sum(EJ),.groups='drop') %>%
      complete(Mode=names(mode_colors),fill=list(Value=0)) %>% mutate(Scenario=s)
    stopifnot(abs(sum(summary$Value)-sum(flow$EnergyGWh)*3.6e-6)<1e-9)
    case_totals[[s]] <- summary
    case_audits[[s]] <- flow %>% group_by(Mode) %>% summarise(Records=n(),Links=n_distinct(Link),
      TimeLabels=n_distinct(Time),OnlyAnnual=all(Time=='y00'),OnlyDaily=all(grepl('^d[0-9]{3}$',Time)),
      .groups='drop') %>% mutate(Scenario=s)
    message('Read ',s,': ',nrow(flow),' selected direction/time records over ',nrow(candidates),' physical links.')
  }
  results[[area]] <- bind_rows(case_totals) %>% mutate(
    Carrier=case_when(grepl('^H2',Mode)~'Hydrogen',grepl('^NH3',Mode)~'Ammonia',TRUE~'Synfuel'),
    Carrier=factor(Carrier,levels=carriers),Mode=factor(Mode,levels=names(mode_colors)),
    Scenario=factor(Scenario,levels=target_scenarios))
  audits[[area]] <- bind_rows(case_audits)
  message('TRANSPORT EJ/YEAR ',area)
  print(as.data.frame(results[[area]] %>% select(Scenario,Mode,Value) %>% pivot_wider(names_from=Mode,values_from=Value)),digits=8,row.names=FALSE)
  message('TIME CONVENTION ',area);print(audits[[area]],n=Inf,width=Inf)
}

# REFERENCE-STYLE PANELS AND EXPLICIT SHARED LEGEND ----------------------------
for(row in active_rows) {
  area <- PSM_RUNS$Region[row];token <- PSM_RUNS$Date[row];z <- results[[area]]
  totals <- z %>% group_by(Scenario,Carrier) %>% summarise(Total=sum(Value),.groups='drop')
  # Same scale across carriers within a region; regional images have independent scales.
  max_total <- max(totals$Total)
  output <- file.path(project,'output',paste0(area,'_',token));dir.create(output,recursive=TRUE,showWarnings=FALSE)
  base <- ggplot(z,aes(Scenario,Value,fill=Mode))+
    geom_col(width=.68,color='white',linewidth=.15,position=position_stack(reverse=TRUE))+
    facet_wrap(vars(Carrier),nrow=1,drop=FALSE)+
    scale_x_discrete(limits=target_scenarios,labels=setNames(scenario_axis_labels,target_scenarios),drop=FALSE)+
    scale_fill_manual(values=mode_colors,drop=FALSE,name=NULL)+
    geom_text(data=totals,aes(Scenario,pmax(Total,max_total*.01)+max_total*.015,
      label=formatC(Total,format='fg',digits=3,flag='#')),inherit.aes=FALSE,size=4.5,vjust=0)+
    scale_y_continuous(limits=c(0,max_total*1.08),expand=expansion(mult=c(.02,0)))+
    labs(x=NULL,y='Gross transport (EJ/yr)')+MyTheme+
    theme(text=element_text(size=17),axis.text.x=element_text(angle=0,hjust=.5,size=14),
      axis.text.y=element_text(size=14),axis.title.y=element_text(size=16),
      strip.text=element_text(size=17,face='bold'),strip.text.x=element_text(size=17,face='bold'),legend.position='bottom',
      legend.text=element_text(size=15),legend.key.size=grid::unit(7,'mm'),
      legend.direction='horizontal',plot.margin=margin(3,5,3,5,'mm'))+
    guides(fill=guide_legend(nrow=2,byrow=TRUE))
  leg <- cowplot::get_legend(gtable::gtable_filter(ggplotGrob(base),'^guide-box-bottom$'))
  stopifnot(!is.null(leg),!inherits(leg,'zeroGrob'))
  header <- ggdraw()+draw_label('Hydrogen-based fuel transport',x=.025,y=.73,hjust=0,size=22,fontface='bold')+
    draw_label(paste0(area,' | ',target_year,' | 500C storage scenarios | ',timeresol),x=.025,y=.26,hjust=0,size=14)
  note <- ggdraw()+draw_label(paste0(
    'Forward + backward flows over physical interregional links; each leg counted. No flow cutoff.\n',
    'H2 pipeline: daily GWh (weight 1); annual NH3 / synfuel flows: y00 weight ',primary_weight,'.\n',
    'Absent modes: ',paste(setdiff(names(mode_colors),available[[area]]),collapse=', '),'.\n',
    'Storage links, same-location connectors and external-import supplies excluded. Run ',token,'.'),
    x=.025,hjust=0,size=10.5)
  # Header / three carrier facets / shared two-row legend / native-unit note.
  combined <- plot_grid(header,base+theme(legend.position='none'),leg,note,ncol=1,
    rel_heights=c(.15,1,.19,.21))
  ggsave(file.path(output,paste0('12_fuel_transport_by_mode_',target_year,'.png')),combined,
    width=340,height=190,units='mm',dpi=300,bg='white')
  message('Saved ',area,' transport comparison.')
}
