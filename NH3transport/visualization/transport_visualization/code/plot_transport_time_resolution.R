# PLOT SETTINGS ---------------------------------------------------------------
PSM_SCENARIO <- 'global_H2_Transport'
PSM_REGION <- 'Global'
PSM_DATE <- '2611061840'
REFERENCE_DATE <- '2610281619'
PSM_COMMIT <- 'd73f5795dc70b8c5fad8c83ecc27c3d7823f8fd7'
timeresol <- '6HOUR'
reference_timeresol <- '12DAY'
target_scenarios <- c('500C-H2_Pipeline_Truck-NH3_Rail_Truck_Ship')
target_years <- seq(2020L,2050L,5L)
map_years <- c(2020L,2030L,2040L,2050L)
comparison_year <- 2050L
FLOW_THRESHOLD <- 0.001 # EJ/year, maps only; summaries retain all positive flows.
NODE_THRESHOLD <- 0.1 # EJ/year, same reference hydrogen-allocation pie definition.

# PACKAGES AND SHARED FIGURE DEFINITIONS ---------------------------------------
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
OUTPUT_DIR <- file.path(PROJECT_DIR,'output',paste(PSM_SCENARIO,PSM_REGION,PSM_DATE,
  '2020_2050_vs',REFERENCE_DATE,sep='_'))
# Optional staging keeps displayed PNGs intact until the operating system can
# atomically replace them; normal execution writes directly to OUTPUT_DIR.
MAP_OUTPUT_DIR <- if('--stage-network-maps' %in% commandArgs(TRUE))
  file.path(PROJECT_DIR,'work','legend_staging') else OUTPUT_DIR
REPLACE_GENERATED <- '--replace-generated' %in% commandArgs(TRUE)
if(length(target_scenarios)!=1L || anyNA(target_scenarios) ||
   any(!nzchar(trimws(target_scenarios)))) stop('Select one nonblank target scenario.')
if(any(!map_years %in% target_years)) stop('Map years must belong to target_years.')
source(file.path(PLOT_REPO,'IntermReport/code/data_import.R'))
source(file.path(PLOT_REPO,'IntermReport/code/plot_common.R'))
source(file.path(PLOT_REPO,'IntermReport/code/fig3/fig3_common.R'))
source(file.path(CODE_DIR,'transport_summary.R'))
source(file.path(CODE_DIR,'generation_template_helpers.R'))
source(file.path(CODE_DIR,'transport_network_functions.R'))
MODE_COLORS <- c('H2 pipeline'='thistle3','H2 road'='thistle3','NH3 rail'='#8E5EA2',
  'NH3 road'='#8E5EA2','NH3 ship'='#8E5EA2','Synfuel ship'='mediumorchid1')
MODE_TYPES <- c('H2 pipeline'='solid','H2 road'='dotted','NH3 rail'='solid',
  'NH3 road'='dotted','NH3 ship'='solid','Synfuel ship'='solid')
FUEL_COLORS <- c('H2 pipeline'='thistle3','H2 road'='#88618E','NH3 rail'='#8E5EA2',
  'NH3 road'='#C3A3CF','NH3 ship'='#553768','Synfuel ship'='mediumorchid1')
GRID_COLORS <- c('Within R31'='lightsteelblue','Across R31'='#4C78A8')
BIO_COLORS <- c('Solid biomass'='#359EAA','Liquid biomass'='#006D77')

# RUN SETTINGS, INPUT VALIDATION AND PROVENANCE --------------------------------
run_paths <- c(setNames(file.path(PSM_OUTPUT_ROOT,PSM_SCENARIO,PSM_REGION,PSM_DATE),timeresol),
  setNames(file.path(PSM_OUTPUT_ROOT,PSM_SCENARIO,PSM_REGION,REFERENCE_DATE),reference_timeresol))
cases <- bind_rows(tibble(Resolution=timeresol,Year=target_years),
  tibble(Resolution=reference_timeresol,Year=comparison_year)) %>%
  mutate(CaseID=paste(Resolution,Year,sep='_'),Run=unname(run_paths[Resolution]))
if(anyDuplicated(cases$CaseID)) stop('Duplicate run/year keys.')
if(dir.exists(OUTPUT_DIR) && length(list.files(OUTPUT_DIR,pattern='\\.png$',recursive=TRUE)) &&
   !REPLACE_GENERATED) stop('Output exists: use --replace-generated explicitly to replace these figures.')
gdxrrw::igdx('C:/GAMS/win64/26.1')
message('R ',getRversion(),'; ggplot2 ',packageVersion('ggplot2'),'; gdxrrw ',packageVersion('gdxrrw'))
for(res in names(run_paths)) {
  run <- run_paths[[res]]
  version <- readLines(file.path(run,'modelstat/systemlog/systemlog.txt'),warn=FALSE)
  if(!any(grepl(PSM_COMMIT,version,fixed=TRUE))) stop('Unexpected producer commit: ',run)
  status <- read.csv(file.path(run,'main/scenario_table.csv'))
  row <- status[status$Scenario %in% target_scenarios,]
  if(nrow(row)!=1 || row$Status!='Optimal' || row$calc_end_year<max(target_years))
    stop('Target scenario has not completed optimally: ',run)
  # Read only the small wt symbol from the original input; never copy the GDX.
  wt <- gdxrrw::rgdx.param(file.path(run,'data/input',paste0(target_scenarios,'.gdx')),'wt')
  weight <- primary_time_weight_from_timeresol(res)
  if(any(abs(wt$wt-weight)>1e-10) || !'y00' %in% as.character(wt$T)) stop('Unexpected saved wt.')
  expected <- if(res=='12DAY') 288L else 365L*24L/as.integer(weight)
  if(nrow(wt)!=expected+1L) stop('Unexpected time-domain size.')
  message(res,': saved wt=',weight,'; chronological/representative labels=',expected,
    '; expanded hours=',expected*weight,'; y00 uses same weight.')
  print(file.info(file.path(run,'main/merged_output.gdx'))[,c('size','mtime')])
  print(row)
}
config <- lapply(run_paths,function(run) readLines(file.path(run,'data/scenario',
  paste0(target_scenarios,'.txt')),warn=FALSE))
if(!identical(config[[1]],config[[2]])) stop('Saved scenario switches differ; inspect before comparing.')
message('Saved scenario switch files match exactly. Producer dirty state and unsaved runtime overrides remain unknown.')
iamcs <- lapply(run_paths,function(run) read_iamc_gdx(file.path(run,'main'),'PSM') %>%
  filter(Scenario %in% target_scenarios,Year %in% target_years))
for(res in names(iamcs)) if(any(!target_years %in% iamcs[[res]]$Year)) stop('Missing IAMC year: ',res)

# PRIMARY ANNUALIZATION AND REFERENCE NODE PIES --------------------------------
geo <- read_geodata()
case_data <- vector('list',nrow(cases))
node_data <- vector('list',nrow(cases))
for(i in seq_len(nrow(cases))) {
  z <- cases[i,]
  RUN_DIR <- z$Run; YEAR <- z$Year; TIME_RESOL <- z$Resolution
  TIME_WEIGHT <- primary_time_weight_from_timeresol(TIME_RESOL)
  message('\nREADING ',z$CaseID)
  case_data[[i]] <- read_case(target_scenarios[[1]],geo)
  node_data[[i]] <- prepare_node_pies(iamcs[[TIME_RESOL]] %>% filter(Year==YEAR),
    case_data[[i]]$annual,geo) %>% mutate(Scenario=z$CaseID)
  case_data[[i]] <- lapply(case_data[[i]],function(d) mutate(d,
    Scenario=z$CaseID,Resolution=z$Resolution,Year=z$Year))
  invisible(gc())
}
annual <- bind_rows(lapply(case_data,`[[`,'annual'))
capacity <- bind_rows(lapply(case_data,`[[`,'capacity'))
links <- bind_rows(lapply(case_data,`[[`,'gross'))
shipping <- bind_rows(lapply(case_data,`[[`,'shipping'))
node_values <- bind_rows(node_data)
iamc <- bind_rows(lapply(names(iamcs),function(res) iamcs[[res]] %>%
  mutate(Resolution=res,Scenario=paste(res,Year,sep='_')))) %>%
  semi_join(cases %>% select(Scenario=CaseID),by='Scenario')

# WORLD GENERATION, TRANSPORT AND GRID METRICS ---------------------------------
gen_spec <- tribble(~Variable,~Category,~Color,~Sign,
  'Sec_Ene_Ele_Oil','Oil','sandybrown',1,
  'Sec_Ene_Ele_Coa','Coal','grey60',1,
  'Sec_Ene_Ele_Gas','Gas','lightgoldenrod',1,
  'Sec_Ene_Ele_Gas_Hyd_Cof','Gas','lightgoldenrod',1,
  'Sec_Ene_Ele_Nuc','Nuclear','moccasin',1,
  'Sec_Ene_Ele_Hyp','Hydro','lightsteelblue',1,
  'Sec_Ene_Ele_Bio','Biomass','#A9D65D',1,
  'Sec_Ene_Ele_Geo','Geothermal','peru',1,
  'Sec_Ene_Ele_Solar','Solar','lightsalmon',1,
  'Sec_Ene_Ele_Win','Wind','lightskyblue3',1,
  'Sec_Ene_Ele_Hyd','Hydrogen','thistle2',1,
  'Sec_Ene_Ele_Cur','Curtailment','deeppink3',-1,
  'Sec_Ene_Ele_Sto_Los','Storage loss','springgreen2',-1,
  'Sec_Ene_Ele_Tra_Los','Transmission loss','seashell3',-1)
GEN_COLORS <- setNames(gen_spec$Color,gen_spec$Category)
GEN_COLORS <- GEN_COLORS[!duplicated(names(GEN_COLORS))]
world <- iamc %>% filter(Region %in% PSM_WORLD_REGIONS)
gen <- world %>% inner_join(gen_spec,by='Variable') %>%
  group_by(Scenario,Resolution,Year,Category) %>% summarise(Value=sum(Value*Sign),.groups='drop')
audit <- gen %>% filter(!Category %in% c('Curtailment','Storage loss','Transmission loss')) %>%
  group_by(Scenario) %>% summarise(Components=sum(Value),.groups='drop') %>%
  left_join(world %>% filter(Variable=='Sec_Ene_Ele') %>% group_by(Scenario) %>%
    summarise(Total=sum(Value),.groups='drop'),by='Scenario')
if(nrow(audit)!=nrow(cases) || anyNA(audit$Total) || any(abs(audit$Components-audit$Total)>1e-5))
  stop('Generation components do not reconcile with IAMC total.')
message('Generation sum audit:'); print(audit,n=Inf)
fuel <- annual %>% filter(!is.na(Mode)) %>% link_endpoints() %>%
  filter(physical_point(Point0)!=physical_point(Point1)) %>%
  group_by(Scenario,Mode) %>% summarise(Value=sum(FlowEJ),.groups='drop') %>%
  complete(Scenario=cases$CaseID,Mode=names(MODE_COLORS),fill=list(Value=0)) %>%
  left_join(cases %>% select(Scenario=CaseID,Resolution,Year),by='Scenario') %>%
  mutate(Carrier=case_when(grepl('^H2',Mode)~'Hydrogen',grepl('^NH3',Mode)~'Ammonia',TRUE~'Synfuel'),
    Carrier=factor(Carrier,levels=c('Hydrogen','Ammonia','Synfuel')),Category=Mode)
biomass <- annual %>% mutate(Category=case_when(
  grepl('^R31-[A-Z0-9]+-(CRN|CRNTRD)_R31-[A-Z0-9]+-(CRN|CRNTRD)$',Link)~'Solid biomass',
  grepl('^R31-[A-Z0-9]+-(BMS|BMSTRD)_R31-[A-Z0-9]+-(BMS|BMSTRD)$',Link)~'Liquid biomass',TRUE~NA_character_)) %>%
  filter(!is.na(Category)) %>% group_by(Scenario,Category) %>% summarise(Value=sum(FlowEJ),.groups='drop') %>%
  complete(Scenario=cases$CaseID,Category=names(BIO_COLORS),fill=list(Value=0)) %>%
  left_join(cases %>% select(Scenario=CaseID,Resolution,Year),by='Scenario')
mapping <- read.csv(file.path(GEO_DIR,'M_R272.csv'),stringsAsFactors=FALSE)
categorize_grid <- function(data) {
  z <- link_endpoints(data) %>% mutate(Node0=physical_point(Point0),Node1=physical_point(Point1)) %>%
    filter(Node0!=Node1)
  a <- mapping$R31[match(z$Node0,mapping$N_IND)]
  b <- mapping$R31[match(z$Node1,mapping$N_IND)]
  if(anyNA(a)||anyNA(b)) stop('Unmapped grid endpoint.')
  z %>% mutate(Category=ifelse(a==b,'Within R31','Across R31'),
    Pair=paste(pmin(Node0,Node1),pmax(Node0,Node1),sep='__'))
}
cap <- categorize_grid(capacity)
check <- cap %>% group_by(Scenario,Pair) %>% summarise(Delta=max(CapacityGW)-min(CapacityGW),.groups='drop')
if(any(check$Delta>1e-8)) stop('Conflicting capacity records for one physical corridor.')
cap <- cap %>% group_by(Scenario,Resolution,Year,Category,Pair) %>%
  summarise(Value=max(CapacityGW)/1000,.groups='drop') %>%
  group_by(Scenario,Resolution,Year,Category) %>% summarise(Value=sum(Value),.groups='drop')
grid_flow <- annual %>% filter(grepl('-AC_.*-AC$',Link)) %>% categorize_grid() %>%
  group_by(Scenario,Resolution,Year,Category) %>% summarise(Value=sum(FlowEJ),.groups='drop')

# METRIC COMPARISONS AND CORRIDOR CHANGES --------------------------------------
compare <- function(data,name,unit) {
  z <- data %>% filter(Year==comparison_year) %>% group_by(Resolution,Category) %>%
    summarise(Value=sum(Value),.groups='drop') %>%
    pivot_wider(names_from=Resolution,values_from=Value,values_fill=0)
  z$Difference <- z[[timeresol]]-z[[reference_timeresol]]
  z$Percent <- ifelse(abs(z[[reference_timeresol]])>1e-8,100*z$Difference/z[[reference_timeresol]],NA_real_)
  message('\n',name,' (',unit,'):'); print(z,n=Inf,width=Inf)
  invisible(z)
}
compare(fuel,'Transport by mode','EJ/year')
compare(gen,'Generation / losses','EJ/year')
compare(biomass,'Biomass transport','EJ/year')
compare(cap,'Grid capacity','TW')
compare(grid_flow,'Grid volume','EJ/year')
message('\n6HOUR transport time series:'); print(fuel %>% filter(Resolution==timeresol) %>%
  select(Year,Category,Value),n=Inf)
message('\n6HOUR generation, biomass and grid time series:')
print(gen %>% filter(Resolution==timeresol) %>% pivot_wider(names_from=Category,values_from=Value),width=Inf)
print(biomass %>% filter(Resolution==timeresol),n=Inf)
print(cap %>% filter(Resolution==timeresol),n=Inf);print(grid_flow %>% filter(Resolution==timeresol),n=Inf)
corridors <- links %>% filter(Year==comparison_year) %>% select(Resolution,Mode,Start,End,FlowEJ) %>%
  pivot_wider(names_from=Resolution,values_from=FlowEJ,values_fill=0)
corridors$Difference <- corridors[[timeresol]]-corridors[[reference_timeresol]]
message('\nLargest absolute corridor changes (gross EJ/year):')
print(corridors %>% arrange(desc(abs(Difference))) %>% head(15),width=Inf)

# FIGURE THEMES, SHARED LEGENDS AND BAR PANELS ---------------------------------
bar_panel <- function(data,colors,y,title,kind,subtitle=NULL) {
  z <- if(kind=='trend') filter(data,Resolution==timeresol) else filter(data,Year==comparison_year)
  z <- z %>% mutate(Category=factor(Category,levels=names(colors)),
    Axis=factor(if(kind=='trend') as.character(Year) else Resolution,
      levels=if(kind=='trend') as.character(target_years) else c(reference_timeresol,timeresol)))
  ggplot(z,aes(Axis,Value,fill=Category)) +
    geom_col(width=.68,color='white',linewidth=.15,position=position_stack(reverse=TRUE)) +
    scale_fill_manual(values=colors,drop=FALSE,name=NULL) +
    scale_y_continuous(expand=expansion(mult=c(.02,.08))) +
    labs(x=NULL,y=y,title=title,subtitle=subtitle) + summary_theme() +
    theme(axis.text.x=element_text(size=10))
}
combine_grid <- function(p_cap,p_flow,title) {
  legend_plot <- p_cap+theme(legend.position='bottom',legend.direction='horizontal',
    legend.key.width=grid::unit(9,'mm'),legend.spacing.x=grid::unit(3,'mm'))
  grob <- ggplotGrob(legend_plot)
  legend <- cowplot::get_legend(gtable::gtable_filter(grob,'^guide-box-bottom$'))
  # Title / [capacity | annual volume] / shared legend / definitions.
  panels <- cowplot::plot_grid(p_cap+theme(legend.position='none'),
    p_flow+theme(legend.position='none'),nrow=1,labels=c('a','b'),align='h')
  header <- ggdraw()+draw_label(title,x=.02,hjust=0,size=15,fontface='bold')
  note <- ggdraw()+draw_label('Physical AC-ended corridors; capacity counted once per corridor.\nVolume is gross forward + backward flow, summed over links.',x=.02,hjust=0,size=9)
  cowplot::plot_grid(header,panels,legend,note,ncol=1,rel_heights=c(.09,1,.1,.14))
}

# GENERATE EVOLUTION AND 2050 SUMMARY FIGURES ----------------------------------
dir.create(OUTPUT_DIR,recursive=TRUE,showWarnings=FALSE)
for(kind in c('trend','comparison')) {
  suffix <- if(kind=='trend') '2020_2050_6HOUR' else '2050_12DAY_vs_6HOUR'
  subtitle <- if(kind=='trend') '500C | H2 pipeline + truck; NH3 rail + truck + ship | 6HOUR' else
    'Same named scenario | 2050 | 12DAY versus 6HOUR'
  p_gen <- bar_panel(gen,GEN_COLORS,'Power generation (EJ/yr)','Power generation',kind,subtitle) +
    geom_hline(yintercept=0,color='grey40',linewidth=.3) +
    guides(fill=guide_legend(nrow=3,byrow=TRUE)) +
    labs(caption='Negative bars: curtailment and storage / transmission losses. Annual IAMC data are not reweighted.')
  p_gen <- apply_generation_template(p_gen,iamc)
  p_fuel <- bar_panel(fuel,FUEL_COLORS,'Gross transport (EJ/yr)','Hydrogen-based fuel transport',kind,subtitle) +
    facet_wrap(vars(Carrier),nrow=1,drop=FALSE) + guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
    labs(caption='Forward + backward flows summed over interregional links; each leg is counted. No map cutoff.\nH2 road flow is too small to distinguish at this scale. Synthetic-fuel transport is SYF shipping in these runs.')
  if(kind=='comparison') p_fuel <- p_fuel+fuel_comparison_text_theme() +
    labs(caption=paste('Forward + backward flows summed over interregional links; each leg is counted. No map cutoff.',
      'H2 road flow is too small to distinguish at this scale.',
      'Synthetic-fuel transport is SYF shipping in these runs.',sep='\n'))
  p_bio <- bar_panel(biomass,BIO_COLORS,'Gross transport (EJ/yr)','Biomass transport',kind,subtitle) +
    labs(caption='Solid / liquid biomass on R31-to-R31 shipping links; external imports excluded.')
  p_cap <- bar_panel(cap,GRID_COLORS,'Installed capacity (TW)','Transmission capacity',kind)
  p_flow <- bar_panel(grid_flow,GRID_COLORS,'Gross transmission (EJ/yr)','Transmission volume',kind)
  p_grid <- combine_grid(p_cap,p_flow,paste('Electricity transmission |',
    if(kind=='trend') '2020-2050 | 6HOUR' else '2050 | 12DAY vs 6HOUR'))
  ggsave(file.path(OUTPUT_DIR,paste0('generation_mix_',suffix,'.png')),p_gen,
    device=ragg::agg_png,width=300,height=175,units='mm',dpi=300,bg='white')
  ggsave(file.path(OUTPUT_DIR,paste0('fuel_transport_by_mode_',suffix,'.png')),p_fuel,
    device=ragg::agg_png,width=320,height=180,units='mm',dpi=300,bg='white')
  ggsave(file.path(OUTPUT_DIR,paste0('biomass_transport_',suffix,'.png')),p_bio,
    device=ragg::agg_png,width=300,height=160,units='mm',dpi=300,bg='white')
  ggsave(file.path(OUTPUT_DIR,paste0('transmission_capacity_and_flow_',suffix,'.png')),p_grid,
    device=ragg::agg_png,width=320,height=170,units='mm',dpi=300,bg='white')
}

# MAPS WITH SHARED FLOW / PIE SCALES ACROSS YEARS AND RESOLUTIONS ----------------
map_keys <- cases %>% filter((Resolution==timeresol & Year %in% map_years) |
  (Resolution==reference_timeresol & Year==comparison_year)) %>% pull(CaseID)
land <- links %>% filter(Scenario %in% map_keys,!grepl('ship$',Mode))
paths <- make_paths(land %>% filter(FlowEJ>=FLOW_THRESHOLD))
sea <- shipping %>% filter(Scenario %in% map_keys)
scales <- list(flow=max(c(land$FlowEJ,sea$FlowEJ)),node=max(node_values$Total[node_values$Scenario %in% map_keys]))
z <- c(.001,.01,.1,.5,1,2,5,10,20,50,100)
scales$flow_breaks <- tail(z[z<=scales$flow & z>=scales$flow/100],4)
SCENARIOS <- setNames(paste0('500C | ',cases$Resolution),cases$CaseID)
message('Map common scales: flow=',scales$flow,' EJ/yr; pie=',scales$node,' EJ/yr.')
map_plots <- list()
for(key in map_keys) {
  YEAR <- cases$Year[match(key,cases$CaseID)]
  dir.create(file.path(MAP_OUTPUT_DIR,key),recursive=TRUE,showWarnings=FALSE)
  map_plots[[key]] <- list()
  for(scope in c('all','Hydrogen','Ammonia','Synfuel')) {
    p <- make_map(key,scope,paths,sea,geo,scales,node_values)
    map_plots[[key]][[scope]] <- p
    ggsave(file.path(MAP_OUTPUT_DIR,key,paste0('network_',tolower(scope),'_',YEAR,'.png')),p,
      device=ragg::agg_png,width=300,height=175,units='mm',dpi=300,bg='white')
  }
}
for(scope in c('all','Hydrogen','Ammonia','Synfuel')) {
  # Evolution: [2020 | 2030] / [2040 | 2050], all common-size map panels.
  panels <- lapply(paste(timeresol,map_years,sep='_'),function(key) map_plots[[key]][[scope]])
  evolution <- cowplot::plot_grid(plotlist=panels,ncol=2)
  ggsave(file.path(OUTPUT_DIR,paste0('network_',tolower(scope),'_evolution_2020_2050.png')),evolution,
    device=ragg::agg_png,width=600,height=350,units='mm',dpi=300,bg='white')
  # Comparison: [12DAY 2050] / [6HOUR 2050], identical flow and pie scales.
  comparison <- cowplot::plot_grid(map_plots[[paste(reference_timeresol,comparison_year,sep='_')]][[scope]],
    map_plots[[paste(timeresol,comparison_year,sep='_')]][[scope]],ncol=1)
  ggsave(file.path(OUTPUT_DIR,paste0('network_',tolower(scope),'_2050_12DAY_vs_6HOUR.png')),comparison,
    device=ragg::agg_png,width=300,height=350,units='mm',dpi=300,bg='white')
}
message('Completed: 36 PNGs (20 individual maps, 8 map panels, 8 thematic figures). No Baseline; no GDX copies or intermediate data outputs.')
