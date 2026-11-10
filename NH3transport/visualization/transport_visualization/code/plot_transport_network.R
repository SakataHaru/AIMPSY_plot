# PLOT SETTINGS ---------------------------------------------------------------
PSM_SCENARIO <- 'global_H2_Transport'
PSM_REGION <- 'Global'
PSM_DATE <- '2610281619'
PSM_COMMIT <- 'd73f5795dc70b8c5fad8c83ecc27c3d7823f8fd7'
YEAR <- 2050L
timeresol <- '12DAY'
FLOW_THRESHOLD <- 0.001 # EJ/y; display threshold only.
NODE_THRESHOLD <- 0.1 # EJ/y; reference Fig. 3-b node display threshold.
target_scenarios <- c(
  '500C-H2_Pipeline-NH3_Rail_Ship',
  '500C-H2_Pipeline_Truck-NH3_Rail_Truck_Ship',
  '500C-H2_Pipeline_Truck-NH3_Rail_Ship',
  '500C-H2_Pipeline-NH3_Rail_Truck_Ship'
)
scenario_labels <- c(
  '500C | H2: pipeline | NH3: rail + ship',
  '500C | H2: pipeline + truck | NH3: rail + truck + ship',
  '500C | H2: pipeline + truck | NH3: rail + ship',
  '500C | H2: pipeline | NH3: rail + truck + ship'
)
scenario_axis_labels <- c('No trucks','H2 + NH3\ntrucks','H2 trucks','NH3 trucks')

# PACKAGES --------------------------------------------------------------------
suppressPackageStartupMessages({
  library(gdxrrw)
  library(dplyr)
  library(ggplot2)
  library(jsonlite)
  library(tidyr)
  library(cowplot)
})
options(warn=1)
message('R ',getRversion(),'; packages: ',paste(vapply(
  c('gdxrrw','ggplot2','dplyr','jsonlite','ragg'),
  function(x) paste0(x,' ',packageVersion(x)),character(1)),collapse=', '))
# DERIVED SETTINGS AND INPUT PATHS ---------------------------------------------
script <- sub('^--file=', '', commandArgs(FALSE)[grepl('^--file=', commandArgs(FALSE))])
CODE_DIR <- dirname(normalizePath(script[[1]], winslash='/'))
PROJECT_DIR <- normalizePath(file.path(CODE_DIR, '..'), winslash='/')
WORKSPACE <- normalizePath(file.path(PROJECT_DIR, '../../../..'), winslash='/')
PLOT_REPO <- file.path(WORKSPACE, 'AIMPSY_plot')
PSM_OUTPUT_ROOT <- '//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output'
RUN_DIR <- file.path(PSM_OUTPUT_ROOT, PSM_SCENARIO, PSM_REGION, PSM_DATE)
OUTPUT_DIR <- file.path(PROJECT_DIR, 'output', paste(PSM_SCENARIO,PSM_REGION,PSM_DATE,YEAR,'500C',sep='_'))
REPLACE_GENERATED <- '--replace-generated' %in% commandArgs(TRUE)
if(anyNA(target_scenarios) || any(!nzchar(trimws(target_scenarios))) ||
   anyDuplicated(target_scenarios) || length(target_scenarios)!=length(scenario_labels))
  stop('Invalid target_scenarios or scenario_labels.')
SCENARIOS <- setNames(scenario_labels,target_scenarios)
# Reuse the report's palette and timeslice conversion; definitions only.
source(file.path(PLOT_REPO, 'IntermReport/code/data_import.R'))
source(file.path(PLOT_REPO, 'IntermReport/code/plot_common.R'))
source(file.path(PLOT_REPO, 'IntermReport/code/fig3/fig3_common.R'))
source(file.path(CODE_DIR, 'transport_summary.R'))
source(file.path(CODE_DIR, 'generation_template_helpers.R'))
MODE_COLORS <- c('H2 pipeline'=unname(FIG3_H2_NODE_COLORS[['Direct H2']]),
  'H2 road'=unname(FIG3_H2_NODE_COLORS[['Direct H2']]),
  'NH3 rail'=unname(FIG3_H2_NODE_COLORS[['NH3']]),
  'NH3 road'=unname(FIG3_H2_NODE_COLORS[['NH3']]),
  'NH3 ship'=unname(FIG3_H2_NODE_COLORS[['NH3']]),
  'Synfuel ship'=unname(FIG3_H2_NODE_COLORS[['Synfuel']]))
MODE_TYPES <- c('H2 pipeline'='solid','H2 road'='dotted','NH3 rail'='solid',
                'NH3 road'='dotted','NH3 ship'='solid','Synfuel ship'='solid')
TIME_RESOL <- toupper(trimws(timeresol))
TIME_WEIGHT <- primary_time_weight_from_timeresol(TIME_RESOL)
GEO_DIR <- file.path(PLOT_REPO, 'IntermReport/data')

# SHARED NETWORK READERS AND DRAWING -------------------------------------------
source(file.path(CODE_DIR, 'transport_network_functions.R'))

# GENERATE AND SAVE PLOTS ------------------------------------------------------
main <- function() {
  outputs <- unlist(lapply(target_scenarios,function(s) file.path(OUTPUT_DIR,s,
    paste0('network_',c('all','hydrogen','ammonia','synfuel'),'_',YEAR,'.png'))))
  if(any(file.exists(outputs)) && !REPLACE_GENERATED)
    stop('Output PNGs already exist; select a new OUTPUT_DIR or explicitly use --replace-generated.')
  version <- readLines(file.path(RUN_DIR,'modelstat/systemlog/systemlog.txt'),warn=FALSE)
  if(!any(grepl(PSM_COMMIT,version,fixed=TRUE))) stop('Producer commit does not match this plot configuration.')
  status <- read.csv(file.path(RUN_DIR,'modelstat/summary.csv'),header=FALSE,stringsAsFactors=FALSE)
  if(any(!target_scenarios %in% status[status$V2=='Optimal' & status$V3==YEAR,1]))
    stop('A target scenario does not have an Optimal summary for the target year.')
  gdxrrw::igdx('C:/GAMS/win64/26.1')
  geo <- read_geodata()
  cases <- lapply(target_scenarios,read_case,geo=geo)
  iamc <- read_iamc_gdx(file.path(RUN_DIR,'main'),'PSM') %>%
    filter(Scenario %in% target_scenarios,Year==YEAR)
  if(any(!target_scenarios %in% iamc$Scenario)) stop('Missing target-year IAMC scenario.')
  annual <- bind_rows(lapply(cases,`[[`,'annual'))
  node_values <- prepare_node_pies(iamc,annual,geo)
  links <- bind_rows(lapply(cases,`[[`,'gross'))
  shipping <- bind_rows(lapply(cases,`[[`,'shipping'))
  message('NH3 road links above the display threshold:')
  print(links %>% filter(Mode=='NH3 road',FlowEJ>=FLOW_THRESHOLD) %>%
    select(Scenario,Start,End,FlowEJ))
  land <- links %>% filter(!grepl('ship$',Mode))
  paths <- make_paths(land %>% filter(FlowEJ>=FLOW_THRESHOLD))
  scales <- list(flow=max(c(land$FlowEJ,shipping$FlowEJ)))
  scales$node <- max(node_values$Total)
  z <- c(.001,.01,.1,.5,1,2,5,10,20,50,100)
  scales$flow_breaks <- tail(z[z<=scales$flow & z>=scales$flow/100],4)
  message('Shared multi-mode land corridors: ',attr(paths,'shared_corridors'))
  message('Common flow scale max=',signif(scales$flow,5),' EJ/y')
  message('Directional shipping links above threshold:')
  print(shipping %>% group_by(Scenario,Mode) %>%
    summarise(Visible=sum(FlowEJ>=FLOW_THRESHOLD),.groups='drop'))
  for(scenario in target_scenarios) {
    dir.create(file.path(OUTPUT_DIR,scenario),recursive=TRUE,showWarnings=FALSE)
    for(scope in c('all','Hydrogen','Ammonia','Synfuel')) {
      p <- make_map(scenario,scope,paths,shipping,geo,scales,node_values)
      target <- file.path(OUTPUT_DIR,scenario,paste0('network_',tolower(scope),'_',YEAR,'.png'))
      ggsave(target,p,device=ragg::agg_png,width=300,height=175,units='mm',dpi=300,bg='white')
      message('Saved: ',target)
    }
  }
  save_summary_figures(iamc,annual,bind_rows(lapply(cases,`[[`,'capacity')),geo)
  message('Completed: ',length(outputs)+4L,' PNGs (16 maps + 4 themes). Source GDX unchanged; no intermediate data saved.')
  invisible(NULL)
}
main()
