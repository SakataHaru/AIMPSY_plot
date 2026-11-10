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
FONT_SCALE <- 1.4
FLOW_THRESHOLD <- 0.001 # Reader diagnostics only; the bar chart has no cutoff.

# PACKAGES AND SHARED READERS --------------------------------------------------
suppressPackageStartupMessages({library(gdxrrw);library(dplyr);library(tidyr);library(jsonlite);library(ggplot2)})
script <- sub('^--file=','',commandArgs(FALSE)[grepl('^--file=',commandArgs(FALSE))])
CODE_DIR <- dirname(normalizePath(script[[1]],winslash='/'))
PROJECT_DIR <- normalizePath(file.path(CODE_DIR,'..'),winslash='/')
WORKSPACE <- normalizePath(file.path(PROJECT_DIR,'../../../..'),winslash='/')
PLOT_REPO <- file.path(WORKSPACE,'AIMPSY_plot')
GEO_DIR <- file.path(PLOT_REPO,'IntermReport/data')
PSM_OUTPUT_ROOT <- '//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output'
OUTPUT_DIR <- file.path(PROJECT_DIR,'output',paste(PSM_SCENARIO,PSM_REGION,PSM_DATE,
  '2020_2050_vs',REFERENCE_DATE,sep='_'))
STAGE_DIR <- file.path(PROJECT_DIR,'work','fuel_comparison_type')
if(length(target_scenarios)!=1 || anyNA(target_scenarios) || !nzchar(target_scenarios)) stop('Select one scenario.')
source(file.path(PLOT_REPO,'IntermReport/code/data_import.R'))
source(file.path(PLOT_REPO,'IntermReport/code/plot_common.R'))
source(file.path(CODE_DIR,'transport_summary.R'))
source(file.path(CODE_DIR,'transport_network_functions.R'))
MODE_COLORS <- c('H2 pipeline'='thistle3','H2 road'='#88618E','NH3 rail'='#8E5EA2',
  'NH3 road'='#C3A3CF','NH3 ship'='#553768','Synfuel ship'='mediumorchid1')
run_paths <- c(setNames(file.path(PSM_OUTPUT_ROOT,PSM_SCENARIO,PSM_REGION,PSM_DATE),timeresol),
  setNames(file.path(PSM_OUTPUT_ROOT,PSM_SCENARIO,PSM_REGION,REFERENCE_DATE),reference_timeresol))
gdxrrw::igdx('C:/GAMS/win64/26.1')
geo <- read_geodata()

# READ ONLY THE TWO 2050 PRIMARY FILES -----------------------------------------
values <- lapply(names(run_paths),function(res) {
  RUN_DIR <<- run_paths[[res]];TIME_RESOL <<- res
  TIME_WEIGHT <<- primary_time_weight_from_timeresol(res)
  log <- readLines(file.path(RUN_DIR,'modelstat/systemlog/systemlog.txt'),warn=FALSE)
  if(!any(grepl(PSM_COMMIT,log,fixed=TRUE))) stop('Unexpected producer commit.')
  annual <- read_case(target_scenarios[[1]],geo)$annual
  annual %>% filter(!is.na(Mode)) %>% link_endpoints() %>%
    filter(physical_point(Point0)!=physical_point(Point1)) %>% group_by(Mode) %>%
    summarise(Value=sum(FlowEJ),.groups='drop') %>% mutate(Resolution=res)
})
fuel <- bind_rows(values) %>% complete(Resolution=names(run_paths),Mode=names(MODE_COLORS),fill=list(Value=0)) %>%
  mutate(Carrier=case_when(grepl('^H2',Mode)~'Hydrogen',grepl('^NH3',Mode)~'Ammonia',TRUE~'Synfuel'),
    Carrier=factor(Carrier,levels=c('Hydrogen','Ammonia','Synfuel')),
    Category=factor(Mode,levels=names(MODE_COLORS)),
    Axis=factor(Resolution,levels=c(reference_timeresol,timeresol)))
if(nrow(fuel)!=12 || anyNA(fuel$Value) || any(!is.finite(fuel$Value))) stop('Invalid fuel totals.')
print(fuel %>% select(Resolution,Mode,Value),n=Inf)

# SAME CHART, COLORS AND DIMENSIONS; LARGER TITLES, AXES AND LEGEND --------------
p <- ggplot(fuel,aes(Axis,Value,fill=Category)) +
  geom_col(width=.68,color='white',linewidth=.15,position=position_stack(reverse=TRUE)) +
  scale_fill_manual(values=MODE_COLORS,drop=FALSE,name=NULL) +
  scale_y_continuous(expand=expansion(mult=c(.02,.08))) +
  labs(x=NULL,y='Gross transport (EJ/yr)',title='Hydrogen-based fuel transport',
    subtitle='Same named scenario | 2050 | 12DAY versus 6HOUR',
    caption=paste('Forward + backward flows summed over interregional links; each leg is counted. No map cutoff.',
      'H2 road flow is too small to distinguish at this scale.',
      'Synthetic-fuel transport is SYF shipping in these runs.',sep='\n')) +
  summary_theme() + facet_wrap(vars(Carrier),nrow=1,drop=FALSE) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) + fuel_comparison_text_theme(FONT_SCALE)
dir.create(STAGE_DIR,recursive=TRUE,showWarnings=FALSE)
target <- file.path(if('--stage-generated' %in% commandArgs(TRUE)) STAGE_DIR else OUTPUT_DIR,
  'fuel_transport_by_mode_2050_12DAY_vs_6HOUR.png')
ggsave(target,p,device=ragg::agg_png,width=320,height=180,units='mm',dpi=300,bg='white')
message('Saved one fuel comparison PNG with type enlarged by ',FONT_SCALE,'.')
