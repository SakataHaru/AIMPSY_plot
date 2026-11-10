# ANALYSIS SETTINGS -----------------------------------------------------------
PSM_SCENARIO <- 'global_H2_Transport'
PSM_REGION <- 'Global'
PSM_DATE <- '2611061840'
REFERENCE_DATE <- '2610281619'
PSM_COMMIT <- 'd73f5795dc70b8c5fad8c83ecc27c3d7823f8fd7'
YEAR <- 2050L
timeresol <- '6HOUR'
reference_timeresol <- '12DAY'
target_scenarios <- c('500C-H2_Pipeline_Truck-NH3_Rail_Truck_Ship')
FLOW_THRESHOLD <- 0.001 # EJ/year; the existing map cutoff, not 0.01.

# SHARED READERS AND INPUT PATHS ----------------------------------------------
suppressPackageStartupMessages({
  library(gdxrrw);library(dplyr);library(tidyr);library(jsonlite)
})
options(warn=1,pillar.sigfig=7)
script <- sub('^--file=','',commandArgs(FALSE)[grepl('^--file=',commandArgs(FALSE))])
CODE_DIR <- dirname(normalizePath(script[[1]],winslash='/'))
PROJECT_DIR <- normalizePath(file.path(CODE_DIR,'..'),winslash='/')
WORKSPACE <- normalizePath(file.path(PROJECT_DIR,'../../../..'),winslash='/')
GEO_DIR <- file.path(WORKSPACE,'AIMPSY_plot/IntermReport/data')
PSM_OUTPUT_ROOT <- '//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output'
if(length(target_scenarios)!=1L || anyNA(target_scenarios) || !nzchar(target_scenarios)) stop('Select one scenario.')
source(file.path(WORKSPACE,'AIMPSY_plot/IntermReport/code/data_import.R'))
source(file.path(CODE_DIR,'transport_summary.R'))
source(file.path(CODE_DIR,'transport_network_functions.R'))
run_paths <- c(setNames(file.path(PSM_OUTPUT_ROOT,PSM_SCENARIO,PSM_REGION,PSM_DATE),timeresol),
  setNames(file.path(PSM_OUTPUT_ROOT,PSM_SCENARIO,PSM_REGION,REFERENCE_DATE),reference_timeresol))
tech_modes <- c('pipeline_h2'='H2 pipeline','truck_h2'='H2 road',
  'truck_nh3'='NH3 road','rail_nh3'='NH3 rail','ship_nh3'='NH3 ship',
  'ship_liq_synliq'='Synfuel ship')
gdxrrw::igdx('C:/GAMS/win64/26.1')
geo <- read_geodata()

# CANDIDATE LINKS FROM THE PREPARED ACTIVE DOMAIN, NOT FROM POSITIVE FLOWS -------
route_tables <- list()
candidate_ids <- list()
for(res in names(run_paths)) {
  RUN_DIR <- run_paths[[res]];TIME_RESOL <- res
  TIME_WEIGHT <- primary_time_weight_from_timeresol(res)
  version <- readLines(file.path(RUN_DIR,'modelstat/systemlog/systemlog.txt'),warn=FALSE)
  if(!any(grepl(PSM_COMMIT,version,fixed=TRUE))) stop('Unexpected producer commit.')
  status <- read.csv(file.path(RUN_DIR,'main/scenario_table.csv'))
  row <- filter(status,Scenario %in% target_scenarios)
  if(nrow(row)!=1L || row$Status!='Optimal' || row$calc_end_year<YEAR) stop('Incomplete run.')
  input <- file.path(RUN_DIR,'data/input',paste0(target_scenarios,'.gdx'))
  wt <- gdxrrw::rgdx.param(input,'wt')
  if(any(abs(wt$wt-TIME_WEIGHT)>1e-10) || !'y00' %in% as.character(wt$T)) stop('Unexpected wt.')
  mapping <- gdxrrw::rgdx.set(input,'M_LL2')
  candidates <- tibble(Link=as.character(mapping[[1]]),Technology=as.character(mapping[[2]])) %>%
    filter(Technology %in% names(tech_modes)) %>% mutate(Mode=unname(tech_modes[Technology]))
  message('Reading active link domain for ',res)
  active <- gdxrrw::rgdx.set(input,'FL_LT')
  # Keep any active time domain, including annual y00. Do not infer candidates
  # from sparse output variables, capacities or a nonzero-flow cutoff.
  active_links <- unique(as.character(active[[1]]))
  rm(active,mapping);invisible(gc())
  candidates <- candidates %>% filter(Link %in% active_links) %>%
    link_endpoints() %>% mutate(Point0=physical_point(Point0),Point1=physical_point(Point1)) %>%
    filter(Point0!=Point1) %>% mutate(Start=pmin(Point0,Point1),End=pmax(Point0,Point1)) %>%
    distinct(Mode,Link,Start,End)
  if(anyDuplicated(candidates$Link) || anyDuplicated(paste(candidates$Mode,candidates$Start,candidates$End)))
    stop('More than one model link for the same undirected mode/corridor.')
  if(!setequal(unique(candidates$Mode),unname(tech_modes))) stop('Missing transport mode in input mapping.')
  candidate_ids[[res]] <- candidates %>% select(Mode,Link) %>% arrange(Mode,Link)
  message('Prepared candidates:');print(count(candidates,Mode),n=Inf)

  # The annual member is before both plotting cutoffs (1e-8 and FLOW_THRESHOLD).
  annual <- read_case(target_scenarios[[1]],geo)$annual %>% filter(!is.na(Mode)) %>%
    group_by(Mode,Link) %>% summarise(FlowEJ=sum(FlowEJ),.groups='drop')
  physical <- annual %>% link_endpoints() %>%
    filter(physical_point(Point0)!=physical_point(Point1)) %>% select(Mode,Link,FlowEJ)
  if(nrow(anti_join(physical,candidates,by=c('Mode','Link')))) stop('Flow outside prepared candidate domain.')
  z <- candidates %>% left_join(physical,by=c('Mode','Link')) %>% mutate(FlowEJ=replace_na(FlowEJ,0))
  if(any(!is.finite(z$FlowEJ)) || any(z$FlowEJ<0)) stop('Invalid route flow.')
  # Check that road/rail denominators also match the primary saved candidate sets.
  primary <- file.path(RUN_DIR,'gams_output/gdx_primary',target_scenarios,paste0(YEAR,'.gdx'))
  for(pair in list(c('H2 road','L_H2ROAD'),c('NH3 road','L_ROAD'),c('NH3 rail','L_RAIL'))) {
    a <- as.character(gdxrrw::rgdx.set(primary,pair[[2]])[[1]])
    if(!setequal(a,z$Link[z$Mode==pair[[1]]])) stop('Primary candidate-set mismatch: ',pair[[1]])
  }
  route_tables[[res]] <- select(z,Mode,Link,FlowEJ)
}
if(!identical(candidate_ids[[reference_timeresol]],candidate_ids[[timeresol]]))
  stop('Candidate networks differ between runs; use separate denominators.')

# ONE UNDIRECTED MODEL LINK = ONE EQUALLY WEIGHTED ROUTE -----------------------
routes <- full_join(rename(route_tables[[reference_timeresol]],Flow12=FlowEJ),
  rename(route_tables[[timeresol]],Flow6=FlowEJ),by=c('Mode','Link'))
if(anyNA(routes)) stop('Missing candidate route after join.')
summarize_routes <- function(threshold) {
  z <- routes %>% mutate(Active12=if(threshold==0) Flow12>0 else Flow12>=threshold,
    Active6=if(threshold==0) Flow6>0 else Flow6>=threshold)
  z %>% group_by(Mode) %>% summarise(
    Candidates=n(),Used12=sum(Active12),Used6=sum(Active6),Common=sum(Active12 & Active6),
    Missing6=sum(Active6 & !Active12),RetainedUnused6=sum(Active12 & !Active6),
    Usage12Percent=100*Used12/Candidates,Usage6Percent=100*Used6/Candidates,
    CoveragePercent=ifelse(Used6>0,100*Common/Used6,NA_real_),
    ExcludablePercent=100*(Candidates-Used12)/Candidates,.groups='drop') %>%
    mutate(Mode=factor(Mode,levels=unname(tech_modes))) %>% arrange(Mode)
}
for(threshold in c(0,FLOW_THRESHOLD)) {
  result <- summarize_routes(threshold)
  if(any(result$Common+result$Missing6!=result$Used6) ||
     any(result$Common+result$RetainedUnused6!=result$Used12)) stop('Route accounting failed.')
  message('\nTHRESHOLD: ',if(threshold==0) 'strictly positive, with no epsilon/noise cutoff' else paste0('>= ',threshold,' EJ/year'))
  print(result,n=Inf,width=Inf)
}
# 6HOUR VOLUME CARRIED BY THE ROUTES RETAINED USING THE 12DAY SCREEN ------------
# Retained links may carry more in 6HOUR than in 12DAY. Therefore do not cap
# their contribution at min(Flow12, Flow6), unlike the earlier overlap metric.
screened <- routes %>% mutate(Eligible12=Flow12>=FLOW_THRESHOLD,
  Important6=Flow6>=FLOW_THRESHOLD)
volume_coverage <- screened %>% group_by(Mode) %>% summarise(
  KeptRoutes=sum(Eligible12),Total6Flow=sum(Flow6),
  Kept6Flow=sum(Flow6[Eligible12]),Removed6Flow=sum(Flow6[!Eligible12]),
  Kept6Percent=100*Kept6Flow/Total6Flow,
  MissingImportantCount=sum(!Eligible12 & Important6),
  MissingImportant6Flow=sum(Flow6[!Eligible12 & Important6]),
  MissingSmall6Flow=sum(Flow6[!Eligible12 & !Important6]),
  MaxRemoved6Flow=max(c(0,Flow6[!Eligible12])),
  MedianImportantMissing6Flow=if(any(!Eligible12 & Important6))
    median(Flow6[!Eligible12 & Important6]) else NA_real_,.groups='drop') %>%
  mutate(Mode=factor(Mode,levels=unname(tech_modes))) %>% arrange(Mode)
if(any(abs(volume_coverage$Kept6Flow+volume_coverage$Removed6Flow-volume_coverage$Total6Flow)>1e-9) ||
   any(abs(volume_coverage$MissingImportant6Flow+volume_coverage$MissingSmall6Flow-
     volume_coverage$Removed6Flow)>1e-9)) stop('Screened volume accounting failed.')
message('\n6HOUR VOLUME ON LINKS RETAINED BY 12DAY >= ',FLOW_THRESHOLD,' EJ/year:')
print(volume_coverage,n=Inf,width=Inf)
message('\nLARGEST 6HOUR FLOWS ON EXCLUDED LINKS, BY MODE:')
print(screened %>% filter(!Eligible12,Important6) %>% group_by(Mode) %>%
  slice_max(Flow6,n=5,with_ties=FALSE) %>% ungroup() %>%
  select(Mode,Link,Flow12,Flow6),n=Inf,width=Inf)
# LINKS ABOVE THE CUTOFF IN 12DAY BUT BELOW IT IN 6HOUR -------------------------
only12 <- screened %>% group_by(Mode) %>% summarise(
  Total12Flow=sum(Flow12),Only12Count=sum(Eligible12 & !Important6),
  Only12Flow=sum(Flow12[Eligible12 & !Important6]),
  Only12Percent=100*Only12Flow/Total12Flow,
  Common12Flow=sum(Flow12[Eligible12 & Important6]),
  BelowCutoff12Flow=sum(Flow12[!Eligible12]),.groups='drop') %>%
  mutate(Mode=factor(Mode,levels=unname(tech_modes))) %>% arrange(Mode)
if(any(abs(only12$Only12Flow+only12$Common12Flow+only12$BelowCutoff12Flow-
  only12$Total12Flow)>1e-9)) stop('12DAY-only flow partition failed.')
counts <- summarize_routes(FLOW_THRESHOLD)
if(any(only12$Only12Count!=counts$RetainedUnused6)) stop('12DAY-only count mismatch.')
message('\nLINKS USED ONLY BY 12DAY UNDER THE SHARED CUTOFF:')
print(only12,n=Inf,width=Inf)
expanded <- counts %>% select(Mode,Common,Used6,CoveragePercent) %>%
  left_join(volume_coverage %>% select(Mode,Kept6Percent,Removed6Flow),by='Mode') %>%
  left_join(only12 %>% select(Mode,Only12Count,Only12Flow,Only12Percent),by='Mode')
message('\nEXPANDED COVERAGE TABLE:')
print(expanded,n=Inf,width=Inf)
message('No data files or plots written; existing outputs and model unchanged.')
