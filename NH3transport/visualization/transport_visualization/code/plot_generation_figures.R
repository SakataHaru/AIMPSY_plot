# PLOT SETTINGS ---------------------------------------------------------------
PSM_SCENARIO <- 'global_H2_Transport'
PSM_REGION <- 'Global'
PSM_DATE <- '2611061840'
REFERENCE_DATE <- '2610281619'
PSM_COMMIT <- 'd73f5795dc70b8c5fad8c83ecc27c3d7823f8fd7'
target_years <- seq(2020L,2050L,5L)
YEAR <- 2050L
target_scenarios <- c('500C-H2_Pipeline-NH3_Rail_Ship',
  '500C-H2_Pipeline_Truck-NH3_Rail_Truck_Ship',
  '500C-H2_Pipeline_Truck-NH3_Rail_Ship','500C-H2_Pipeline-NH3_Rail_Truck_Ship')
single_scenario <- '500C-H2_Pipeline_Truck-NH3_Rail_Truck_Ship'
scenario_axis_labels <- c('No trucks','H2 + NH3\ntrucks','H2 trucks','NH3 trucks')

# PACKAGES, SHARED TEMPLATE AND RUN PATHS --------------------------------------
suppressPackageStartupMessages({library(gdxrrw);library(dplyr);library(tidyr);library(ggplot2);library(cowplot)})
script <- sub('^--file=','',commandArgs(FALSE)[grepl('^--file=',commandArgs(FALSE))])
CODE_DIR <- dirname(normalizePath(script[[1]],winslash='/'))
PROJECT_DIR <- normalizePath(file.path(CODE_DIR,'..'),winslash='/')
WORKSPACE <- normalizePath(file.path(PROJECT_DIR,'../../../..'),winslash='/')
PLOT_REPO <- file.path(WORKSPACE,'AIMPSY_plot')
PSM_OUTPUT_ROOT <- '//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output'
run_new <- file.path(PSM_OUTPUT_ROOT,PSM_SCENARIO,PSM_REGION,PSM_DATE)
run_old <- file.path(PSM_OUTPUT_ROOT,PSM_SCENARIO,PSM_REGION,REFERENCE_DATE)
out_new <- file.path(PROJECT_DIR,'output',paste(PSM_SCENARIO,PSM_REGION,PSM_DATE,
  '2020_2050_vs',REFERENCE_DATE,sep='_'))
out_old <- file.path(PROJECT_DIR,'output',paste(PSM_SCENARIO,PSM_REGION,REFERENCE_DATE,YEAR,'500C',sep='_'))
stage <- '--stage-generated' %in% commandArgs(TRUE)
stage_dir <- file.path(PROJECT_DIR,'work','generation_template')
if(stage) dir.create(stage_dir,recursive=TRUE,showWarnings=FALSE)
if(anyNA(target_scenarios)||any(!nzchar(target_scenarios))||!single_scenario %in% target_scenarios)
  stop('Invalid target scenarios.')
source(file.path(PLOT_REPO,'IntermReport/code/data_import.R'))
source(file.path(PLOT_REPO,'IntermReport/code/plot_common.R'))
source(file.path(CODE_DIR,'generation_template_helpers.R'))
gdxrrw::igdx('C:/GAMS/win64/26.1')

# READ ALREADY ANNUAL IAMC DATA; PRIMARY FLOWS ARE NOT NEEDED -------------------
for(run in c(run_new,run_old)) {
  log <- readLines(file.path(run,'modelstat/systemlog/systemlog.txt'),warn=FALSE)
  if(!any(grepl(PSM_COMMIT,log,fixed=TRUE))) stop('Unexpected producer commit.')
}
current <- read_iamc_gdx(file.path(run_new,'main'),'PSM') %>%
  filter(Scenario==single_scenario,Year %in% target_years)
reference <- read_iamc_gdx(file.path(run_old,'main'),'PSM') %>%
  filter(Scenario %in% target_scenarios,Year==YEAR)
if(!setequal(unique(current$Year),target_years)||!setequal(unique(reference$Scenario),target_scenarios))
  stop('Missing scenario/year data.')
# SAME BAR GEOMETRY AND BOTTOM LEGEND; TEMPLATE COLORS AND ORDER ONLY -----------
source(file.path(CODE_DIR,'transport_summary.R'))
make_bar <- function(iamc,scenario_map,axis_labels,title,subtitle,trend=FALSE,width=.68) {
  reference <- reference_generation(iamc,scenario_map)
  z <- reference$data %>% filter(Year %in% unique(iamc$Year))
  z$Axis <- if(trend) factor(z$Year,levels=target_years) else z$ScenarioKey
  colors <- reference$scales$get_scales('fill')
  p <- ggplot(z,aes(Axis,Value,fill=Legend)) +
    geom_col(width=width,color='white',linewidth=.15,position=position_stack(reverse=TRUE)) +
    colors + scale_x_discrete(labels=axis_labels,drop=FALSE) +
    scale_y_continuous(expand=expansion(mult=c(.02,.08))) +
    geom_hline(yintercept=0,color=if(width==.65) 'grey35' else 'grey40',linewidth=.3) +
    labs(x=NULL,y='Power generation (EJ/yr)',fill=NULL,title=title,subtitle=subtitle,
      caption=if(width==.65) 'Negative bars: curtailment and storage / transmission losses, following the interim report.' else
        'Negative bars: curtailment and storage / transmission losses. Annual IAMC data are not reweighted.') +
    summary_theme() + guides(fill=guide_legend(nrow=3,byrow=TRUE))
  if(width!=.65) p <- p+theme(axis.text.x=element_text(size=10))
  p
}
trend_plot <- make_bar(current,setNames(single_scenario,'6HOUR'),as.character(target_years),
  'Power generation','500C | H2 pipeline + truck; NH3 rail + truck + ship | 6HOUR',trend=TRUE)
comparison_input <- bind_rows(current %>% filter(Year==YEAR) %>% mutate(Scenario='6HOUR'),
  reference %>% filter(Scenario==single_scenario) %>% mutate(Scenario='12DAY'))
comparison_plot <- make_bar(comparison_input,c('12DAY'='12DAY','6HOUR'='6HOUR'),
  c('12DAY'='12DAY','6HOUR'='6HOUR'),'Power generation',
  'Same named scenario | 2050 | 12DAY versus 6HOUR')
four_cases_plot <- make_bar(reference,setNames(target_scenarios,target_scenarios),
  setNames(scenario_axis_labels,target_scenarios),'Power generation | 2050',
  '500C scenarios | H2 pipeline and NH3 rail + ship available in every case',width=.65)

# SAVE ONLY THE THREE GENERATION PNG FILES ------------------------------------
trend_path <- file.path(if(stage) stage_dir else out_new,'generation_mix_2020_2050_6HOUR.png')
comparison_path <- file.path(if(stage) stage_dir else out_new,'generation_mix_2050_12DAY_vs_6HOUR.png')
four_cases_path <- file.path(if(stage) stage_dir else out_old,'generation_mix_2050.png')
ggsave(trend_path,trend_plot,device=ragg::agg_png,width=300,height=175,units='mm',dpi=300,bg='white')
ggsave(comparison_path,comparison_plot,device=ragg::agg_png,width=300,height=175,units='mm',dpi=300,bg='white')
ggsave(four_cases_path,four_cases_plot,device=ragg::agg_png,width=300,height=175,units='mm',dpi=300,bg='white')
message('Completed: 3 generation PNGs using the interim-report template.')
