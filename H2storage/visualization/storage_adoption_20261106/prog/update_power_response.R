# PLOT SETTINGS ---------------------------------------------------------------
PSM_SCENARIO <- 'H2_storage_compressor'
PSM_REGION <- 'NorthAmerica'
PSM_DATE <- '2610291832'
target_scenarios <- c('500C','500C-H2storage','500C-H2sto_only_tank','500C-H2sto_only_cavern')
scenario_labels <- c('No storage','Tank + cavern','Tank only','Cavern only')
target_year <- 2045L
timeresol <- '6HOUR'

# PATHS, SHARED CLASSIFICATION AND EXISTING DISPLAY STYLE ----------------------
.libPaths(c('C:/Users/Haru Sakata/AppData/Local/R/win-library/4.4',.libPaths()))
suppressPackageStartupMessages({library(gdxrrw);library(dplyr);library(tidyr);library(ggplot2);library(cowplot)})
igdx('C:/GAMS/win64/26.1')
script <- sub('^--file=','',commandArgs()[grepl('^--file=',commandArgs())])[1]
project <- dirname(dirname(normalizePath(script,winslash='/')))
workspace <- normalizePath(file.path(project,'../../../..'),winslash='/')
source(file.path(project,'prog/generation_style.R'))
source_root <- '//SY_3600_01/atfs02/Individual/hsakata/AIMPSY_2nd/output'
run <- file.path(source_root,PSM_SCENARIO,PSM_REGION,PSM_DATE)
output <- file.path(project,'output',paste0(PSM_REGION,'_',PSM_DATE))
weights <- c('12DAY'=ceiling(365/12*1000)/1000,'6HOUR'=6,'4HOUR'=4,'3HOUR'=3,'1HOUR'=1)
timeresol <- toupper(timeresol);stopifnot(timeresol %in% names(weights))
primary_weight <- unname(weights[timeresol])
case <- function(x)factor(scenario_labels[match(x,target_scenarios)],levels=scenario_labels)
case_colors <- setNames(c('#666666','#009E73','#0072B2','#E69F00'),scenario_labels)
# Reuse reader/status definitions without executing the evolution entry point.
definitions <- parse(file.path(project,'prog/system_evolution.R'))
for(expr in definitions)if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&
  as.character(expr[[2]]) %in% c('readpar','filtered','check_year','capacity_values'))eval(expr)
mapping <- read.csv(file.path(workspace,'AIMPSY_plot/IntermReport/data/M_R272.csv'))
nodes <- mapping$N_IND[mapping$R31 %in% c('USA','CAN','MEX')]
technologies <- c('Solar PV','Wind','Electrolyzer','Battery power')

# FOUR SUCCESSFUL PRIMARY RESULTS; UNCHANGED TOTAL GENERATION AND CAPACITY ----
generation <- response <- list()
for(s in target_scenarios) {
  check_year(file.path(run,'gams_output/log',s,paste0(target_year,'.lst')))
  fp <- file.path(run,'gams_output/gdx_primary',s,paste0(target_year,'.gdx'))
  g <- readpar(fp,'vgs_l');l <- readpar(fp,'vfs_l')
  wt <- readpar(file.path(run,'data/input',paste0(s,'.gdx')),'wt')
  times <- wt$T[grepl('^d[0-9]{3}t',wt$T)]
  stopifnot(length(times)==1460,all(wt$Value[wt$T %in% times]==primary_weight))
  vg <- filtered(fp,'vg_l',list(nodes,'AC',unique(g$R[g$I=='AC']),times)) %>% mutate(Technology=genclass(R))
  stopifnot(all(abs(vg$Value[is.na(vg$Technology)])<1e-8))
  z <- vg %>% filter(!is.na(Technology)) %>% group_by(Technology) %>% summarise(Value=sum(Value)*primary_weight/1000,.groups='drop')
  stopifnot(abs(sum(z$Value)-sum(vg$Value)*primary_weight/1000)<1e-5)
  message(s,' total generation TWh=',sum(z$Value));print(z)
  generation[[s]] <- z %>% mutate(Case=case(s))
  response[[s]] <- tibble(Scenario=s,Case=case(s),Technology=c(technologies,'BECCS'),
    Value=c(capacity_values(g,l),sum(g$Value[g$I=='AC'&g$R=='biomass_ccs'])))
}
generation <- bind_rows(generation)
response <- bind_rows(response) %>% group_by(Technology) %>% mutate(Delta=Value-Value[Scenario==target_scenarios[1]]) %>% ungroup()

# UPDATED GENERATION PANEL AND EXISTING CAPACITY-CHANGE PANEL ------------------
theme_set(theme_minimal(base_size=11)+theme(panel.grid.minor=element_blank(),legend.position='bottom',
  plot.title=element_text(face='bold'),plot.caption=element_text(hjust=0,size=8),
  plot.background=element_rect(fill='white',color=NA)))
pgen <- ggplot(generation,aes(Case,Value,fill=Technology))+
  geom_col(position=position_stack(reverse=TRUE),width=.65,show.legend=TRUE)+
  scale_fill_manual(values=gencolors,limits=secele$Legend,drop=FALSE)+
  guides(fill=guide_legend(ncol=3,byrow=TRUE))+
  labs(title='Electricity generation',x=NULL,y='TWh/yr',fill=NULL)
presponse <- ggplot(filter(response,Scenario!=target_scenarios[1]),aes(Technology,Delta,fill=Case))+
  geom_col(position='dodge')+geom_hline(yintercept=0,linewidth=.3)+scale_fill_manual(values=case_colors)+coord_flip()+
  labs(title='Installed capacity change from no storage',x=NULL,y='Capacity difference (GW)',fill=NULL)
stamp <- paste0(PSM_REGION,' | ',target_year,' | 6-hour power / daily H2 | Run ',PSM_DATE,
  '\n2050 infeasible; results through 2045 only.')
# Header / generation and installed-capacity response, each with its own key.
pgen <- pgen+power_response_text_theme()
presponse <- presponse+power_response_text_theme()
combined <- plot_grid(pgen,presponse,nrow=1,labels=c('a','b'),label_size=16,rel_widths=c(1,1.1))
combined <- plot_grid(ggdraw()+draw_label(stamp,size=12),combined,ncol=1,rel_heights=c(.16,1))
ggsave(file.path(output,'06_power_system_response.png'),combined,width=14,height=7.5,units='in',dpi=240,bg='white')
