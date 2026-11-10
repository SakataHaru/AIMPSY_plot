# INTERIM-REPORT FIGURE 1-D AS THE GENERATION TEMPLATE -------------------------
# Source definitions only: the report entry point is not executed.
source(file.path(PLOT_REPO,'IntermReport/code/fig1/fig1_d.R'))

reference_generation <- function(iamc,scenario_map) {
  input <- list(aimt_iamc=tibble(),psm_iamc=iamc,variable_specs=list(),
    metadata=list(scenario_map=scenario_map))
  p <- make_fig1_d(input)
  lookup <- tibble(ScenarioKey=names(scenario_map),Scenario=unname(scenario_map))
  observed <- iamc %>% filter(Scenario %in% unname(scenario_map),
    Region %in% PSM_WORLD_REGIONS,Variable=='Sec_Ene_Ele') %>%
    group_by(Scenario,Year) %>% summarise(IAMC=sum(Value),.groups='drop')
  audit <- p$data %>% filter(Sign==1) %>%
    mutate(ScenarioKey=as.character(ScenarioKey)) %>% group_by(ScenarioKey,Year) %>%
    summarise(Components=sum(Value),.groups='drop') %>%
    left_join(lookup,by='ScenarioKey') %>% inner_join(observed,by=c('Scenario','Year'))
  if(nrow(audit)!=nrow(observed) || any(abs(audit$Components-audit$IAMC)>1e-5))
    stop('Template generation components do not reconcile with IAMC totals.')
  print(audit,n=Inf)
  p
}

apply_generation_template <- function(p,iamc) {
  ids <- unique(as.character(iamc$Scenario))
  reference <- reference_generation(iamc,setNames(ids,ids))
  # Only replace the category-color scale and legend order. Keep the caller's
  # bars, stacking, axes, bottom legend position, dimensions and other styling.
  suppressMessages(p + reference$scales$get_scales('fill') + labs(fill=NULL) +
    guides(fill=guide_legend(nrow=3,byrow=TRUE)))
}
