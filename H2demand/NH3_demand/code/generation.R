# Reuse Fig. 1-d's categories, colors, stacking, losses and theme for CHN.
make_china_generation <- function(iamc, scenario_map, reference_dir) {
  input <- list(aimt_iamc=tibble(), psm_iamc=iamc, variable_specs=list(),
                metadata=list(scenario_map=scenario_map))
  p <- make_fig1_d(input)
  totals <- p$data %>% filter(Sign == 1) %>% group_by(ScenarioKey, Year) %>%
    summarise(ComponentTotal=sum(Value), .groups='drop') %>%
    mutate(Scenario=unname(scenario_map[as.character(ScenarioKey)])) %>%
    left_join(iamc %>% filter(Region=='CHN', Variable=='Sec_Ene_Ele') %>%
                select(Scenario, Year, IAMC_Total=Value), by=c('Scenario','Year'))
  if (any(is.na(totals$IAMC_Total)) || any(abs(totals$ComponentTotal-totals$IAMC_Total)>1e-5)) {
    stop('China generation components do not reconcile to the IAMC total')
  }
  p
}
