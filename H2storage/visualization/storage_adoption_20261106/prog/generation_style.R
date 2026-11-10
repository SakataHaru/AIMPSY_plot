# USER-SPECIFIED GENERATION PALETTE AND BOTTOM-TO-TOP ORDER --------------------
secele <- tibble::tribble(~Variable,~Legend,~Color,
  'Sec_Ene_Ele_Oil_wo_CCS','Oil w/o CCS','sandybrown',
  'Sec_Ene_Ele_Oil_w_CCS','Oil w/ CCS','tan3',
  'Sec_Ene_Ele_Coa_wo_CCS','Coal w/o CCS','grey50',
  'Sec_Ene_Ele_Coa_w_CCS','Coal w/ CCS','grey30',
  'Sec_Ene_Ele_Gas_wo_CCS','Gas w/o CCS','lightgoldenrod',
  'Sec_Ene_Ele_Gas_w_CCS','Gas w/ CCS','lightgoldenrod3',
  'Sec_Ene_Ele_Nuc','Nuclear','moccasin',
  'Sec_Ene_Ele_Hyp','Hydro','lightsteelblue',
  'Sec_Ene_Ele_Bio_wo_CCS','Biomass w/o CCS','darkolivegreen2',
  'Sec_Ene_Ele_Bio_w_CCS','Biomass w/ CCS','darkolivegreen4',
  'Sec_Ene_Ele_Geo','Geothermal','peru',
  'Sec_Ene_Ele_Solar','Solar','lightsalmon',
  'Sec_Ene_Ele_Win','Wind','lightskyblue3',
  'Sec_Ene_Ele_Hyd','Hydrogen','thistle2')
gencolors <- setNames(secele$Color,secele$Legend)
power_response_text_theme <- function() ggplot2::theme(
  text=ggplot2::element_text(size=14),
  axis.text=ggplot2::element_text(size=12),axis.title=ggplot2::element_text(size=14),
  plot.title=ggplot2::element_text(size=15,face='bold'),
  legend.text=ggplot2::element_text(size=12),legend.title=ggplot2::element_text(size=12),
  legend.key.size=grid::unit(5,'mm'))
genclass <- function(r) {
  ccs <- grepl('(^|_)ccs(_|$)',r)
  label <- dplyr::case_when(
    grepl('^oil',r)~ifelse(ccs,'Oil w/ CCS','Oil w/o CCS'),
    grepl('^coal',r)~ifelse(ccs,'Coal w/ CCS','Coal w/o CCS'),
    grepl('^gas',r)~ifelse(ccs,'Gas w/ CCS','Gas w/o CCS'),
    grepl('^nuclear',r)~'Nuclear',grepl('^hydro',r)~'Hydro',
    grepl('^biomass',r)~ifelse(ccs,'Biomass w/ CCS','Biomass w/o CCS'),
    grepl('^geothermal',r)~'Geothermal',grepl('^solar',r)~'Solar',
    grepl('^wind',r)~'Wind',grepl('^h2',r)~'Hydrogen',TRUE~NA_character_)
  factor(label,levels=secele$Legend)
}
