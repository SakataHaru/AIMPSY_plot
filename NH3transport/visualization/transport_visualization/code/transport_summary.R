# NODE PIES: REFERENCE FIGURE 3-B DEFINITION -----------------------------------
physical_point <- function(x) sub(
  '-(H2-ROAD|NH3-RAIL|H2TRD|NH3TRD|SYFTRD|SYMTRD|H2|NH3|SYF|SYM|AC)$','',x)

link_endpoints <- function(data) {
  parts <- strsplit(as.character(data$Link),'_',fixed=TRUE)
  if(any(lengths(parts)!=2L)) stop('Unexpected link endpoint label.')
  data$Point0 <- vapply(parts,`[[`,character(1),1L)
  data$Point1 <- vapply(parts,`[[`,character(1),2L)
  data
}

prepare_node_pies <- function(iamc,annual,geo) {
  total <- iamc %>% filter(Variable=='Sec_Ene_Hyd',Region %in% geo$nodes$Point) %>%
    group_by(Scenario,Region) %>% summarise(TotalH2=sum(Value),.groups='drop')
  missing <- setdiff(target_scenarios,total$Scenario)
  if(length(missing)) {
    # Sparse IAMC omits zero production in early years; distinguish that from
    # a positive production total whose detailed-node allocation is missing.
    positive <- iamc %>% filter(Scenario %in% missing,Variable=='Sec_Ene_Hyd',Value>1e-8)
    if(nrow(positive)) stop('Positive hydrogen IAMC total lacks detailed-node data.')
    total <- bind_rows(total,expand_grid(Scenario=missing,Region=geo$nodes$Point) %>% mutate(TotalH2=0))
    message('No positive hydrogen production in this year: zero node pies for ',paste(missing,collapse=', '))
  }
  conv <- annual %>% filter(grepl('-(H2|NH3|SYF|SYM)_.*-(H2|NH3|SYF|SYM)$',Link)) %>%
    link_endpoints() %>% mutate(
      From=ifelse(Direction=='vff_l',Point0,Point1),
      To=ifelse(Direction=='vff_l',Point1,Point0),
      FromCarrier=sub('^.*-','',From),ToCarrier=sub('^.*-','',To),
      Region=physical_point(From),
      Category=case_when(FromCarrier=='H2' & ToCarrier=='NH3'~'NH3',
        FromCarrier=='H2' & ToCarrier %in% c('SYF','SYM')~'Synfuel',TRUE~NA_character_)) %>%
    filter(!is.na(Category)) %>% group_by(Scenario,Region,Category) %>%
    summarise(Value=sum(FlowEJ),.groups='drop')
  wide <- conv %>% complete(Scenario=target_scenarios,
     Region=unique(c(geo$nodes$Point,geo$regions$Point)),
     Category=c('NH3','Synfuel'),fill=list(Value=0)) %>%
    pivot_wider(names_from=Category,values_from=Value,values_fill=0) %>%
    full_join(total,by=c('Scenario','Region')) %>%
    mutate(across(c(TotalH2,NH3,Synfuel),~replace_na(.x,0)),
      Residual=TotalH2-NH3-Synfuel,`Direct H2`=pmax(Residual,0))
  message('Node pie audit: reference residual clipped at zero; R31 conversion nodes retained:')
  print(wide %>% group_by(Scenario) %>% summarise(
    IAMC_H2=sum(TotalH2),NH3Input=sum(NH3),SynfuelInput=sum(Synfuel),
    ClippedNodes=sum(Residual < -1e-6),ClippedEJ=sum(pmax(-Residual,0)),.groups='drop'))
  values <- wide %>% select(Scenario,Region,`Direct H2`,NH3,Synfuel) %>%
    pivot_longer(c(`Direct H2`,NH3,Synfuel),names_to='Category',values_to='Value') %>%
    group_by(Scenario,Region) %>% mutate(Total=sum(Value)) %>% ungroup() %>%
    mutate(Order=match(Category,names(FIG3_H2_NODE_COLORS)),Half='full') %>%
    left_join(geo$coordinates,by=c('Region'='Point'))
  if(any(!is.finite(values$Value)) || any(values$Value < -1e-8) ||
     anyNA(values$lon[values$Total>1e-6])) stop('Invalid or unmapped pie data.')
  values
}

node_legend_panel <- function(max_value) {
  breaks <- scales::breaks_pretty(n=3)(c(0,max_value))
  breaks <- tail(breaks[breaks>0 & breaks<=max_value],3)
  sizes <- tibble(Value=breaks,x=62+26*(seq_along(breaks)-1),y=10)
  colors <- tibble(Category=names(FIG3_H2_NODE_COLORS),x=c(150,203,249),y=10)
  ggplot() + geom_point(data=sizes,aes(x,y,size=Value),shape=21,fill='white',
       color='grey25',stroke=.35,show.legend=FALSE) +
    geom_text(data=sizes,aes(x=x+8,y=y,label=scales::number(Value,accuracy=.1)),
       hjust=0,size=4,show.legend=FALSE) +
    scale_size_area(limits=c(0,max_value),max_size=12.4,guide='none') +
    annotate('text',x=6,y=10,label='H2 allocation\n(EJ/yr)',hjust=0,size=4.1) +
    geom_point(data=colors,aes(x,y,fill=Category),shape=22,size=3,
       color='grey35',stroke=.2,show.legend=FALSE) +
    geom_text(data=colors,aes(x=x+5,y=y,label=Category),hjust=0,size=4,
       show.legend=FALSE) +
    scale_fill_manual(values=FIG3_H2_NODE_COLORS,guide='none') +
    coord_cartesian(xlim=c(0,300),ylim=c(0,20),expand=FALSE) +
    theme_void() + theme(plot.background=element_rect(fill='white',color=NA),
       plot.margin=margin(1,0,1,0,unit='mm'))
}

# COMMON SCENARIO AXES AND REPORT STYLE ----------------------------------------
summary_theme <- function() {
  MyTheme + theme(text=element_text(size=12),plot.title=element_text(size=15,face='bold'),
    plot.subtitle=element_text(size=10),axis.text.x=element_text(angle=0,hjust=.5,size=10),
    axis.text.y=element_text(size=10),axis.title.y=element_text(size=11),
    legend.text=element_text(size=10),legend.title=element_text(size=10),
    legend.position='bottom',plot.caption=element_text(size=9,hjust=0),
    strip.text=element_text(size=12,face='bold'),plot.margin=margin(8,10,8,8))
}

# Larger type for the specifically requested 2050 resolution-comparison figure.
fuel_comparison_text_theme <- function(scale=1.4) {
  theme(text=element_text(size=12*scale),plot.title=element_text(size=15*scale),
    plot.subtitle=element_text(size=10*scale),axis.text.x=element_text(size=10*scale),
    axis.text.y=element_text(size=10*scale),axis.title.y=element_text(size=11*scale),
    strip.text=element_text(size=12*scale),strip.text.x=element_text(size=12*scale),
    strip.text.y=element_text(size=12*scale),legend.text=element_text(size=10*scale),
    legend.title=element_text(size=10*scale),plot.caption=element_text(size=9*scale),
    legend.key.height=grid::unit(5.5,'mm'),legend.key.width=grid::unit(6,'mm'))
}

scenario_axis <- function() scale_x_discrete(limits=target_scenarios,
  labels=setNames(scenario_axis_labels,target_scenarios),drop=FALSE)

summary_bar <- function(data,colors,y,title=NULL) {
  ggplot(data,aes(Scenario,Value,fill=Category)) +
    geom_col(width=.65,color='white',linewidth=.15,position=position_stack(reverse=TRUE)) +
    scenario_axis() + scale_fill_manual(values=colors,drop=FALSE,name=NULL) +
    scale_y_continuous(expand=expansion(mult=c(.02,.08))) +
    labs(x=NULL,y=y,title=title) + summary_theme()
}

# GENERATION, FUEL, BIOMASS AND TRANSMISSION COMPARISON -------------------------
save_summary_figures <- function(iamc,annual,capacity,geo) {
  subtitle <- '500C scenarios | H2 pipeline and NH3 rail + ship available in every case'
  # Same parent-variable grouping and palette as IntermReport/fig1/fig1_d.R.
  spec <- tribble(~Variable,~Category,~Color,~Sign,
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
  world <- iamc %>% filter(Region %in% PSM_WORLD_REGIONS)
  gen <- world %>% inner_join(spec,by='Variable') %>% group_by(Scenario,Category) %>%
    summarise(Value=sum(Value*Sign),.groups='drop')
  gen_colors <- setNames(spec$Color,spec$Category);gen_colors<-gen_colors[!duplicated(names(gen_colors))]
  gen <- gen %>% mutate(Category=factor(Category,levels=names(gen_colors)))
  audit <- gen %>% filter(!Category %in% c('Curtailment','Storage loss','Transmission loss')) %>%
    group_by(Scenario) %>% summarise(Components=sum(Value),.groups='drop') %>%
    left_join(world %>% filter(Variable=='Sec_Ene_Ele') %>% group_by(Scenario) %>%
      summarise(IAMC=sum(Value),.groups='drop'),by='Scenario')
  print(audit)
  if(nrow(audit)!=length(target_scenarios) || any(abs(audit$Components-audit$IAMC)>1e-5))
    stop('Generation components do not reconcile with Sec_Ene_Ele.')
  p_generation <- summary_bar(gen,gen_colors,'Power generation (EJ/yr)',paste('Power generation |',YEAR)) +
    geom_hline(yintercept=0,linewidth=.3,color='grey35') +
    labs(subtitle=subtitle,caption='Negative bars: curtailment and storage / transmission losses, following the interim report.') +
    guides(fill=guide_legend(nrow=3,byrow=TRUE))
  p_generation <- apply_generation_template(p_generation,iamc)

  fuel <- annual %>% filter(!is.na(Mode)) %>% link_endpoints() %>%
    filter(physical_point(Point0)!=physical_point(Point1)) %>% group_by(Scenario,Mode) %>%
    summarise(Value=sum(FlowEJ),.groups='drop') %>%
    complete(Scenario=target_scenarios,Mode=names(MODE_COLORS),fill=list(Value=0)) %>%
    mutate(Carrier=case_when(grepl('^H2',Mode)~'Hydrogen',grepl('^NH3',Mode)~'Ammonia',TRUE~'Synfuel'),
      Carrier=factor(Carrier,levels=c('Hydrogen','Ammonia','Synfuel')),
      Category=factor(Mode,levels=names(MODE_COLORS)))
  fuel_colors <- c('H2 pipeline'='thistle3','H2 road'='#88618E',
    'NH3 rail'='#8E5EA2','NH3 road'='#C3A3CF','NH3 ship'='#553768','Synfuel ship'='mediumorchid1')
  p_fuel <- summary_bar(fuel,fuel_colors,'Gross transport (EJ/yr)',paste('Hydrogen-based fuel transport |',YEAR)) +
    facet_wrap(vars(Carrier),nrow=1,drop=FALSE) +
    labs(subtitle=subtitle,caption='Sum of forward + backward flows over physical links; multi-leg transport is counted on each leg.\nNo map display threshold is applied. H2 truck flows are positive but too small to distinguish at this scale.') +
    theme(axis.text.x=element_text(size=9)) + guides(fill=guide_legend(nrow=2,byrow=TRUE))
  print(fuel %>% select(Scenario,Mode,Value))

  biomass <- annual %>% mutate(Category=case_when(
    grepl('^R31-[A-Z0-9]+-(CRN|CRNTRD)_R31-[A-Z0-9]+-(CRN|CRNTRD)$',Link)~'Solid biomass',
    grepl('^R31-[A-Z0-9]+-(BMS|BMSTRD)_R31-[A-Z0-9]+-(BMS|BMSTRD)$',Link)~'Liquid biomass',TRUE~NA_character_)) %>%
    filter(!is.na(Category)) %>% group_by(Scenario,Category) %>%
    summarise(Value=sum(FlowEJ),.groups='drop') %>% complete(Scenario=target_scenarios,
      Category=c('Solid biomass','Liquid biomass'),fill=list(Value=0)) %>%
    mutate(Category=factor(Category,levels=c('Solid biomass','Liquid biomass')))
  p_biomass <- summary_bar(biomass,c('Solid biomass'='#359EAA','Liquid biomass'='#006D77'),
      'Gross transport (EJ/yr)',paste('Biomass transport |',YEAR)) +
    labs(subtitle=subtitle,caption='R31-to-R31 ship links; sum of forward + backward annual flows. Imports from outside the modeled network are excluded.')
  print(biomass)

  mapping <- read.csv(file.path(GEO_DIR,'M_R272.csv'),stringsAsFactors=FALSE)
  categorize_grid <- function(data) {
    z <- link_endpoints(data)
    z$Node0 <- physical_point(z$Point0);z$Node1 <- physical_point(z$Point1)
    z <- z[z$Node0!=z$Node1,]
    a <- mapping$R31[match(z$Node0,mapping$N_IND)]
    b <- mapping$R31[match(z$Node1,mapping$N_IND)]
    if(anyNA(a)||anyNA(b)) stop('Unmapped AC transmission endpoint.')
    z %>% mutate(Category=ifelse(a==b,'Within R31','Across R31'),
      Pair=paste(pmin(Node0,Node1),pmax(Node0,Node1),sep='__'))
  }
  cap <- categorize_grid(capacity)
  check <- cap %>% group_by(Scenario,Pair) %>% summarise(Delta=max(CapacityGW)-min(CapacityGW),.groups='drop')
  if(any(check$Delta>1e-8)) stop('Conflicting directional grid capacity records.')
  cap <- cap %>% group_by(Scenario,Category,Pair) %>% summarise(Value=max(CapacityGW)/1000,.groups='drop') %>%
    group_by(Scenario,Category) %>% summarise(Value=sum(Value),.groups='drop')
  flow <- annual %>% filter(grepl('-AC_.*-AC$',Link)) %>% categorize_grid() %>%
    group_by(Scenario,Category) %>% summarise(Value=sum(FlowEJ),.groups='drop')
  grid_colors <- c('Within R31'='lightsteelblue','Across R31'='#4C78A8')
  cap$Category<-factor(cap$Category,levels=names(grid_colors))
  flow$Category<-factor(flow$Category,levels=names(grid_colors))
  p_cap <- summary_bar(cap,grid_colors,'Installed capacity (TW)','Transmission capacity')
  p_flow <- summary_bar(flow,grid_colors,'Gross transmission (EJ/yr)','Transmission volume')
  legend_source <- ggplotGrob(p_cap+theme(legend.position='bottom',legend.direction='horizontal'))
  grid_legend <- cowplot::get_legend(gtable::gtable_filter(legend_source,'^guide-box-bottom$'))
  # Title / two panels / shared legend / definition note.
  grid_panels <- cowplot::plot_grid(p_cap+theme(legend.position='none'),p_flow+theme(legend.position='none'),
      nrow=1,labels=c('a','b'),label_size=13,align='h')
  grid_title <- cowplot::ggdraw()+cowplot::draw_label(paste('Electricity transmission |',YEAR),
      x=.02,hjust=0,fontface='bold',size=15)
  grid_note <- cowplot::ggdraw()+cowplot::draw_label(
      'Capacity: one record per physical AC-ended corridor. Volume: forward + backward link flows.\nAcross-R31 capacity follows the interim-report boundary; within-R31 links are shown separately.',
      x=.02,hjust=0,size=9)
  p_grid <- cowplot::plot_grid(grid_title,grid_panels,grid_legend,grid_note,ncol=1,
      rel_heights=c(.08,1,.10,.14))
  print(cap);print(flow)
  ggsave(file.path(OUTPUT_DIR,paste0('generation_mix_',YEAR,'.png')),p_generation,
    device=ragg::agg_png,width=300,height=175,units='mm',dpi=300,bg='white')
  ggsave(file.path(OUTPUT_DIR,paste0('fuel_transport_by_mode_',YEAR,'.png')),p_fuel,
    device=ragg::agg_png,width=320,height=180,units='mm',dpi=300,bg='white')
  ggsave(file.path(OUTPUT_DIR,paste0('biomass_transport_',YEAR,'.png')),p_biomass,
    device=ragg::agg_png,width=300,height=160,units='mm',dpi=300,bg='white')
  ggsave(file.path(OUTPUT_DIR,paste0('transmission_capacity_and_flow_',YEAR,'.png')),p_grid,
    device=ragg::agg_png,width=320,height=170,units='mm',dpi=300,bg='white')
}
