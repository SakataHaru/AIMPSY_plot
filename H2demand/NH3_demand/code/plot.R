# Run from any directory: Rscript H2demand/NH3_demand/code/plot.R
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(ggplot2); library(gdxrrw); library(sf); library(cowplot)})
script <- sub('^--file=', '', commandArgs()[grepl('^--file=', commandArgs())][1])
base <- dirname(dirname(normalizePath(script, winslash='/')))
repo <- dirname(dirname(base)); ref <- file.path(repo,'IntermReport/code')
out <- file.path(base,'output'); dir.create(out, recursive=TRUE, showWarnings=FALSE)
source(file.path(ref,'plot_common.R')); source(file.path(ref,'data_import.R'))
source(file.path(base,'code/fig3_common.R'))
source(file.path(base,'code/fig3_b.R'))
source(file.path(base,'code/transport.R'))
scenarios <- c('500C','500C-NH3_R272_ROAD'); sm <- setNames(scenarios,scenarios)
year <- 2050L
time_weight <- primary_time_weight_from_timeresol('12DAY')
panel_scale <- length(scenarios)/2
igdx('C:/GAMS/win64/26.1')
iamc <- read_iamc_gdx(file.path(base,'data/main'),'PSM') %>% filter(Scenario %in% scenarios)
coverage <- tidyr::crossing(Scenario=scenarios,Region=PSM_WORLD_REGIONS,Year=seq(2020,year,5))
missing <- anti_join(coverage,iamc %>% filter(Variable=='Sec_Ene_Ele'),by=c('Scenario','Region','Year'))
if(nrow(missing)) stop('Missing world-region generation totals: ',paste(capture.output(print(missing)),collapse='\n'))
source(file.path(base,'code/generation.R'))
source(file.path(base,'code/fig1_d.R'))
source(file.path(base,'code/fig2_d.R'))
source(file.path(base,'code/fig2_c.R'))
save_generation <- function() {
 save_png_mm(make_world_generation(iamc,sm,year)+labs(title='World: power generation, 2020-2050'),
             file.path(out,'generation_mix_WORLD_2020_2050.png'),
             width_mm=355.6*panel_scale,height_mm=152.4,dpi=240)
}
if ('--generation-only' %in% commandArgs(trailingOnly=TRUE)) {
 save_generation()
 message('Saved world generation PNG')
 quit(save='no')
}
geo <- load_fig3_geodata(file.path(repo,'IntermReport/data'))
rmapping <- read.csv(file.path(repo,'IntermReport/data/M_R272.csv'))
if(!setequal(unique(rmapping$R31),PSM_WORLD_REGIONS)) stop('Unexpected world-region mapping')
nodes <- geo$nodes$Node
primary <- list(psm_vfs_l=tibble(),psm_vff_l=tibble(),psm_vfb_l=tibble(),
 psm_region_map=transmute(rmapping,Node=N_IND,Region=R31),
 metadata=list(year=year,scenario_map=sm,time_resol='12DAY',time_weight_hours=time_weight))
demands <- list(); weights <- list()
for (s in scenarios) {
 path <- file.path(base,'data/gdx_primary',s,paste0(year,'.gdx'))
 if(!file.exists(path)) stop('Missing primary GDX: ',path)
 fw <- rgdx.param(path,'vff_l'); bw <- rgdx.param(path,'vfb_l')
 times <- unique(c(as.character(fw$T),as.character(bw$T)))
 hourly <- times[grepl('^d[0-9]+t[0-9]+$',times)]
 days <- split(sub('^.*t','',hourly),sub('t.*$','',hourly))
 if(length(days)!=12L || any(lengths(days)!=24L) ||
    !all(vapply(days,function(x) setequal(as.integer(x),0:23),logical(1)))) stop('Expected 12 days x 24 hours: ',s)
 w <- time_weight; weights[[s]] <- w
 message(s,': 12 representative days x 24 hours; annual weight = ',w)
 for(sym in c('vff_l','vfb_l')) {
  z <- if(sym=='vff_l') fw else bw
  primary[[paste0('psm_',sym)]] <- bind_rows(primary[[paste0('psm_',sym)]],
   z %>% transmute(Scenario=s,Link=as.character(L),Value=as.numeric(.data[[sym]])*w*3.6e-6) %>%
    group_by(Scenario,Link) %>% summarise(Value=sum(Value),.groups='drop'))
 }
 z <- rgdx.param(path,'dmd')
 demands[[s]] <- z %>% filter(as.character(MT) %in% c(times,'annual'), I %in% c('H2','NH3','SYF','SYM')) %>%
  transmute(Scenario=s,Node=as.character(N),Carrier=as.character(I),Value=as.numeric(dmd)*w*3.6e-6) %>%
  group_by(Scenario,Carrier) %>% summarise(Value=sum(Value),.groups='drop')
}
saveplot <- function(p,name,width=14,height=6) {
 save_png_mm(p,file.path(out,paste0(name,'.png')),width_mm=width*25.4,height_mm=height*25.4,dpi=240)
}
carrier_transport <- make_fig2_c(primary)
print(attr(carrier_transport,'fig2_c_data') %>% select(Scenario,Carrier,Transport,TransportEJ),width=Inf)
saveplot(carrier_transport,'carrier_transport_WORLD_2050',12,6)
if ('--transport-only' %in% commandArgs(trailingOnly=TRUE)) {
 message('Saved world carrier transport PNG'); quit(save='no')
}
spec <- tibble::tribble(~Variable,~Category,~Color,
 'Sec_Ene_Ele_Coa','Coal','grey45','Sec_Ene_Ele_Gas','Gas','#D8BC75',
 'Sec_Ene_Ele_Oil','Oil','sandybrown','Sec_Ene_Ele_Nuc','Nuclear','moccasin',
 'Sec_Ene_Ele_Hyp','Hydro','#2C7FB8','Sec_Ene_Ele_Bio','Biomass','#A9D65D',
 'Sec_Ene_Ele_Geo','Geothermal','peru','Sec_Ene_Ele_Solar','Solar','lightsalmon',
 'Sec_Ene_Ele_Win','Wind','lightskyblue3','Sec_Ene_Ele_Hyd','Hydrogen','thistle2',
 'Sec_Ene_Ele_Gas_Hyd_Cof','Gas/H2 co-firing','#B59C70')
generation <- iamc %>% filter(Region %in% PSM_WORLD_REGIONS,Year>=2020,Year<=year) %>% inner_join(spec,by='Variable')
tot <- generation %>% group_by(Scenario,Year) %>% summarise(ComponentTotal=sum(Value),.groups='drop') %>%
 left_join(iamc %>% filter(Region %in% PSM_WORLD_REGIONS,Variable=='Sec_Ene_Ele') %>%
  group_by(Scenario,Year) %>% summarise(IAMC_Total=sum(Value),.groups='drop'),by=c('Scenario','Year')) %>%
 mutate(Difference=ComponentTotal-IAMC_Total)
if(any(abs(tot$Difference)>1e-5)) stop('Generation categories do not reconcile to total')
save_generation()
# Map primary demand to the reference panel's final-energy variable interface.
dd <- bind_rows(demands)
dget <- function(s,c) sum(dd$Value[dd$Scenario==s & dd$Carrier %in% c])
synthetic_demand <- bind_rows(lapply(scenarios,function(s) tibble(Scenario=s,Region='World',Year=year,
 Variable=c('Fin_Ene_Hyd','Fin_Ene_Hyd_Amm','Fin_Ene_Liq_Hyd_syn','Fin_Ene_Gas_Hyd_syn'),
 Value=c(dget(s,c('H2','NH3')),dget(s,'NH3'),dget(s,'SYF'),dget(s,'SYM')))))
data <- list(aimt_iamc=synthetic_demand,psm_iamc=iamc,variable_specs=list(),metadata=list(scenario_map=sm))
sankey <- make_fig2_d(data,primary)
audit <- attr(sankey,'fig2_d_balance_audit')
print(audit %>% select(Scenario,ends_with('Difference'),IAMCValidationMismatch),width=Inf)
if(any(audit$IAMCValidationMismatch)) stop('Primary conversion outputs do not reconcile to IAMC (1% tolerance)')
sankey <- sankey+labs(title='World: hydrogen conversion and use, 2050',
 subtitle='Final use: primary model demand; conversion flows: input energy basis',y='Energy flow (EJ/yr)')+
 theme(legend.position='bottom',strip.text=element_text(size=11),axis.title.y=element_text(size=10))
saveplot(sankey,'hydrogen_conversion_WORLD_2050',14*panel_scale,7)
# Node generation follows Fig. 3-a's category aggregation.
gspec <- spec %>% mutate(Category=ifelse(Category %in% c('Coal','Gas','Oil','Geothermal','Hydrogen','Gas/H2 co-firing'),'Other',Category))
gn <- iamc %>% filter(Year==year,Region %in% nodes) %>% inner_join(gspec %>% select(Variable,Category),by='Variable') %>%
 group_by(Scenario,Region,Category) %>% summarise(Value=sum(Value),.groups='drop')
conversion <- function(z,reverse=FALSE) {
 z %>% split_primary_links() %>% mutate(From=if(reverse) Node1 else Node0,To=if(reverse) Node0 else Node1,
 Region=fig3_link_node(From),FromCarrier=sub('^.*-','',From),ToCarrier=sub('^.*-','',To)) %>%
 filter(Region %in% nodes,FromCarrier=='H2',ToCarrier %in% c('NH3','SYF','SYM')) %>%
 mutate(Category=as.character(ifelse(ToCarrier=='NH3','NH3','Synfuel'))) %>% select(Scenario,Region,Category,Value)
}
cn <- bind_rows(conversion(primary$psm_vff_l),conversion(primary$psm_vfb_l,TRUE)) %>%
 group_by(Scenario,Region,Category) %>% summarise(Value=sum(Value),.groups='drop') %>%
 complete(Scenario=scenarios,Region=nodes,Category=c('NH3','Synfuel'),fill=list(Value=0)) %>%
 pivot_wider(names_from=Category,values_from=Value)
hn <- iamc %>% filter(Year==year,Region %in% nodes,Variable=='Sec_Ene_Hyd') %>% select(Scenario,Region,TotalH2=Value) %>%
 full_join(cn,by=c('Scenario','Region')) %>% mutate(TotalH2=replace_na(TotalH2,0),
 Residual=TotalH2-NH3-Synfuel,`Direct H2`=pmax(Residual,0))
node_audit <- hn %>% group_by(Scenario) %>% summarise(
 NodesWithNegativeResidual=sum(Residual < -1e-5),
 ClippedResidualEJ=sum(pmax(-Residual,0)),MinimumResidualEJ=min(Residual),.groups='drop')
print(node_audit,width=Inf)
if(any(hn$Residual < -1e-5)) {
 warning('Some nodes use more H2 for conversion than they produce locally; ',
         'Direct H2 is the non-negative production residual, as in reference Fig. 3-b, not a net-trade-adjusted use balance')
 print(tibble::as_tibble(hn) %>% filter(Residual < -1e-5) %>% arrange(Residual) %>% head(10),width=Inf)
}
hn <- hn %>% select(Scenario,Region,`Direct H2`,NH3,Synfuel) %>% pivot_longer(-c(Scenario,Region),names_to='Category',values_to='Value')
flows <- bind_rows(lapply(scenarios,function(s) fig3_gross_links(primary,s,'.*-AC_.*-AC$',geo$nodes,'node',
 drop_dateline_crossing=FALSE) %>% filter(Point0!=Point1,FlowEJ>1e-6) %>% mutate(Scenario=s,Carrier='Electricity')))
electric_paths <- make_carrier_transport_paths(flows,'Electricity')
map_pair <- function(vals,colors,title,name,transmission=FALSE) {
 vals <- vals %>% left_join(geo$nodes,by=c('Region'='Node')) %>% mutate(Order=match(Category,names(colors)))
 maxval <- max(vals %>% group_by(Scenario,Region) %>% summarise(Total=sum(Value),.groups='drop') %>% pull(Total))
 maxflow <- max(c(flows$FlowEJ,0.01)); radius <- 5.5
 plots <- lapply(scenarios,function(s) {
  p <- ggplot()+geom_sf(data=geo$basemap,fill='grey97',color='grey75',linewidth=.2)+
   coord_sf(xlim=c(-180,180),ylim=c(-60,85),expand=FALSE)+theme_void(base_size=11)
  if(transmission) p <- p+geom_path(data=filter(electric_paths,Scenario==s),
   aes(x=lon,y=lat,group=PathPiece,linewidth=FlowEJ),color='grey30',alpha=.5)+
   scale_linewidth_continuous(limits=c(0,maxflow),range=c(.15,2.5),trans='sqrt',name='Gross transmission (EJ/yr)')
  pies <- fig3_pie_layer(filter(vals,Scenario==s,Value>1e-7),maxval,radius)
  pies$show.legend <- c(fill=TRUE,colour=FALSE,linewidth=FALSE,size=FALSE)
  p <- p+geom_point(data=geo$nodes,aes(lon,lat),size=.6,color='grey55')+
   pies+
   scale_fill_manual(values=colors,limits=names(colors),drop=FALSE,name=NULL)+
   labs(title=s)+
   theme(plot.title=element_text(hjust=.5,face='bold'),legend.position='bottom',
         plot.caption=element_text(size=8,hjust=.5))
  # Physical size guide uses the identical mm radius as the pie layer.
  breaks <- pretty(c(0,maxval),n=3);breaks <- breaks[breaks>0 & breaks<=maxval]
  guide <- tibble(lon=seq(-160,by=23,length.out=length(breaks)),lat=-42,Value=breaks)
  p+geom_point(data=guide,aes(lon,lat,size=Value),shape=21,fill='white',show.legend=FALSE)+
   scale_size_area(limits=c(0,maxval),max_size=2*radius,guide='none')+
   geom_text(data=guide,aes(lon,lat,label=sprintf('%.1f',Value)),nudge_y=-9,size=3)+
   annotate('text',x=-172,y=-25,label='Node scale (EJ/yr)',hjust=0,size=3)
 })
 p <- plot_grid(plotlist=plots,ncol=1)
 p <- plot_grid(ggdraw()+draw_label(title,fontface='bold',size=14),p,ncol=1,rel_heights=c(.07,1))
 saveplot(p,name,16,17)
}
map_pair(gn,FIG3_GENERATION_COLORS,'World: node generation and electricity transmission, 2050','node_generation_transmission_WORLD_2050',TRUE)
# Use the actual Fig. 3-b renderer, including its opposite sea-trade curvatures
# and directional arrows. Probe its data to fix identical scales for both panels.
hydrogen_maps <- lapply(scenarios,function(s) make_fig3_b(data,primary,geo,s))
node_max <- max(vapply(hydrogen_maps,function(p) {
 z <- attr(p,'fig3_b_nodes')
 max(z %>% group_by(Region) %>% summarise(Total=sum(Value),.groups='drop') %>% pull(Total))
},numeric(1)))
flow_max <- max(unlist(lapply(hydrogen_maps,function(p)
 lapply(attr(p,'fig3_b_transport'),function(z) z$FlowEJ))))
hydrogen_maps <- lapply(scenarios,function(s) {
 p <- make_fig3_b(data,primary,geo,s,node_max=node_max,flow_max=flow_max)
 transport <- attr(p,'fig3_b_transport')
 print(bind_rows(lapply(names(transport),function(k) tibble(
  Scenario=s,Transport=k,Links=nrow(transport[[k]]),FlowEJ=sum(transport[[k]]$FlowEJ)))))
 p+labs(title=s,caption=paste0(
  'Arrows: directional sea trade; H2 pipelines / NH3 road links: gross flow. Links < 0.01 EJ/yr omitted.\n',
  'Direct H2 = max(local H2 production - conversion inputs, 0); excludes net trade.'))+
  theme(plot.title=element_text(hjust=.5,face='bold',size=14),
        plot.caption=element_text(size=8,hjust=.5))
})
for (i in seq_along(scenarios)) {
 scenario_map <- hydrogen_maps[[i]]+labs(
  title=paste0('World: hydrogen allocation and carrier transport, ',year,'\n',scenarios[[i]]))
 saveplot(scenario_map,paste0('node_hydrogen_conversion_WORLD_',year,'_',scenarios[[i]]),16,9)
}
hydrogen_map <- plot_grid(ggdraw()+draw_label(
 'World: hydrogen allocation and carrier transport, 2050 (pies: H2 input basis)',fontface='bold',size=14),
 plot_grid(plotlist=hydrogen_maps,ncol=1),ncol=1,rel_heights=c(.07,1))
saveplot(hydrogen_map,'node_hydrogen_conversion_WORLD_2050',16,17)
summary <- iamc %>% filter(Region %in% PSM_WORLD_REGIONS,Year==year,Variable %in% c('Sec_Ene_Ele','Sec_Ene_Hyd_Ele','Sec_Ene_Hyd_Amm')) %>%
 group_by(Scenario,Variable) %>% summarise(Value=sum(Value),.groups='drop')
summary <- summary %>%
 left_join(summary %>% filter(Scenario=='500C') %>% select(Variable,Baseline=Value),by='Variable') %>%
 mutate(Difference=Value-Baseline,Percent=ifelse(Baseline==0,NA_real_,100*Difference/Baseline),
        Scenario=factor(Scenario,levels=scenarios)) %>% arrange(Variable,Scenario)
print(tibble::as_tibble(summary),width=Inf); message('Saved plots to ',out)
