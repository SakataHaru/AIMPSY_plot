# Run from any directory: Rscript H2demand/NH3_demand/code/plot.R
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(ggplot2); library(gdxrrw); library(sf); library(cowplot)})
script <- sub('^--file=', '', commandArgs()[grepl('^--file=', commandArgs())][1])
base <- dirname(dirname(normalizePath(script, winslash='/')))
repo <- dirname(dirname(base)); ref <- file.path(repo,'IntermReport/code')
out <- file.path(base,'output'); dir.create(out, recursive=TRUE, showWarnings=FALSE)
source(file.path(ref,'plot_common.R')); source(file.path(ref,'data_import.R'))
source(file.path(base,'code/fig3_common.R'))
scenarios <- c('500C','500C-NH3_R272'); sm <- setNames(scenarios,scenarios)
igdx('C:/GAMS/win64/26.1')
iamc <- read_iamc_gdx(file.path(base,'data/main'),'PSM') %>% filter(Scenario %in% scenarios)
source(file.path(base,'code/generation.R'))
source(file.path(base,'code/fig1_d.R'))
source(file.path(base,'code/fig2_d.R'))
save_generation <- function() {
 save_png_mm(make_china_generation(iamc,sm,ref),
             file.path(out,'generation_mix_CHN_2020_2040.png'),
             width_mm=355.6,height_mm=152.4,dpi=240)
}
if ('--generation-only' %in% commandArgs(trailingOnly=TRUE)) {
 save_generation()
 message('Saved China generation PNG')
 quit(save='no')
}
geo <- load_fig3_geodata(file.path(repo,'IntermReport/data'))
rmapping <- read.csv(file.path(repo,'IntermReport/data/M_R272.csv'))
geo$nodes <- geo$nodes %>% filter(Node %in% rmapping$N_IND[rmapping$R31=='CHN'])
nodes <- geo$nodes$Node
primary <- list(psm_vfs_l=tibble(),psm_vff_l=tibble(),psm_vfb_l=tibble(),
 psm_region_map=transmute(rmapping,Node=N_IND,Region=R31),metadata=list(year=2040,scenario_map=sm))
demands <- list(); weights <- list()
for (s in scenarios) {
 path <- file.path(base,'data/gdx_primary',s,'2040.gdx')
 fw <- rgdx.param(path,'vff_l'); bw <- rgdx.param(path,'vfb_l')
 times <- unique(as.character(fw$T)); nt <- sum(grepl('^d[0-9]+t[0-9]+$',times))
 # Standard full-year hourly blocks: 365 days x 4 blocks => 6 hours/block.
 if(nt != 1460L) stop('Unexpected time grid: ',nt,'; verify annual weighting before plotting')
 w <- 6; weights[[s]] <- w
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
spec <- tibble::tribble(~Variable,~Category,~Color,
 'Sec_Ene_Ele_Coa','Coal','grey45','Sec_Ene_Ele_Gas','Gas','#D8BC75',
 'Sec_Ene_Ele_Oil','Oil','sandybrown','Sec_Ene_Ele_Nuc','Nuclear','moccasin',
 'Sec_Ene_Ele_Hyp','Hydro','#2C7FB8','Sec_Ene_Ele_Bio','Biomass','#A9D65D',
 'Sec_Ene_Ele_Geo','Geothermal','peru','Sec_Ene_Ele_Solar','Solar','lightsalmon',
 'Sec_Ene_Ele_Win','Wind','lightskyblue3','Sec_Ene_Ele_Hyd','Hydrogen','thistle2',
 'Sec_Ene_Ele_Gas_Hyd_Cof','Gas/H2 co-firing','#B59C70')
generation <- iamc %>% filter(Region=='CHN',Year>=2020,Year<=2040) %>% inner_join(spec,by='Variable')
tot <- generation %>% group_by(Scenario,Year) %>% summarise(ComponentTotal=sum(Value),.groups='drop') %>%
 left_join(iamc %>% filter(Region=='CHN',Variable=='Sec_Ene_Ele') %>% select(Scenario,Year,IAMC_Total=Value),by=c('Scenario','Year')) %>%
 mutate(Difference=ComponentTotal-IAMC_Total)
if(any(abs(tot$Difference)>1e-5)) stop('Generation categories do not reconcile to total')
save_generation()
# Map primary demand to the reference panel's final-energy variable interface.
dd <- bind_rows(demands)
dget <- function(s,c) sum(dd$Value[dd$Scenario==s & dd$Carrier %in% c])
synthetic_demand <- bind_rows(lapply(scenarios,function(s) tibble(Scenario=s,Region='CHN',Year=2040L,
 Variable=c('Fin_Ene_Hyd','Fin_Ene_Hyd_Amm','Fin_Ene_Liq_Hyd_syn','Fin_Ene_Gas_Hyd_syn'),
 Value=c(dget(s,c('H2','NH3')),dget(s,'NH3'),dget(s,'SYF'),dget(s,'SYM')))))
data <- list(aimt_iamc=synthetic_demand,psm_iamc=iamc,variable_specs=list(),metadata=list(scenario_map=sm))
PSM_WORLD_REGIONS <- 'CHN'
sankey <- make_fig2_d(data,primary)+labs(title='China: hydrogen conversion and use, 2040',
 subtitle='Final use: primary model demand; conversion flows: input energy basis',y='Energy flow (EJ/yr)')+
 theme(legend.position='bottom',strip.text=element_text(size=11),axis.title.y=element_text(size=10))
saveplot(sankey,'hydrogen_conversion_CHN_2040',14,7)
# Node generation follows Fig. 3-a's category aggregation.
gspec <- spec %>% mutate(Category=ifelse(Category %in% c('Coal','Gas','Oil','Geothermal','Hydrogen','Gas/H2 co-firing'),'Other',Category))
gn <- iamc %>% filter(Year==2040,Region %in% nodes) %>% inner_join(gspec %>% select(Variable,Category),by='Variable') %>%
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
hn <- iamc %>% filter(Year==2040,Region %in% nodes,Variable=='Sec_Ene_Hyd') %>% select(Scenario,Region,TotalH2=Value) %>%
 full_join(cn,by=c('Scenario','Region')) %>% mutate(TotalH2=replace_na(TotalH2,0),
 Residual=TotalH2-NH3-Synfuel,`Direct H2`=pmax(Residual,0))
if(any(hn$Residual < -1e-5)) warning('Conversion inputs exceed hydrogen production at some nodes')
hn <- hn %>% select(Scenario,Region,`Direct H2`,NH3,Synfuel) %>% pivot_longer(-c(Scenario,Region),names_to='Category',values_to='Value')
flows <- bind_rows(lapply(scenarios,function(s) fig3_gross_links(primary,s,'.*-AC_.*-AC$',geo$nodes,'node') %>% mutate(Scenario=s)))
# Same gross forward + backward treatment as the reference Fig. 3-b H2 network.
fuel_coordinates <- bind_rows(geo$nodes, geo$regions %>% filter(Region=='CHN') %>%
                              transmute(Node=paste0('R31-',Region),lon,lat))
fuel_patterns <- c(H2='.*-(H2|H2TRD)_.*-(H2|H2TRD)$',
 NH3='.*-(NH3|NH3TRD)_.*-(NH3|NH3TRD)$',
 Synfuel='.*-(SYF|SYFTRD|SYM|SYMTRD)_.*-(SYF|SYFTRD|SYM|SYMTRD)$')
fuel_flows <- bind_rows(lapply(scenarios,function(s) bind_rows(lapply(names(fuel_patterns),function(carrier) {
 fig3_gross_links(primary,s,fuel_patterns[[carrier]],fuel_coordinates,'node') %>%
  filter(Point0!=Point1,FlowEJ>=0.01) %>% mutate(Scenario=s,Carrier=carrier)
}))))
print(fuel_flows %>% group_by(Scenario,Carrier) %>% summarise(Links=n(),GrossFlowEJ=sum(FlowEJ),.groups='drop'))
map_pair <- function(vals,colors,title,name,transmission=FALSE,fuel_transport=FALSE) {
 vals <- vals %>% left_join(geo$nodes,by=c('Region'='Node')) %>% mutate(Order=match(Category,names(colors)))
 maxval <- max(vals %>% group_by(Scenario,Region) %>% summarise(Total=sum(Value),.groups='drop') %>% pull(Total))
 maxflow <- max(flows$FlowEJ); radius <- 5.5
 plots <- lapply(scenarios,function(s) {
  p <- ggplot()+geom_sf(data=geo$basemap,fill='grey97',color='grey75',linewidth=.2)+
   coord_sf(xlim=c(72,136),ylim=c(17,55),expand=FALSE)+theme_void(base_size=11)
  if(transmission) p <- p+geom_curve(data=filter(flows,Scenario==s,FlowEJ>1e-6),
   aes(x=lon0,y=lat0,xend=lon1,yend=lat1,linewidth=FlowEJ),curvature=.08,color='grey30',alpha=.5)+
   scale_linewidth_continuous(limits=c(0,maxflow),range=c(.15,2.5),trans='sqrt',name='Gross transmission (EJ/yr)')
  if(fuel_transport && nrow(fuel_flows)>0) {
   links <- filter(fuel_flows,Scenario==s)
   transport_colors <- c(H2=unname(FIG3_H2_NODE_COLORS[['Direct H2']]),
                         NH3=unname(FIG3_H2_NODE_COLORS[['NH3']]),Synfuel=unname(CARRIER_COLORS[['Synfuel']]))
   p <- p+geom_curve(data=links,aes(x=lon0,y=lat0,xend=lon1,yend=lat1),
                    curvature=.08,color='white',linewidth=1,show.legend=FALSE)+
    geom_curve(data=links,aes(x=lon0,y=lat0,xend=lon1,yend=lat1,linewidth=FlowEJ,color=Carrier),
               curvature=.08,alpha=.9,lineend='round')+
    scale_color_manual(values=transport_colors,name='Transport carrier')+
    scale_linewidth_continuous(limits=c(0,max(fuel_flows$FlowEJ)),range=c(.2,2.8),trans='sqrt',
                              name='Gross transport (EJ/yr)')+
    guides(color=guide_legend(override.aes=list(linewidth=1.1),order=1,nrow=1),
           linewidth=guide_legend(order=2,nrow=1),fill=guide_legend(order=3,nrow=1))+
    theme(legend.box='vertical')
  }
  pies <- fig3_pie_layer(filter(vals,Scenario==s,Value>1e-7),maxval,radius)
  pies$show.legend <- c(fill=TRUE,colour=FALSE,linewidth=FALSE,size=FALSE)
  p <- p+geom_point(data=geo$nodes,aes(lon,lat),size=.6,color='grey55')+
   pies+
   scale_fill_manual(values=colors,limits=names(colors),drop=FALSE,name=NULL)+
   geom_text(data=geo$nodes,aes(lon,lat,label=sub('CHN-','',Node)),nudge_y=-.8,size=2.5)+
   labs(title=s)+theme(plot.title=element_text(hjust=.5,face='bold'),legend.position='bottom')
  # Physical size guide uses the identical mm radius as the pie layer.
  breaks <- pretty(c(0,maxval),n=3);breaks <- breaks[breaks>0 & breaks<=maxval]
  guide <- tibble(lon=seq(78,by=10,length.out=length(breaks)),lat=19.5,Value=breaks)
  p+geom_point(data=guide,aes(lon,lat,size=Value),shape=21,fill='white',show.legend=FALSE)+
   scale_size_area(limits=c(0,maxval),max_size=2*radius,guide='none')+
   geom_text(data=guide,aes(lon,lat,label=sprintf('%.1f',Value)),nudge_y=-1.4,size=3)+
   annotate('text',x=74,y=23,label='EJ/yr',hjust=0,size=3)
 })
 p <- plot_grid(plotlist=plots,nrow=1)
 p <- plot_grid(ggdraw()+draw_label(title,fontface='bold',size=14),p,ncol=1,rel_heights=c(.07,1))
 saveplot(p,name,16,7.5)
}
map_pair(gn,FIG3_GENERATION_COLORS,'China: node generation and electricity transmission, 2040','node_generation_transmission_CHN_2040',TRUE)
map_pair(hn,FIG3_H2_NODE_COLORS,'China: hydrogen allocation and carrier transport, 2040 (pies: H2 input basis)','node_hydrogen_conversion_CHN_2040',fuel_transport=TRUE)
summary <- iamc %>% filter(Region=='CHN',Year==2040,Variable %in% c('Sec_Ene_Ele','Sec_Ene_Hyd_Ele','Sec_Ene_Hyd_Amm')) %>%
 select(Scenario,Variable,Value) %>% pivot_wider(names_from=Scenario,values_from=Value) %>%
 mutate(Difference=`500C-NH3_R272`-`500C`,Percent=100*Difference/`500C`)
print(summary); message('Saved plots to ',out)
