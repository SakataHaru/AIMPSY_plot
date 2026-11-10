# PLOT SETTINGS ---------------------------------------------------------------
PSM_SCENARIO <- "H2_storage_compressor"
PSM_RUNS <- data.frame(Region=c("Asia","NorthAmerica","Europe"),
  Date=c("2610301310","2610291832","2610281735"), MainYear=c(2045L,2045L,2030L))
target_scenarios <- c("500C","500C-H2storage","500C-H2sto_only_tank","500C-H2sto_only_cavern")
scenario_labels <- c("No storage","Tank + cavern","Tank only","Cavern only")
timeresol <- "6HOUR"
first_year <- 2020L

# PACKAGES, PATHS AND DISPLAY SETTINGS ----------------------------------------
.libPaths(c("C:/Users/Haru Sakata/AppData/Local/R/win-library/4.4",.libPaths()))
suppressPackageStartupMessages({library(gdxrrw);library(dplyr);library(tidyr)
  library(ggplot2);library(cowplot)})
gdxrrw::igdx("C:/GAMS/win64/26.1")
script<-sub("^--file=","",commandArgs()[grepl("^--file=",commandArgs())])[1]
project<-dirname(dirname(normalizePath(script,winslash="/")))
workspace<-normalizePath(file.path(project,"../../../.."),winslash="/")
source_root<-"//SY_3600_01/atfs02/Individual/hsakata/AIMPSY_2nd/output"
mapping<-read.csv(file.path(workspace,"AIMPSY_plot/IntermReport/data/M_R272.csv"))
region_groups<-list(Asia=c("CHN","IDN","IND","JPN","KOR","MYS","THA","VNM","XEA","XSA","XSE"),
  NorthAmerica=c("USA","CAN","MEX"),Europe=c("XE15","XE10","XE3","XEWI"))
timeresol<-toupper(trimws(timeresol))
time_weights<-c("12DAY"=ceiling(365/12*1000)/1000,"6HOUR"=6,"4HOUR"=4,"3HOUR"=3,"1HOUR"=1)
stopifnot(timeresol %in% names(time_weights),length(target_scenarios)==length(scenario_labels),
  !anyDuplicated(target_scenarios),all(nzchar(target_scenarios)))
primary_weight<-unname(time_weights[timeresol])
case<-function(x)factor(scenario_labels[match(x,target_scenarios)],levels=scenario_labels)
case_colors<-setNames(c("#666666","#009E73","#0072B2","#E69F00"),scenario_labels)
case_lines<-setNames(c("solid","solid","dashed","dotdash"),scenario_labels)
technologies<-c("Solar PV","Wind","Electrolyzer","Battery power")
source(file.path(project,"prog/generation_style.R"))
theme_set(theme_minimal(base_size=11)+theme(panel.grid.minor=element_blank(),legend.position="bottom",
  plot.title=element_text(face="bold"),strip.text=element_text(face="bold"),
  plot.caption=element_text(hjust=0,size=8),plot.background=element_rect(fill="white",color=NA)))

# BOUNDED SYMBOL READS AND MODEL-YEAR CHECKS -----------------------------------
readpar<-function(file,symbol) {
  x<-rgdx.param(file,symbol);x[]<-lapply(x,function(z)if(is.factor(z))as.character(z)else z)
  names(x)[ncol(x)]<-"Value";stopifnot(all(is.finite(x$Value)));x
}
filtered<-function(file,symbol,uels) {
  stopifnot(all(lengths(uels)>0))
  x<-rgdx(file,list(name=symbol,uels=uels))
  z<-as.data.frame(lapply(seq_len(x$dim),function(i)x$uels[[i]][x$val[,i]]),stringsAsFactors=FALSE)
  names(z)<-x$domains;z$Value<-x$val[,x$dim+1];stopifnot(all(is.finite(z$Value)));z
}
check_year<-function(file) {
  con<-file(file,"r");on.exit(close(con));ms<-ss<-character()
  repeat {
    x<-readLines(con,n=2000,warn=FALSE);if(!length(x))break
    ms<-c(ms,grep("^\\*\\*\\*\\* MODEL STATUS",x,value=TRUE));ss<-c(ss,grep("^\\*\\*\\*\\* SOLVER STATUS",x,value=TRUE))
    if(length(ms)&&length(ss))break
  }
  stopifnot(length(ms)==1,length(ss)==1,grepl("STATUS\\s+1\\s+Optimal",ms),grepl("STATUS\\s+1\\s+Normal",ss))
}
capacity_values<-function(g,l) c(
  sum(g$Value[g$I=="AC"&grepl("^solar",g$R)]),
  sum(g$Value[g$I=="AC"&grepl("^wind",g$R)]),
  sum(l$Value[grepl("-AC_.*-H2$",l$L)]),sum(l$Value[grepl("-AC_.*-DC$",l$L)]))

# CAPACITY HISTORY AND MAIN-YEAR SOLAR ACCOUNTING -----------------------------
for(row in seq_len(nrow(PSM_RUNS))) {
  area<-PSM_RUNS$Region[row];token<-PSM_RUNS$Date[row];year<-PSM_RUNS$MainYear[row]
  years<-seq(first_year,year,5L);nodes<-mapping$N_IND[mapping$R31 %in% region_groups[[area]]]
  run<-file.path(source_root,PSM_SCENARIO,area,token)
  output<-file.path(project,"output",paste0(area,"_",token));dir.create(output,recursive=TRUE,showWarnings=FALSE)
  message("REGION ",area," | ",year)
  history<-generation<-solar_accounting<-node_solar<-response<-list();idx<-0L
  for(s in target_scenarios) {
    for(y in years) {
      primary<-file.path(run,"gams_output/gdx_primary",s,paste0(y,".gdx"))
      check_year(file.path(run,"gams_output/log",s,paste0(y,".lst")))
      g<-readpar(primary,"vgs_l");l<-readpar(primary,"vfs_l")
      stopifnot(all(g$N[g$I %in% c("AC","AC2")] %in% nodes))
      idx<-idx+1L
      history[[idx]]<-tibble(Scenario=s,Case=case(s),Year=y,Technology=technologies,Value=capacity_values(g,l))
      if(y!=year)next
      inp<-file.path(run,"data/input",paste0(s,".gdx"))
      time<-readpar(inp,"wt");times<-time$T[grepl("^d[0-9]{3}t",time$T)]
      stopifnot(length(times)==365*24/primary_weight,all(time$Value[time$T %in% times]==primary_weight))
      ac_tech<-unique(g$R[g$I=="AC"])
      vg<-filtered(primary,"vg_l",list(nodes,"AC",ac_tech,times))
      generation[[s]]<-vg %>% mutate(Technology=genclass(R),Value=Value*primary_weight/1000) %>%
        group_by(Technology) %>% summarise(Value=sum(Value),.groups="drop") %>% mutate(Scenario=s,Case=case(s))
      response[[s]]<-bind_rows(history[[idx]],tibble(Scenario=s,Case=case(s),Year=y,Technology="BECCS",Value=sum(g$Value[g$I=="AC"&g$R=="biomass_ccs"])))
      pvtech<-unique(g$R[g$I=="AC"&grepl("^solar",g$R)])
      cf<-filtered(inp,"gmx_t",list(nodes,"AC",pvtech,times,as.character(year)))
      # Exact model upper-envelope generation, not an external annual CF guess.
      available<-cf %>% left_join(g %>% filter(I=="AC",R %in% pvtech) %>% rename(CapacityGW=Value),by=c("N","I","R"))
      stopifnot(!anyNA(available$CapacityGW))
      available<-available %>% mutate(PotentialGWh=Value*CapacityGW*primary_weight)
      actual<-vg %>% filter(R %in% pvtech) %>% group_by(N,I,R) %>% summarise(GenerationGWh=sum(Value)*primary_weight,.groups="drop")
      site<-available %>% group_by(N,I,R) %>% summarise(CapacityGW=first(CapacityGW),
        ResourceCF=sum(Value)*primary_weight/8760,PotentialGWh=sum(PotentialGWh),.groups="drop") %>%
        left_join(actual,by=c("N","I","R")) %>% mutate(GenerationGWh=replace_na(GenerationGWh,0),
          CurtailmentGWh=PotentialGWh-GenerationGWh,Scenario=s)
      stopifnot(all(site$CurtailmentGWh>=-pmax(.01,1e-6*site$PotentialGWh)))
      # Check primary annual P_SOL against time-integrated AC solar dispatch.
      annual<-readpar(primary,"vp_l")
      stopifnot(abs(sum(annual$Value[annual$I=="AC"&annual$K=="P_SOL"])-sum(site$GenerationGWh)/1000)<.01)
      cap<-sum(site$CapacityGW);pot<-sum(site$PotentialGWh)/1000;actual_total<-sum(site$GenerationGWh)/1000
      solar_accounting[[s]]<-tibble(Scenario=s,Case=case(s),CapacityGW=cap,PotentialTWh=pot,GenerationTWh=actual_total,
        CurtailmentTWh=pot-actual_total,ResourceCFPct=pot*1000/(cap*8760)*100,
        ActualCFPct=actual_total*1000/(cap*8760)*100,CurtailmentPct=100*(pot-actual_total)/pot,
        RooftopCapacityGW=sum(g$Value[g$I=="AC2"&grepl("^solar",g$R)]))
      node_solar[[s]]<-site
    }
  }
  history<-bind_rows(history) %>% mutate(Technology=factor(Technology,levels=technologies))
  generation<-bind_rows(generation)
  response<-bind_rows(response) %>% group_by(Technology) %>% mutate(Delta=Value-Value[Scenario==target_scenarios[1]]) %>% ungroup()
  accounting<-bind_rows(solar_accounting)
  stamp<-paste0(area," | ",year," | 6-hour power / daily H2 | Run ",token,
    if(area=="Europe")"\n2035 infeasible; latest successful year is 2030." else "\n2050 infeasible; results through 2045 only.")

  # REVERSED GENERATION STACK: EXISTING COMPARISON FIGURE -----------------------
  pgen<-ggplot(generation,aes(Case,Value,fill=Technology))+geom_col(position=position_stack(reverse=TRUE),width=.65,show.legend=TRUE)+
    scale_fill_manual(values=gencolors,limits=secele$Legend,drop=FALSE)+
    guides(fill=guide_legend(ncol=3,byrow=TRUE))+labs(title="Electricity generation",x=NULL,y="TWh/yr",fill=NULL)
  presponse<-ggplot(filter(response,Scenario!=target_scenarios[1]),aes(Technology,Delta,fill=Case))+
    geom_col(position="dodge")+geom_hline(yintercept=0,linewidth=.3)+scale_fill_manual(values=case_colors)+coord_flip()+
    labs(title="Installed capacity change from no storage",x=NULL,y="Capacity difference (GW)",fill=NULL)
  # Generation mix / capacity response; no shared legend because units differ.
  combined<-plot_grid(pgen+power_response_text_theme(),presponse+power_response_text_theme(),nrow=1,labels=c("a","b"),label_size=16,rel_widths=c(1,1.1))
  combined<-plot_grid(ggdraw()+draw_label(stamp,size=12),combined,ncol=1,rel_heights=c(.16,1))
  ggsave(file.path(output,"06_power_system_response.png"),combined,width=14,height=7.5,units="in",dpi=240,bg="white")

  # FOUR FACETS: ACTUAL INSTALLED CAPACITY AT SOLVED MODEL YEARS ---------------
  trend<-ggplot(history,aes(Year,Value,color=Case,linetype=Case,group=Case))+
    geom_line(linewidth=.8)+geom_point(size=1.8)+facet_wrap(~Technology,ncol=2,scales="free_y")+
    scale_color_manual(values=case_colors,drop=FALSE)+scale_linetype_manual(values=case_lines,drop=FALSE)+
    scale_x_continuous(breaks=years)+scale_y_continuous(limits=c(0,NA),expand=expansion(mult=c(0,.08)))+
    labs(title=paste(area,"| installed capacity by scenario"),subtitle="Solar PV / wind / electrolyzer / battery power | facet-specific vertical scales",
      x="Model year",y="Installed capacity (GW)",color=NULL,linetype=NULL,
      caption=paste(stamp,"Solar/wind: AC-bus generation. Electrolyzer: electricity input. Battery: grid inverter power.\nLines connect solved five-year snapshots; intermediate years were not solved.",sep="\n"))+
    guides(color=guide_legend(nrow=1),linetype=guide_legend(nrow=1))
  ggsave(file.path(output,"10_capacity_evolution_facets.png"),trend,width=12,height=8,units="in",dpi=240,bg="white")

  # SOLAR CAPACITY / OUTPUT / CURTAILMENT, WITH EXPLICITLY DISTINCT UNITS --------
  pvplot<-accounting %>% select(Case,CapacityGW,GenerationTWh,CurtailmentTWh,ActualCFPct) %>%
    pivot_longer(-Case,names_to="Metric",values_to="Value") %>%
    mutate(Metric=factor(Metric,levels=c("CapacityGW","GenerationTWh","CurtailmentTWh","ActualCFPct"),
      labels=c("Installed capacity (GW)","Actual generation (TWh/yr)","Unutilized potential (TWh/yr)","Actual capacity factor (%)")))
  ppv<-ggplot(pvplot,aes(Case,Value,fill=Case))+geom_col(width=.65)+facet_wrap(~Metric,ncol=2,scales="free_y")+
    scale_fill_manual(values=case_colors)+labs(title=paste(area,"| why PV capacity and output differ"),
      subtitle=paste("Main year:",year,"| AC-bus solar PV only"),x=NULL,y=NULL,fill=NULL,
      caption=paste(stamp,"Unutilized potential = sum(model capacity-factor upper bound x installed capacity x hours) minus actual generation.",sep="\n"))+
    theme(legend.position="none",axis.text.x=element_text(angle=15,hjust=1))
  ggsave(file.path(output,"11_solar_capacity_and_utilization.png"),ppv,width=12,height=8,units="in",dpi=240,bg="white")

  # ACCOUNTING DECOMPOSITION AND COUNTRY SHIFTS: CONSOLE ONLY -------------------
  base<-accounting %>% filter(Scenario==target_scenarios[1]);on<-accounting %>% filter(Scenario==target_scenarios[2])
  cap_effect<-(on$CapacityGW-base$CapacityGW)*base$ResourceCFPct/100*8760/1000
  resource_effect<-on$CapacityGW*(on$ResourceCFPct-base$ResourceCFPct)/100*8760/1000
  curtail_effect<-base$CurtailmentTWh-on$CurtailmentTWh
  stopifnot(abs(cap_effect+resource_effect+curtail_effect-(on$GenerationTWh-base$GenerationTWh))<1e-6)
  message("SOLAR ACCOUNTING ",area);print(as.data.frame(accounting),digits=8,row.names=FALSE)
  message("ON-OFF solar output decomposition TWh: capacity=",cap_effect," resource-composition=",resource_effect," less-curtailment=",curtail_effect)
  country<-bind_rows(node_solar) %>% left_join(mapping %>% select(N=N_IND,Country=R31),by="N") %>%
    group_by(Scenario,Country) %>% summarise(CapacityGW=sum(CapacityGW),GenerationTWh=sum(GenerationGWh)/1000,
      PotentialTWh=sum(PotentialGWh)/1000,.groups="drop")
  message("COUNTRY PV CHANGES ",area)
  print(country %>% filter(Scenario %in% target_scenarios[1:2]) %>% pivot_wider(names_from=Scenario,values_from=c(CapacityGW,GenerationTWh,PotentialTWh)),width=Inf,n=Inf)
  message("CAPACITY HISTORY ",area);print(history,width=Inf,n=Inf)
  message("Finished ",area,": updated 06 and added 10 / 11.")
}
