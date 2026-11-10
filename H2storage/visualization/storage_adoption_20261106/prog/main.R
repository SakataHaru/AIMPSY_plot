# PLOT SETTINGS ---------------------------------------------------------------
PSM_SCENARIO <- "H2_storage_compressor"
PSM_RUNS <- data.frame(Region=c("Asia","NorthAmerica","Europe"),
  Date=c("2610301310","2610291832","2610281735"), MainYear=c(2045L,2045L,2030L))
target_scenarios <- c("500C","500C-H2storage","500C-H2sto_only_tank","500C-H2sto_only_cavern")
scenario_labels <- c("No storage","Tank + cavern","Tank only","Cavern only")
timeresol <- "6HOUR"
first_year <- 2020L

# PACKAGES AND DERIVED PATHS --------------------------------------------------
.libPaths(c("C:/Users/Haru Sakata/AppData/Local/R/win-library/4.4", .libPaths()))
suppressPackageStartupMessages({library(gdxrrw);library(dplyr);library(tidyr)
  library(ggplot2);library(cowplot);library(sf);library(scales)})
gdxrrw::igdx("C:/GAMS/win64/26.1")
script <- sub("^--file=", "", commandArgs()[grepl("^--file=",commandArgs())])[1]
project <- dirname(dirname(normalizePath(script,winslash="/")))
workspace <- normalizePath(file.path(project,"../../../.."),winslash="/")
source(file.path(project,"prog/generation_style.R"))
outroot <- file.path(project,"output")
source_root <- "//SY_3600_01/atfs02/Individual/hsakata/AIMPSY_2nd/output"
timeresol <- toupper(trimws(timeresol))
time_weights <- c("12DAY"=ceiling(365/12*1000)/1000,"6HOUR"=6,"4HOUR"=4,"3HOUR"=3,"1HOUR"=1)
stopifnot(timeresol %in% names(time_weights),length(target_scenarios)==length(scenario_labels),
  !anyDuplicated(target_scenarios),all(nzchar(target_scenarios)))
primary_weight <- unname(time_weights[timeresol])
region_arg<-sub("^--region=","",grep("^--region=",commandArgs(trailingOnly=TRUE),value=TRUE))
active_rows<-if(length(region_arg)) which(PSM_RUNS$Region %in% region_arg) else seq_len(nrow(PSM_RUNS))
stopifnot(length(active_rows)>0)
case <- function(x) factor(scenario_labels[match(x,target_scenarios)],levels=scenario_labels)
on_scenario <- target_scenarios[2]
stores <- c("h2_tank","h2_cavern")
store_names <- c(h2_tank="Ground tank",h2_cavern="Salt cavern")
store_colors <- c("Ground tank"="#0072B2","Salt cavern"="#E69F00")
case_colors <- setNames(c("#666666","#009E73","#0072B2","#E69F00"),scenario_labels)
mapping <- read.csv(file.path(workspace,"AIMPSY_plot/IntermReport/data/M_R272.csv"))
geography <- sf::st_read(file.path(workspace,"AIMPSY_plot/IntermReport/data/nodes_centroid.geojson"),quiet=TRUE)
region_groups <- list(Asia=c("CHN","IDN","IND","JPN","KOR","MYS","THA","VNM","XEA","XSA","XSE"),
  NorthAmerica=c("USA","CAN","MEX"),Europe=c("XE15","XE10","XE3","XEWI"))
theme_set(theme_minimal(base_size=11)+theme(panel.grid.minor=element_blank(),
  legend.position="bottom",plot.title=element_text(face="bold"),
  strip.text=element_text(face="bold"),plot.caption=element_text(size=8,hjust=0),
  plot.background=element_rect(fill="white",color=NA)))

# GDX READERS AND YEAR-LEVEL STATUS CHECKS ------------------------------------
readpar <- function(file,symbol) {
  x <- rgdx.param(file,symbol)
  x[] <- lapply(x,function(v) if(is.factor(v)) as.character(v) else v)
  names(x)[ncol(x)] <- "Value"
  stopifnot(all(is.finite(x$Value)))
  x
}
filtered <- function(file,symbol,uels) {
  x <- rgdx(file,list(name=symbol,uels=uels))
  z <- as.data.frame(lapply(seq_len(x$dim),function(i) x$uels[[i]][x$val[,i]]),stringsAsFactors=FALSE)
  names(z) <- x$domains
  z$Value <- x$val[,x$dim+1]
  stopifnot(all(is.finite(z$Value)))
  z
}
year_status <- function(file) {
  con <- file(file,"r");on.exit(close(con))
  ms <- ss <- character()
  repeat {
    x <- readLines(con,n=2000,warn=FALSE)
    if(!length(x))break
    ms <- c(ms,grep("^\\*\\*\\*\\* MODEL STATUS",x,value=TRUE))
    ss <- c(ss,grep("^\\*\\*\\*\\* SOLVER STATUS",x,value=TRUE))
    if(length(ms)&&length(ss))break
  }
  list(ok=length(ms)==1&&length(ss)==1&&grepl("STATUS\\s+1\\s+Optimal",ms)&&
    grepl("STATUS\\s+1\\s+Normal",ss),model=ms,solver=ss)
}
near <- function(a,b,atol=1e-5,rtol=1e-7) all(abs(a-b)<=pmax(atol,rtol*pmax(abs(a),abs(b))))
iamc_spec <- function(dat,variables,years,regions) {
  z <- dat %>% filter(Region %in% regions,Year %in% years,Variable %in% variables) %>%
    group_by(Scenario,Year,Variable) %>% summarise(Value=sum(Value),.groups="drop")
  crossing(Scenario=target_scenarios,Year=years,Variable=variables) %>%
    left_join(z,by=c("Scenario","Year","Variable")) %>% mutate(Value=replace_na(Value,0),Case=case(Scenario))
}
read_daily <- function(file,cap,wt) {
  stopifnot(as.numeric(rgdx(file,list(name="h2_time_version"))$val)==4)
  stopifnot(all(wt$Value[grepl("^d[0-9]{3}$",wt$T)]==1),
    all(wt$Value[grepl("^d[0-9]{3}t",wt$T)]==primary_weight))
  un <- list(unique(cap$N),unique(cap$I),stores,wt$T)
  params <- list(Inventory="ve_l",NetWithdrawal="vh_l",Charge="h2_stg_charge_l",Discharge="h2_stg_discharge_l")
  csmall <- cap %>% filter(S %in% stores) %>% rename(Capacity=Value)
  daily <- crossing(csmall,Day=1:365)
  for(nm in names(params)) {
    z <- filtered(file,params[[nm]],un)
    tt <- names(z)[4]
    stopifnot(all(grepl("^d[0-9]{3}$",z[[tt]])))
    z$Day <- as.integer(sub("d","",z[[tt]]));z[[tt]]<-NULL
    stopifnot(!anyDuplicated(z[c("N","I","S","Day")]))
    orphan <- anti_join(z,csmall,by=c("N","I","S"))
    stopifnot(all(abs(orphan$Value)<1e-5))
    names(z)[names(z)=="Value"]<-nm
    daily <- left_join(daily,z,by=c("N","I","S","Day"))
    daily[[nm]]<-replace_na(daily[[nm]],0)
  }
  eta <- readpar(file,"eeta") %>% rename(Retention=Value)
  daily <- daily %>% left_join(eta,by=c("N","I","S")) %>% group_by(N,I,S) %>%
    arrange(Day,.by_group=TRUE) %>% mutate(Prev=lag(Inventory,default=last(Inventory)),
      Residual=Inventory-Retention^24*Prev+NetWithdrawal,Tolerance=pmax(1e-4,1e-6*Capacity)) %>% ungroup()
  stopifnot(!anyNA(daily$Retention),all(daily$Inventory>=-daily$Tolerance),
    all(daily$Inventory<=daily$Capacity+daily$Tolerance),all(abs(daily$Residual)<=daily$Tolerance),
    all(daily$Charge>=-daily$Tolerance),all(daily$Discharge>=-daily$Tolerance))
  message("Daily stock balance maximum residual GWh: ",max(abs(daily$Residual)))
  daily %>% mutate(Inventory=pmax(Inventory,0),Charge=pmax(Charge,0),Discharge=pmax(Discharge,0))
}

# REGION READS, NATIVE/REPORTING RECONCILIATION AND AGGREGATION ----------------
all_main <- list();all_metrics <- list()
for(row in active_rows) {
  area<-PSM_RUNS$Region[row];token<-PSM_RUNS$Date[row];year<-PSM_RUNS$MainYear[row]
  years<-seq(first_year,year,5L);r31<-region_groups[[area]]
  nodes<-mapping$N_IND[mapping$R31 %in% r31]
  run<-file.path(source_root,PSM_SCENARIO,area,token)
  output<-file.path(outroot,paste0(area,"_",token));dir.create(output,recursive=TRUE,showWarnings=FALSE)
  message("REGION ",area," | main year ",year," | run ",run)
  for(s in target_scenarios)for(y in years) {
    state<-year_status(file.path(run,"gams_output/log",s,paste0(y,".lst")))
    if(!state$ok)stop("Unsuccessful selected model year: ",area," / ",s," / ",y)
  }
  message("Year status passed: ",length(years)*length(target_scenarios)," scenario-years")
  if(area=="Europe")for(s in target_scenarios) {
    state<-year_status(file.path(run,"gams_output/log",s,"2035.lst"))
    stopifnot(!state$ok);message("Excluded Europe 2035: ",state$model)
  }
  reportfile<-file.path(run,"main/merged_output.gdx")
  if(file.exists(reportfile)) {
    raw<-readpar(reportfile,"iamc_gdx")
    iamc<-raw %>% transmute(Scenario=Sc,Region=Sr,Variable=Sv,Year=as.integer(Y5),Value) %>%
      filter(Scenario %in% target_scenarios,Year %in% years)
  } else {
    message("No merged IAMC file; all plot data will come from primary results.")
    iamc<-tibble(Scenario=character(),Region=character(),Variable=character(),Year=integer(),Value=numeric())
  }
  stopifnot(!anyDuplicated(iamc[c("Scenario","Region","Variable","Year")]))
  # The merged reporting GDX contains duplicated scenario results. It is an
  # audit source only; every plotted number below comes from native primary.
  native_rows<-list();nn<-0L
  native_row<-function(z,s,y,var) {
    z %>% left_join(mapping %>% select(N=N_IND,Region=R31),by="N") %>%
      group_by(Region) %>% summarise(Value=sum(Value),.groups="drop") %>%
      mutate(Scenario=s,Year=y,Variable=var)
  }
  for(s in target_scenarios)for(y in years) {
    fp<-file.path(run,"gams_output/gdx_primary",s,paste0(y,".gdx"))
    capy<-readpar(fp,"ves_l")
    cy<-readpar(fp,"vfs_l") %>% filter(grepl("-H2_.*-H2[SC]$",L)) %>%
      mutate(N=sub("-H2_.*$","",L),S=as.character(ifelse(grepl("-H2S$",L),stores[1],stores[2])))
    for(i in seq_along(stores)) {
      nn<-nn+1L;native_rows[[nn]]<-native_row(capy %>% filter(N %in% nodes,S==stores[i]),s,y,c("Cap_Hyd_Sto_Cap_Tan","Cap_Hyd_Sto_Cap_Cav")[i])
      nn<-nn+1L;native_rows[[nn]]<-native_row(cy %>% filter(N %in% nodes,S==stores[i]),s,y,c("Compressor_Tank_GW","Compressor_Cavern_GW")[i])
    }
    if(s!=target_scenarios[1]) {
      for(symbol in c("h2_stg_charge_l","h2_stg_discharge_l")) {
        z<-readpar(fp,symbol)
        for(i in seq_along(stores)) {
          nn<-nn+1L;native_rows[[nn]]<-native_row(z %>% filter(N %in% nodes,S==stores[i]),s,y,
            paste0(ifelse(symbol=="h2_stg_charge_l","Hyd_Sto_Cha_","Hyd_Sto_Dis_"),c("Tan","Cav")[i]))
        }
      }
    }
  }
  native_annual<-bind_rows(native_rows)
  bad<-native_annual %>% filter(!grepl("^Compressor_",Variable)) %>% left_join(iamc %>% filter(Region %in% r31) %>% select(Scenario,Region,Year,Variable,Reported=Value),
      by=c("Scenario","Region","Year","Variable")) %>% mutate(Reported=replace_na(Reported,0)) %>%
    filter(abs(Value-Reported)>pmax(1e-4,1e-7*abs(Value)))
  if(file.exists(reportfile)) {
    message("Reporting mismatches excluded from plots: ",nrow(bad));print(head(bad,4))
  }
  # Plot interface retains IAMC variable labels, with explicitly rebuilt values.
  iamc<-native_annual
  capacities<-daily_all<-compressors<-generation_rows<-response_rows<-compression_rows<-list()
  for(s in target_scenarios) {
    file<-file.path(run,"gams_output/gdx_primary",s,paste0(year,".gdx"))
    cap<-readpar(file,"ves_l")
    cs<-crossing(N=nodes,S=stores) %>% left_join(cap %>% filter(S %in% stores) %>% select(N,S,Value),by=c("N","S")) %>%
      mutate(Value=replace_na(Value,0),Scenario=s,Case=case(s),Type=factor(store_names[S],levels=names(store_colors)))
    stopifnot(all(cs$Value>=-1e-6))
    native<-cs %>% group_by(S) %>% summarise(Value=sum(Value),.groups="drop")
    rep<-iamc_spec(iamc,c("Cap_Hyd_Sto_Cap_Tan","Cap_Hyd_Sto_Cap_Cav"),year,r31) %>% filter(Scenario==s)
    stopifnot(near(native$Value[match(stores,native$S)],rep$Value[match(c("Cap_Hyd_Sto_Cap_Tan","Cap_Hyd_Sto_Cap_Cav"),rep$Variable)]))
    capacities[[s]]<-cs
    linkcap<-readpar(file,"vfs_l")
    flowcap<-linkcap %>% filter(grepl("-H2_.*-H2[SC]$",L)) %>%
      mutate(N=sub("-H2_.*$","",L),S=as.character(ifelse(grepl("-H2S$",L),stores[1],stores[2])),Scenario=s,
        Case=case(s),Type=factor(store_names[S],levels=names(store_colors)))
    stopifnot(all(flowcap$N %in% nodes));compressors[[s]]<-flowcap
    vgcap<-readpar(file,"vgs_l")
    # Power generation in native GW, 6-hour operation; only AC is read.
    time_data<-readpar(file.path(run,"data/input",paste0(s,".gdx")),"wt")
    times<-time_data$T[grepl("^d[0-9]{3}t",time_data$T)]
    stopifnot(length(times)==365*24/primary_weight,all(time_data$Value[time_data$T %in% times]==primary_weight))
    ac_tech<-unique(vgcap$R[vgcap$I=="AC"])
    dispatch<-filtered(file,"vg_l",list(nodes,"AC",ac_tech,times))
    stopifnot(all(grepl("^d[0-9]{3}t",dispatch$T)))
    generation_rows[[s]]<-dispatch %>% mutate(Technology=genclass(R),Value=Value*primary_weight/1000) %>%
      group_by(Technology) %>% summarise(Value=sum(Value),.groups="drop") %>% mutate(Scenario=s,Case=case(s),Year=year)
    response_rows[[s]]<-data.frame(Scenario=s,Year=year,
      Variable=c("Cap_Ele_SolarPV","Cap_Ele_Win","Cap_Ele_Sto_Bat","Cap_Hyd_Ele","Cap_Ele_Bio_w_CCS"),
      Value=c(sum(vgcap$Value[vgcap$I=="AC"&grepl("^solar",vgcap$R)]),
        sum(vgcap$Value[vgcap$I=="AC"&grepl("^wind",vgcap$R)]),
        sum(linkcap$Value[grepl("-AC_.*-DC$",linkcap$L)]),
        sum(linkcap$Value[grepl("-AC_.*-H2$",linkcap$L)]),
        sum(vgcap$Value[vgcap$I=="AC"&vgcap$R=="biomass_ccs"])))
    if(s!=target_scenarios[1]) {
      daily<-read_daily(file,cap,readpar(file,"wt")) %>% mutate(Scenario=s,Case=case(s))
      elec<-readpar(file,"h2_stg_elec_l")
      compression_rows[[s]]<-data.frame(Scenario=s,Case=case(s),TWh=sum(elec$Value)*primary_weight/1000)
      for(symbol in c("Charge","Discharge")) {
        annual<-daily %>% group_by(S) %>% summarise(Value=sum(.data[[symbol]]),.groups="drop")
        vars<-paste0(ifelse(symbol=="Charge","Hyd_Sto_Cha_","Hyd_Sto_Dis_"),c("Tan","Cav"))
        report<-iamc_spec(iamc,vars,year,r31) %>% filter(Scenario==s)
        av<-replace_na(annual$Value[match(stores,annual$S)],0)
        stopifnot(near(av,report$Value[match(vars,report$Variable)],atol=1e-4))
      }
      daily_all[[s]]<-daily
    } else stopifnot(all(cs$Value<1e-6),sum(abs(flowcap$Value))<1e-6)
  }
  capacity<-bind_rows(capacities) %>% left_join(mapping %>% select(N=N_IND,Country=R31),by="N")
  compressor<-bind_rows(compressors) %>% left_join(mapping %>% select(N=N_IND,Country=R31),by="N")
  daily<-bind_rows(daily_all) %>% left_join(mapping %>% select(N=N_IND,Country=R31),by="N") %>%
    mutate(Type=factor(store_names[S],levels=names(store_colors)))
  message("Native capacity and annual/day flow checks passed")
  stamp<-paste0(area," | ",year," | 6-hour power / daily H2 | Run ",token,
    if(area=="Europe")"\n2035 infeasible in all four cases; latest successful year shown." else "\n2050 infeasible in all four 500C cases; excluded.")
  scfill<-scale_fill_manual(values=store_colors,drop=FALSE)
  capvars<-c("Cap_Hyd_Sto_Cap_Tan","Cap_Hyd_Sto_Cap_Cav")
  chvars<-c("Hyd_Sto_Cha_Tan","Hyd_Sto_Cha_Cav")
  annual<-iamc_spec(iamc,c(capvars,chvars),years,r31) %>%
    mutate(Type=factor(ifelse(grepl("Tan$",Variable),"Ground tank","Salt cavern"),levels=names(store_colors)),
      Metric=ifelse(Variable %in% capvars,"Energy capacity (TWh)","Gross charging (TWh/yr)"),Value=Value/1000)

  # CAPACITY, CHARGING AND COUNTRY DISTRIBUTION PANELS -------------------------
  maincap<-annual %>% filter(Year==year,Variable %in% capvars)
  maincharge<-annual %>% filter(Year==year,Variable %in% chvars)
  pcap<-ggplot(maincap,aes(Case,Value,fill=Type))+geom_col(position=position_stack(reverse=TRUE),width=.65)+scfill+
    labs(title="Installed H2 storage",x=NULL,y="Energy capacity (TWh)",fill=NULL)
  pcharge<-ggplot(maincharge,aes(Case,Value,fill=Type))+geom_col(position=position_stack(reverse=TRUE),width=.65)+scfill+
    labs(title="Annual gross charging",x=NULL,y="Charging (TWh/yr)",fill=NULL)
  # Installed cowplot predates ggplot2's multiple guide-box slots. Retain only
  # the bottom slot's name before using the required get_legend extractor.
  legend_source<-ggplotGrob(pcap+theme(legend.position="bottom",legend.key.width=grid::unit(8,"mm"))+guides(fill=guide_legend(nrow=1)))
  hide_slots<-grepl("^guide-box",legend_source$layout$name)&legend_source$layout$name!="guide-box-bottom"
  legend_source$layout$name[hide_slots]<-"unused-guide-slot"
  legend<-get_legend(legend_source)
  stopifnot(!is.null(legend),!inherits(legend,"zeroGrob"))
  # Two metric panels above their shared legend.
  fig1<-plot_grid(plot_grid(pcap+theme(legend.position="none"),pcharge+theme(legend.position="none"),nrow=1,labels=c("a","b")),legend,ncol=1,rel_heights=c(1,.12))
  fig1<-plot_grid(ggdraw()+draw_label(stamp,size=11),fig1,ncol=1,rel_heights=c(.17,1))
  ggsave(file.path(output,"01_storage_cases.png"),fig1,width=12,height=5.5,units="in",dpi=240,bg="white")
  ptrend<-ggplot(filter(annual,Scenario!=target_scenarios[1]),aes(factor(Year),Value,fill=Type))+
    geom_col(position=position_stack(reverse=TRUE),width=.7)+facet_grid(Metric~Case,scales="free_y")+scfill+
    labs(title=paste(area,"| storage adoption through",year),subtitle="Only solved five-year snapshots; no interpolation",x=NULL,y=NULL,fill=NULL,caption=stamp)+
    theme(axis.text.x=element_text(angle=45,hjust=1))
  ggsave(file.path(output,"02_adoption_timeline.png"),ptrend,width=13,height=6.5,units="in",dpi=240,bg="white")
  bycountry<-capacity %>% filter(Scenario==on_scenario) %>% group_by(Country,Type) %>% summarise(Value=sum(Value)/1000,.groups="drop")
  corder<-bycountry %>% group_by(Country) %>% summarise(Total=sum(Value),.groups="drop") %>% arrange(Total) %>% pull(Country)
  bycountry$Country<-factor(bycountry$Country,levels=corder)
  compcountry<-compressor %>% filter(Scenario==on_scenario) %>% group_by(Country,Type) %>% summarise(Value=sum(Value),.groups="drop")
  compcountry$Country<-factor(compcountry$Country,levels=corder)
  countrycap<-ggplot(bycountry,aes(Country,Value,fill=Type))+geom_col(position=position_stack(reverse=TRUE))+coord_flip()+scfill+
    labs(title="Storage location",x=NULL,y="Installed energy capacity (TWh)",fill=NULL)
  countrycomp<-ggplot(compcountry,aes(Country,Value,fill=Type))+geom_col(position=position_stack(reverse=TRUE))+coord_flip()+scfill+
    labs(title="Independent compressor capacity",x=NULL,y="Hydrogen throughput capacity (GW_H2)",fill=NULL)
  # Country storage and compressor capacities, aligned side by side.
  fig3<-plot_grid(plot_grid(countrycap+theme(legend.position="none"),countrycomp+theme(legend.position="none"),nrow=1,labels=c("a","b")),legend,ncol=1,rel_heights=c(1,.10))
  fig3<-plot_grid(ggdraw()+draw_label(paste(stamp,"Tank + cavern case",sep="\n"),size=10),fig3,ncol=1,rel_heights=c(.17,1))
  ggsave(file.path(output,"03_storage_and_compressors_by_country.png"),fig3,width=12,height=7,units="in",dpi=240,bg="white")

  # DAILY INVENTORY AND FLOWS -------------------------------------------------
  inv<-daily %>% filter(Scenario==on_scenario) %>% group_by(Country,Day,Type) %>% summarise(Value=sum(Inventory)/1000,.groups="drop")
  pinv<-ggplot(inv,aes(Day,Value,fill=Type))+geom_area(stat="identity",position=position_stack(reverse=TRUE))+
    facet_wrap(~Country,ncol=ifelse(length(r31)>4,3,2),scales="free_y")+scfill+
    scale_x_continuous(breaks=c(1,91,182,274,365),limits=c(1,365),expand=expansion(mult=0))+
    scale_y_continuous(limits=c(0,NA),expand=expansion(mult=c(0,.07)))+
    labs(title=paste(area,"| daily end-of-day H2 inventory |",year),subtitle="Tank + cavern case | panel-specific vertical scales",x="Day of year",y="Inventory (TWh)",fill=NULL,caption=stamp)
  if(area=="NorthAmerica") {
    inv<-inv %>% mutate(Country=factor(Country,levels=c("USA","CAN","MEX")))
    totals<-inv %>% group_by(Country,Day) %>% summarise(Value=sum(Value),.groups="drop")
    for(country in c("USA","CAN","MEX")) {
    pinv<-ggplot(filter(inv,Country==country),aes(Day,Value,fill=Type))+
      geom_area(stat="identity",position=position_stack(reverse=FALSE))+
      geom_line(data=filter(totals,Country==country),aes(Day,Value,color="Total"),inherit.aes=FALSE,linewidth=.45)+
      scale_fill_manual(values=setNames(c("#A6CEE3","#FDBF6F"),names(store_colors)),
        labels=c("Hydrogen tank","Salt cavern"),name=NULL)+
      scale_color_manual(values=c(Total="black"),name=NULL)+
      guides(fill=guide_legend(order=1,nrow=1),color=guide_legend(order=2,nrow=1))+
      scale_x_continuous(breaks=c(1,60,120,180,240,300,365),limits=c(1,365),expand=c(0,0))+
      scale_y_continuous(limits=c(0,NA),expand=expansion(mult=c(0,.05)))+
      labs(title=paste0(country,": daily hydrogen inventory | ",year),
        x="Day of year",y="End-of-day inventory (TWh)")+
      theme_classic(base_size=11)+theme(plot.title=element_text(face="bold"),
        legend.position="bottom",legend.box="horizontal",legend.key.size=grid::unit(5,"mm"))
    ggsave(file.path(output,paste0("15_daily_inventory_",country,"_",year,".png")),pinv,
      width=230,height=120,units="mm",dpi=300,bg="white")
    }
  } else {
    ggsave(file.path(output,"04_daily_inventory_by_country.png"),pinv,width=12,height=ifelse(length(r31)>4,10,6.5),units="in",dpi=240,bg="white")
  }
  inventory_region<-daily %>% group_by(Case,Scenario,Day,Type) %>% summarise(Inventory=sum(Inventory)/1000,Charge=sum(Charge)/1000,Discharge=sum(Discharge)/1000,.groups="drop")
  pstock<-ggplot(inventory_region,aes(Day,Inventory,fill=Type))+geom_area(stat="identity",position=position_stack(reverse=TRUE))+
    facet_wrap(~Case,nrow=1,scales="free_y")+scfill+labs(title="Daily inventory",x="Day of year",y="TWh",fill=NULL)
  flow<-inventory_region %>% pivot_longer(c(Charge,Discharge),names_to="Flow",values_to="Value") %>% mutate(Value=ifelse(Flow=="Discharge",-Value,Value))
  pflow<-ggplot(flow,aes(Day,Value,fill=Type))+geom_col(width=1,position=position_stack(reverse=TRUE))+
    geom_hline(yintercept=0,linewidth=.3)+facet_wrap(~Case,nrow=1,scales="free_y")+scfill+
    labs(title="Daily gross charging (+) and withdrawal (-)",x="Day of year",y="TWh/day",fill=NULL)
  # Inventory row / signed flow row / shared legend. No stock time weights.
  fig5<-plot_grid(pstock+theme(legend.position="none"),pflow+theme(legend.position="none"),legend,ncol=1,rel_heights=c(1,1,.13),labels=c("a","b",""))
  fig5<-plot_grid(ggdraw()+draw_label(stamp,size=10),fig5,ncol=1,rel_heights=c(.13,1))
  ggsave(file.path(output,"05_inventory_and_flows_cases.png"),fig5,width=14,height=8,units="in",dpi=240,bg="white")

  # GENERATION MIX AND CAPACITY RESPONSE --------------------------------------
  gen<-bind_rows(generation_rows)
  stopifnot(all(is.finite(gen$Value)),all(gen$Value>=-1e-5))
  pgen<-ggplot(gen,aes(Case,Value,fill=Technology))+geom_col(position=position_stack(reverse=TRUE),width=.65,show.legend=TRUE)+
    scale_fill_manual(values=gencolors,limits=secele$Legend,drop=FALSE)+guides(fill=guide_legend(ncol=3,byrow=TRUE))+
    labs(title="Electricity generation",x=NULL,y="TWh/yr",fill=NULL)
  responsevars<-c("Cap_Ele_SolarPV","Cap_Ele_Win","Cap_Ele_Sto_Bat","Cap_Hyd_Ele","Cap_Ele_Bio_w_CCS")
  response<-bind_rows(response_rows) %>% mutate(Case=case(Scenario),Technology=c("Solar PV","Wind","Battery power","Electrolyzer","BECCS")[match(Variable,responsevars)]) %>%
    group_by(Variable) %>% mutate(Delta=Value-Value[Scenario==target_scenarios[1]]) %>% ungroup()
  presponse<-ggplot(filter(response,Scenario!=target_scenarios[1]),aes(Technology,Delta,fill=Case))+geom_col(position="dodge")+
    geom_hline(yintercept=0,linewidth=.3)+scale_fill_manual(values=case_colors)+coord_flip()+
    labs(title="Installed capacity change from no storage",x=NULL,y="Capacity difference (GW)",fill=NULL)
  # Separate units: generation on left; capacity difference on right.
  fig6<-plot_grid(pgen+power_response_text_theme(),presponse+power_response_text_theme(),nrow=1,labels=c("a","b"),label_size=16,rel_widths=c(1,1.1))
  fig6<-plot_grid(ggdraw()+draw_label(stamp,size=12),fig6,ncol=1,rel_heights=c(.16,1))
  ggsave(file.path(output,"06_power_system_response.png"),fig6,width=14,height=7.5,units="in",dpi=240,bg="white")

  # STORAGE MAP: SHARED AREA SCALE, DISTINCT TECHNOLOGY PANELS -----------------
  bounds<-switch(area,Asia=c(60,150,-12,57),NorthAmerica=c(-179,-50,5,84),Europe=c(-25,45,32,74))
  pol<-geography %>% filter(N_PSY %in% nodes)
  points<-capacity %>% filter(Scenario==on_scenario) %>% left_join(st_drop_geometry(geography) %>%
    select(N=N_PSY,Lon=centroid_x,Lat=centroid_y),by="N") %>% mutate(TWh=pmax(Value,0)/1000)
  stopifnot(!anyNA(points$Lon),!anyNA(points$Lat))
  offmap<-points %>% filter(Lon<bounds[1]|Lon>bounds[2]|Lat<bounds[3]|Lat>bounds[4])
  mapnote<-paste0("Circle AREA represents storage energy capacity. Shared scale across types.\nOutside map window: ",format(sum(offmap$TWh),digits=3)," TWh (included in regional totals).")
  pmap<-ggplot()+geom_sf(data=pol,fill="#F4F4F4",color="#B0B0B0",linewidth=.2)+
    geom_point(data=filter(points,TWh>1e-8),aes(Lon,Lat,size=TWh,fill=Type),shape=21,color="#333333",stroke=.2)+
    facet_wrap(~Type,nrow=1)+scale_size_area(max_size=15,name="Capacity (TWh)")+scfill+
    coord_sf(xlim=bounds[1:2],ylim=bounds[3:4],expand=FALSE)+
    labs(title=paste(area,"| H2 storage locations |",year),subtitle="Tank + cavern case",x=NULL,y=NULL,fill=NULL,
      caption=paste(stamp,mapnote,sep="\n"))+theme(panel.grid=element_blank(),axis.text=element_text(size=8))
  ggsave(file.path(output,"07_storage_capacity_map.png"),pmap,width=13,height=6.5,units="in",dpi=240,bg="white")

  # COMPRESSOR EXPANSION AND CAVERN-POTENTIAL UTILIZATION -----------------------
  comptrend<-iamc_spec(iamc,c("Compressor_Tank_GW","Compressor_Cavern_GW"),years,r31) %>%
    filter(Scenario!=target_scenarios[1]) %>%
    mutate(Type=factor(ifelse(Variable=="Compressor_Tank_GW","Ground tank","Salt cavern"),levels=names(store_colors)))
  pcomp<-ggplot(comptrend,aes(factor(Year),Value,fill=Type))+geom_col(position=position_stack(reverse=TRUE),width=.7)+
    facet_wrap(~Case,nrow=1)+scfill+labs(title=paste(area,"| independent compressor expansion"),
      subtitle="Installed H2 throughput capacity; common vertical scale across cases",x=NULL,y="Capacity (GW_H2)",fill=NULL,caption=stamp)+
    theme(axis.text.x=element_text(angle=45,hjust=1))
  ggsave(file.path(output,"08_compressor_capacity_timeline.png"),pcomp,width=13,height=5.5,units="in",dpi=240,bg="white")
  potfile<-file.path(run,"gams_output/gdx_primary",on_scenario,paste0(year,".gdx"))
  potential<-readpar(potfile,"h2_cavern_potential")
  names(potential)[1]<-"N"
  potential<-data.frame(N=nodes) %>% left_join(potential,by="N") %>% mutate(Value=replace_na(Value,0)) %>%
    rename(PotentialGWh=Value) %>% left_join(mapping %>% select(N=N_IND,Country=R31),by="N")
  potnode<-capacity %>% filter(Scenario==on_scenario,S==stores[2]) %>%
    left_join(potential %>% select(N,PotentialGWh),by="N") %>% mutate(PotentialGWh=replace_na(PotentialGWh,0))
  stopifnot(all(potnode$Value<=potnode$PotentialGWh+pmax(1e-4,1e-6*potnode$PotentialGWh)))
  # Include every selected node's potential, even where no cavern was built.
  potcountry<-potential %>% group_by(Country) %>% summarise(PotentialTWh=sum(PotentialGWh)/1000,.groups="drop") %>%
    left_join(bycountry %>% filter(Type=="Salt cavern") %>% transmute(Country=as.character(Country),CavernTWh=Value),by="Country") %>%
    left_join(bycountry %>% filter(Type=="Ground tank") %>% transmute(Country=as.character(Country),TankTWh=Value),by="Country") %>%
    mutate(CavernTWh=replace_na(CavernTWh,0),TankTWh=replace_na(TankTWh,0),
      UtilizationPct=ifelse(PotentialTWh>1e-8,100*CavernTWh/PotentialTWh,NA_real_),
      Country=factor(Country,levels=corder))
  pcav<-ggplot(filter(potcountry,!is.na(UtilizationPct)),aes(Country,UtilizationPct))+
    geom_col(aes(y=100),fill="#E8E8E8",width=.65)+geom_col(fill=store_colors[["Salt cavern"]],width=.65)+
    geom_text(aes(label=paste0(format(signif(UtilizationPct,3),trim=TRUE),"%")),hjust=-.1,size=3)+
    coord_flip(ylim=c(0,115),clip="off")+labs(title="Cavern deployment / available potential",x=NULL,y="Share of modeled cavern potential (%)")
  occupancy<-daily %>% filter(Scenario==on_scenario,S==stores[2]) %>% group_by(Country,Day) %>%
    summarise(InventoryTWh=sum(Inventory)/1000,.groups="drop") %>% group_by(Country) %>%
    summarise(PeakInventoryTWh=max(InventoryTWh),.groups="drop") %>%
    left_join(potcountry %>% mutate(Country=as.character(Country)),by="Country") %>%
    mutate(PeakFillPct=100*PeakInventoryTWh/CavernTWh,Country=factor(Country,levels=corder))
  pfill<-ggplot(filter(occupancy,CavernTWh>1e-6),aes(Country,PeakFillPct))+
    geom_col(fill=store_colors[["Salt cavern"]],width=.65)+coord_flip(ylim=c(0,110))+
    labs(title="Peak inventory / installed cavern capacity",x=NULL,y="Peak fill (%)")
  # Potential use / installed cavern plus tank / occupancy of installed caverns.
  figpot<-plot_grid(pcav,countrycap+theme(legend.position="none"),pfill,nrow=1,labels=c("a","b","c"),rel_widths=c(1.1,1.1,1))
  figpot<-plot_grid(ggdraw()+draw_label(paste(stamp,"Tank + cavern case | Potential is the model's provisional onshore ceiling.",sep="\n"),size=10),figpot,legend,ncol=1,rel_heights=c(.18,1,.10))
  ggsave(file.path(output,"09_cavern_potential_and_tank_deployment.png"),figpot,width=16,height=7.5,units="in",dpi=240,bg="white")
  message("CAVERN POTENTIAL AND TANK DEPLOYMENT");print(potcountry,width=Inf,n=Inf)
  message("CAVERN OCCUPANCY");print(occupancy %>% select(Country,PeakInventoryTWh,PeakFillPct),n=Inf)

  # REPORT OVERVIEW AND CONSOLE ANALYSIS --------------------------------------
  overview_stock<-ggplot(filter(inventory_region,Scenario==on_scenario),aes(Day,Inventory,fill=Type))+
    geom_area(stat="identity",position=position_stack(reverse=TRUE))+scfill+labs(title="Regional daily inventory",x="Day of year",y="TWh",fill=NULL)
  # 2 x 2: capacity / gross charging; location / chronological inventory.
  overview<-plot_grid(pcap+theme(legend.position="none"),pcharge+theme(legend.position="none"),
    countrycap+theme(legend.position="none"),overview_stock+theme(legend.position="none"),ncol=2,labels=c("a","b","c","d"),align="hv")
  overview<-plot_grid(ggdraw()+draw_label(stamp,size=11),overview,legend,ncol=1,rel_heights=c(.12,1,.07))
  ggsave(file.path(output,"00_report_overview.png"),overview,width=13,height=9,units="in",dpi=240,bg="white")
  metrics<-daily %>% group_by(Scenario,Case,S,Type) %>% summarise(ChargeTWh=sum(Charge)/1000,DischargeTWh=sum(Discharge)/1000,.groups="drop") %>%
    left_join(capacity %>% group_by(Scenario,S) %>% summarise(CapacityTWh=sum(Value)/1000,.groups="drop"),by=c("Scenario","S")) %>%
    left_join(compressor %>% group_by(Scenario,S) %>% summarise(CompressorGW=sum(Value),.groups="drop"),by=c("Scenario","S")) %>%
    mutate(GrossChargeCapacityRatio=ChargeTWh/CapacityTWh,EnergyPowerHours=CapacityTWh*1000/CompressorGW)
  comp_elec<-as_tibble(bind_rows(compression_rows))
  message("METRICS ",area);print(metrics,width=Inf,n=Inf)
  message("COMPRESSION ELECTRICITY TWh");print(comp_elec %>% select(Case,TWh),n=Inf)
  message("COUNTRY CAPACITY TWh");print(bycountry,n=Inf)
  message("CAPACITY RESPONSE GW");print(response %>% select(Case,Technology,Value,Delta),n=Inf)
  message("CHARGING CHANGE TWh/yr");print(maincharge,n=Inf)
  message("GENERATED 10 PNGs: ",output)
  all_main[[area]]<-maincap %>% mutate(Area=area,Year=year)
  all_metrics[[area]]<-metrics %>% mutate(Area=area,Year=year)
}

# CROSS-REGION COMPARISON -----------------------------------------------------
for(row in seq_len(nrow(PSM_RUNS))) {
  area<-PSM_RUNS$Region[row]
  if(area %in% names(all_main))next
  yr<-PSM_RUNS$MainYear[row]
  selected_nodes<-mapping$N_IND[mapping$R31 %in% region_groups[[area]]]
  all_main[[area]]<-bind_rows(lapply(target_scenarios,function(s) {
    fp<-file.path(source_root,PSM_SCENARIO,area,PSM_RUNS$Date[row],"gams_output/gdx_primary",s,paste0(yr,".gdx"))
    state<-year_status(file.path(source_root,PSM_SCENARIO,area,PSM_RUNS$Date[row],"gams_output/log",s,paste0(yr,".lst")))
    stopifnot(state$ok)
    z<-readpar(fp,"ves_l")
    tibble(Scenario=s,Case=case(s),Type=factor(store_names[stores],levels=names(store_colors)),
      Value=vapply(stores,function(st)sum(z$Value[z$N %in% selected_nodes & z$S==st])/1000,numeric(1)),Area=area,Year=yr)
  }))
}
cross<-bind_rows(all_main) %>% mutate(Area=paste0(Area," (",Year,")"))
pcross<-ggplot(cross,aes(Case,Value,fill=Type))+geom_col(position=position_stack(reverse=TRUE),width=.65)+
  facet_wrap(~Area,scales="free_y",nrow=1)+scale_fill_manual(values=store_colors,drop=FALSE)+
  labs(title="H2 storage deployment: latest successful comparison years",subtitle="Asia / North America: 2045; Europe: 2030. Panel-specific vertical scales.",
    x=NULL,y="Installed energy capacity (TWh)",fill=NULL,caption="Same daily H2 balance in all cases. Different regional boundaries and model years; not a ranking of storage potential.")+
  theme(axis.text.x=element_text(angle=25,hjust=1))
ggsave(file.path(outroot,"regional_comparison_capacity.png"),pcross,width=15,height=5.5,units="in",dpi=240,bg="white")
message("FINISHED: 31 PNGs. No model execution or source-output changes.")
