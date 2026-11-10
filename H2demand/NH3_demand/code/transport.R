# Display gross H2 pipeline and NH3 truck flows with the same width scale.
# Canonical endpoint ordering also handles reversed source labels. Lane positions
# use the union of carriers across scenarios, keeping comparison panels aligned.
make_carrier_transport_paths <- function(flows, carrier_order = c('H2','NH3','Synfuel'),
                                         lane_spacing = 0.45, samples = 81L) {
  if (!nrow(flows)) {
    out <- mutate(flows,lon=numeric(),lat=numeric(),PathID=character(),PathPiece=character(),Step=integer())
    attr(out,'shared_corridors') <- tibble::tibble()
    return(out)
  }
  links <- flows %>% mutate(
    Swap = Point0 > Point1,
    Start = pmin(Point0,Point1), End = pmax(Point0,Point1),
    X0 = ifelse(Swap,lon1,lon0), Y0 = ifelse(Swap,lat1,lat0),
    X1 = ifelse(Swap,lon0,lon1), Y1 = ifelse(Swap,lat0,lat1)
  ) %>%
    # Follow the short corridor across the Pacific instead of crossing the map.
    mutate(X1=X0+((X1-X0+180) %% 360)-180) %>%
    group_by(Scenario,Carrier,Start,End,X0,Y0,X1,Y1) %>%
    summarise(FlowEJ=sum(FlowEJ),.groups='drop')
  lanes <- links %>% distinct(Start,End,Carrier) %>%
    group_by(Start,End) %>% arrange(match(Carrier,carrier_order),.by_group=TRUE) %>%
    mutate(Carriers=n(),Lane=(n()+1)/2-row_number()) %>% ungroup()
  links <- links %>% left_join(lanes,by=c('Start','End','Carrier')) %>%
    mutate(PathID=paste(Scenario,Start,End,Carrier,sep='__'))
  tt <- seq(0,1,length.out=samples)
  # Fade the offset near the endpoints so all lines still meet the real nodes.
  taper <- pmin(1,tt/0.15,(1-tt)/0.15)
  taper <- taper^2*(3-2*taper)
  paths <- bind_rows(lapply(seq_len(nrow(links)),function(i) {
    z <- links[i,]
    # Longitude compression gives a perpendicular offset even on N/S links.
    # Lane spacing is in latitude-equivalent degrees; callers set it for map size.
    lon_scale <- max(cos((z$Y0+z$Y1)/2*pi/180),0.25)
    dx <- (z$X1-z$X0)*lon_scale; dy <- z$Y1-z$Y0
    distance <- sqrt(dx^2+dy^2)
    if (!is.finite(distance) || distance<=0) stop('Invalid transport corridor: ',z$PathID)
    shift <- 0.08*distance*tt*(1-tt)+z$Lane*lane_spacing*taper
    tibble::tibble(
      Scenario=z$Scenario,Carrier=z$Carrier,FlowEJ=z$FlowEJ,PathID=z$PathID,
      Start=z$Start,End=z$End,Lane=z$Lane,Step=seq_along(tt),
      lon=z$X0+(z$X1-z$X0)*tt-shift*dy/distance/lon_scale,
      lat=z$Y0+(z$Y1-z$Y0)*tt+shift*dx/distance
    )
  }))
  paths <- paths %>% mutate(lon=((lon+180) %% 360)-180) %>%
    group_by(PathID) %>% arrange(Step,.by_group=TRUE) %>%
    mutate(PathPiece=paste0(PathID,'__',cumsum(abs(lon-lag(lon,default=first(lon)))>180))) %>% ungroup()
  attr(paths,'shared_corridors') <- lanes %>% filter(Carriers>1) %>% distinct(Start,End,Carriers)
  paths
}
