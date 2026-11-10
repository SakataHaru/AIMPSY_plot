suppressPackageStartupMessages(library(gdxrrw))
igdx('C:/GAMS/win64/26.1')
p <- '//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output/global_H2_Transport/Global/2611061840/data/input/500C-H2_Pipeline_Truck-NH3_Rail_Truck_Ship.gdx'
x <- gdxInfo(p,dump=FALSE,returnList=TRUE)
print(x$sets[grepl('^FL_L|^L_|^M_LL2',x$sets)])
for(n in intersect(c('FL_L','FL_LT','L_H2PIPE','L_PPL','L_NAV','L_H2ROAD','L_ROAD','L_RAIL','M_LL2'),x$sets)) {
  a<-rgdx.set(p,n)
  cat(n,':',nrow(a),'rows; columns=',paste(names(a),collapse=','),'\n')
  print(head(a,3))
}
