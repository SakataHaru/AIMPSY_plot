suppressPackageStartupMessages(library(gdxrrw))
igdx('C:/GAMS/win64/26.1')
r <- '//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output/global_H2_Transport/Global/2611061840'
s <- '500C-H2_Pipeline_Truck-NH3_Rail_Truck_Ship'
z <- rgdx.param(file.path(r,'main/merged_output.gdx'),'iamc_gdx')
z <- z[as.character(z$Sc)==s,]
print(table(as.character(z$Y5)))
v <- unique(as.character(z$Sv)); print(v[grepl('Tra|Trd|Hyd|Cost|Emi',v)])
p <- file.path(r,'data/input',paste0(s,'.gdx'))
x <- rgdx.param(p,'wt'); print(head(x)); print(tail(x)); print(table(x$wt))
print(head(rgdx.set(p,'T')))
