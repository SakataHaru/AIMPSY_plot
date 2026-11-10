suppressPackageStartupMessages(library(gdxrrw))
igdx('C:/GAMS/win64/26.1')
root <- '//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output/global_H2_Transport/Global'
s <- '500C-H2_Pipeline_Truck-NH3_Rail_Truck_Ship'
p <- file.path(root,c('2611061840','2610281619'),'data/input',paste0(s,'.gdx'))
meta <- gdxInfo(p[[1]],dump=FALSE,returnList=TRUE)
print(meta$parameters[grepl('dmd|fcc|gcc|fvoc|gsc|fsc|feta|ncv|lt',meta$parameters)])
for(n in intersect(c('dmda_t','gcc_t','fcc_t','fvoc_t','ffoc_t','gsc_base','fsc_base','feta','ncv'),meta$parameters)) {
  a <- lapply(p,function(path) {
    z <- rgdx.param(path,n)
    z[] <- lapply(z,function(x) if(is.factor(x)) as.character(x) else x)
    z <- z[do.call(order,z),,drop=FALSE]; rownames(z)<-NULL; z
  })
  cat(n,': ',nrow(a[[1]]),' / ',nrow(a[[2]]),' rows; ',
      paste(all.equal(a[[1]],a[[2]],tolerance=1e-10),collapse='; '),'\n',sep='')
  if(n=='fsc_base') {
    z <- merge(a[[1]],a[[2]],by=c('L','Y'),suffixes=c('_6HOUR','_12DAY'))
    z <- z[abs(z$fsc_base_6HOUR-z$fsc_base_12DAY)>1e-8,]
    print(head(z,12)); cat('Differing stock rows=',nrow(z),
      '; all distribution=',all(grepl('-AC_.*-AC2$',z$L)),'\n')
  }
}
