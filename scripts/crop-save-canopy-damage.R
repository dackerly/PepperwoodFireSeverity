rm(list=ls())
library(raster)
library(sp)
library(rgdal)
getwd()

r <- raster('/Volumes/Google\ Drive/My\ Drive/My_Drive_Cloud/Drive-Projects/BayArea/SonomaFires-2017/gis/Tukman/Oct2017CanopyDamage/oct17_candam')
r
plot(r)
abline(v=6340000)
abline(v=6385000)
abline(h=1933000)
abline(h=2010000)
ex <- drawExtent()
#ex <- matrix(c(6340000,6385000,1933000,2010000),nrow=2)

rTubbs <- crop(r,extent(ex))
plot(rTubbs)

pex <- drawExtent()
pTubbs <- crop(rTubbs,pex)
plot(pTubbs)

writeRaster(pTubbs,'bigdata/Tukman/candam/canopy_damage.grd',overwrite=T)
proj4string(pTubbs)

xx <- raster('bigdata/Tukman/candam/canopy_damage.grd')
xx
