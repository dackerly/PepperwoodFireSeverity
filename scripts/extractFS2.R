## re-extract burn severity using updated plot coordinates
# can be used to update in the future with higher precision GPS data

# See Park et al. 2014 (doi:10.3390/rs6031827) for discussion of burn severity metrics
# From Park et al:
# Based on landsat spectral band measures:
# NBR = (b4-b7)/(b4+b7)
# dNBR = ((NBRprefire - NBRpostfire)*1000)-dNBRoffset
# offset is values calculated outside fire perimeter to account for other changes in veg
# dNBR as distributed by MTBS does not include the dNBRoffset
# RdNBR = dNBR/(abs(dNBR))^0.5
# RBR = dNBR/(NBRprefire + 1.001)

rm(list=ls())
source('scripts/Load.data.R')
library(raster)
library(sp)

## load coordinates for sampling points - this section can be edited by different users, and the rest should then run
# at the end of this section, points should be in an object called 'cp' which is a list with three components:
# cp[[1]] data.frame with all data desired for export of the sampled fire seversity data
# cp[[2]] SpatialPointsData object with point locations, and associated proj4string projection information. If points represent a plot it's assumed to be the center point
# cp[[3]] a data value for the size of the plot represented by each point, in units of m representing length of the side of the plot
# set outName for your desired outfile name

# code for Ackerly vegplots and hectares
{
  ## script for loading vegplots and hectares
  # choose which plot size and whether or not full plots or quads
  source('scripts/function-plotCenters.R')
  fnames <- c('vegplots-54-20m-cpt.csv','hectares-18-20m-cpt.csv','vegplots-54-5m-cpt.csv')
  plotSize <- c(100,20,20,5)
  
  # select which infile
  #f <- 1
  #outName <- 'vegplots-54-20m-FS.csv'

  # select which infile
  #f <- 2
  #outName <- 'hectares-18-20m-FS.csv'

  # select which infile
  f <- 3
  outName <- 'vegplots-54-5m-FS.csv'
  
  # set outfile name
  
  cp <- plotCenters(fname=fnames[f],plotSize=plotSize[f])
}

## INSERT OTHER CODE HERE FOR OTHER LOCATION DATA
{}

# showMaps T to examine each raster and check points are properly positioned
# F for faster processing
showMaps <- F

# Transfer cp list items to three variables for subsequent use below
cpdf <- cp[[1]]
cpSP <- cp[[2]]
cpR <- cp[[3]]
head(cpdf)
plot(cpSP)
print(cpR)

## NOW EXTRACT FIRE SEVERITY DATA FROM SPATIAL DATA FILES
## MTBS - 30m resolution so no reason to do the vegplot quads
dir('bigdata/mtbs/2017_Tubbs/')
system('cat bigdata/mtbs/readme.txt')

# Tubbs
t.rdnbr <- raster('bigdata/mtbs/2017_Tubbs/ca3859812261820171009/ca3859812261820171009_20170707_20180710_rdnbr.tif')

t.dnbr<- raster('bigdata/mtbs/2017_Tubbs/ca3859812261820171009/ca3859812261820171009_20170707_20180710_dnbr.tif')

t.dnbr6<- raster('bigdata/mtbs/2017_Tubbs/ca3859812261820171009/ca3859812261820171009_20170707_20180710_dnbr6.tif')

# Kincade
k.rdnbr <- raster('bigdata/mtbs/2019_Kincade/ca3879612276720191023/ca3879612276720191023_20190424_20200426_rdnbr.tif')

k.dnbr <- raster('bigdata/mtbs/2019_Kincade/ca3879612276720191023/ca3879612276720191023_20190424_20200426_dnbr.tif')

k.dnbr6 <- raster('bigdata/mtbs/2019_Kincade/ca3879612276720191023/ca3879612276720191023_20190424_20200426_dnbr6.tif')

# transform spatial points to projection for these rasters
cps.sptr <- spTransform(cpSP,CRS(proj4string(t.dnbr)))
cpts <- coordinates(cps.sptr)

# check locations 
if (showMaps) plot(t.dnbr)
if (showMaps) points(cpts,pch=19,col='red')

cpdf$Tubbs.MTBS.RDNBR.30 <- extract(t.rdnbr,cpts)
cpdf$Tubbs.MTBS.DNBR.30 <- extract(t.dnbr,cpts)
cpdf$Tubbs.MTBS.DNBR6.30 <- extract(t.dnbr6,cpts)
cpdf$Kincade.MTBS.RDNBR.30 <- extract(k.rdnbr,cpts)
cpdf$Kincade.MTBS.DNBR.30 <- extract(k.dnbr,cpts)
cpdf$Kincade.MTBS.DNBR6.30 <- extract(k.dnbr6,cpts)
head(cpdf)

## Tukman burn damage
system('cat bigdata/Tukman/readme.txt')
r <- raster('bigdata/Tukman/candam/canopy_damage.grd')
r

# transform spatial points to projection for these rasters
cps.sptr <- spTransform(cpSP,CRS(proj4string(r)))
cpts <- coordinates(cps.sptr)

# check locations 
if (showMaps) plot(r)
if (showMaps) points(cpts,pch=19,col='red')

cpdf$Tubbs.Tukman.CanDam.10 <- extract(r,cpts)
head(cpdf)

# Clark
system('cat bigdata/Clark/readme.txt')

r <- raster('bigdata/Clark/Tubbs/LC08_CU_001008_20170925_201701027_RBR.tif')
r

# transform spatial points to projection for these rasters
cps.sptr <- spTransform(cpSP,CRS(proj4string(r)))
cpts <- coordinates(cps.sptr)

# check locations 
if (showMaps) plot(r)
if (showMaps) points(cpts)

cpdf$Tubbs.Clark.RBR.30 <- extract(r,cpts)
head(cpdf)

# USFS BARC fire severity
system('cat bigdata/BARC/readme.txt')

r <- raster('bigdata/BARC/Tubbs/barc256_s220170927_s220171017_20m.tif')
r

# transform spatial points to projection for these rasters
cps.sptr <- spTransform(cpSP,CRS(proj4string(r)))
cpts <- coordinates(cps.sptr)

# check locations 
if (showMaps) plot(r)
if (showMaps) points(cpts,pch=19,col='red')

cpdf$Tubbs.BARC.dNBR.20 <- extract(r,cpts)

## extract 5 m for veg plot quads
r2 <- raster('bigdata/BARC/Tubbs/barc256_s220170927_spot20171018_5m.tif')

# transform spatial points to projection for these rasters
cps.sptr <- spTransform(cpSP,CRS(proj4string(r2)))
cpts <- coordinates(cps.sptr)

# check locations 
if (showMaps) plot(r2)
if (showMaps) points(cpts,pch=19,col='red')

cpdf$Tubbs.BARC.dNDVI.5 <- extract(r2,cpts)

## extract 5 m classified
r2 <- raster('bigdata/BARC/Tubbs/Tubbs_burn_severity_20171018_UTM.tif')
r2

# transform spatial points to projection for these rasters
cps.sptr <- spTransform(cpSP,CRS(proj4string(r2)))
cpts <- coordinates(cps.sptr)

# check locations 
if (showMaps) plot(r2)
if (showMaps) points(cpts,pch=19,col='red')

cpdf$Tubbs.BARC.dNDVI4.5 <- extract(r2,cpts)
head(cpdf)

# Soil Burn Severity - from Tukman gis files
system('cat bigdata/SBS/readme.txt')

r2 <- raster('bigdata/SBS/SoilBurnSeverity/Tubbs_SBS.tif')
r2

# transform spatial points to projection for these rasters
cps.sptr <- spTransform(cpSP,CRS(proj4string(r2)))
cpts <- coordinates(cps.sptr)

# check locations 
if (showMaps) plot(r2)
if (showMaps) points(cpts,pch=19,col='red')

cpdf$Tubbs.BARC.SBS4.5 <- extract(r2,cpts)
head(cpdf)

write.csv(cpdf,paste('data/FSextract/',outName,sep=''))
