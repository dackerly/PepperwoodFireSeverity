# map Tubbs
rm(list=ls())
#source('scripts/Load.data.R')
library(terra)
library(sp)

# Tubbs
t.rdnbr <- rast('bigdata/mtbs/2017_Tubbs/ca3859812261820171009/ca3859812261820171009_20170707_20180710_rdnbr.tif')
plot(t.rdnbr)
t.rdnbr
rdnbr.vals <- values(t.rdnbr)
rdnbr.vals[which(rdnbr.vals<0)] <- 0
rdnbr.vals[which(rdnbr.vals>1000)] <- 1000
hist(rdnbr.vals)
t.rdnbr0 <- setValues(t.rdnbr,rdnbr.vals)

plot(t.rdnbr0)
crs(t.rdnbr0)
pras <- projection(t.rdnbr0)

filepath <- "bigdata/Tubbs-shapefile/2017_Tubbs_Fire/2017_Tubbs_Fire.shp"
tubbs.sf <- st_read(filepath)
crs(tubbs.sf)
plot(tubbs.sf,max.plot=1,border='red',col=NA)
tubbs.p <- st_transform(tubbs.sf,"EPSG:4326")
plot(tubbs.p,max.plot=1,border='red',col=NA)

filepath <- '/Users/david/Documents/My_Docs/Projects/Pepperwood/PWD_GIS/PPshapefiles/PPshapefile-geo/Pepperwood.shp'
pwdsf <- st_read(filepath)
pwdsf <- st_set_crs(pwdsf, 4326)
pwdsf
plot(tubbs.p,max.plot=1,border='red',col=NA)
plot(pwdsf,max.plot=1,border='black',col=NA)

t.rdnbr0.p <- terra::project(t.rdnbr0,crs(pwdsf))
t.rdnbr0.p.m <- mask(t.rdnbr0.p,tubbs.p)
t.rdnbr0.p.m <- crop(t.rdnbr0.p.m,c(-122.77,-122.58,38.46,38.68))

plot(t.rdnbr0.p.m)
#plot(tubbs.p,max.plot=1,border='red',col=NA,add=T)
plot(pwdsf,max.plot=1,border='red',col=NA,add=T,lwd=2)

t.rdnbr0.pwd <- crop(t.rdnbr0.p.m,c(-122.75,-122.67,38.56,38.62))

plot(t.rdnbr0.pwd)
plot(pwdsf,max.plot=1,border='red',col=NA,add=T,lwd=2)

# now get plots
source('scripts/function-plotCenters.R')
fnames <- c('vegplots-54-20m-cpt.csv','hectares-18-20m-cpt.csv','vegplots-54-5m-cpt.csv')
plotSize <- c(100,20,20,5)

cp <- plotCenters(fname=fnames[f],plotSize=plotSize[f])
ch <- plotCenters(fname=fnames[2],plotSize=plotSize[2])
rsel <- which(ch[[1]]$Quad=='C3')


plot(t.rdnbr0.pwd)
plot(pwdsf,max.plot=1,border='red',col=NA,add=T,lwd=2)
points(cp[[1]]$GEO.x,cp[[1]]$GEO.y,col='red',pch=19)
points(ch[[1]]$GEO.x[rsel],ch[[1]]$GEO.y[rsel],col='red',pch=1,cex=2)

# now with discrete fire severities
rdnbr.vals <- values(t.rdnbr0.pwd)
fsvals <- rdnbr.vals
fsvals[which(is.na(rdnbr.vals))] <- 1
fsvals[which(!is.na(rdnbr.vals))] <- 4
fsvals[which(rdnbr.vals<=430)] <- 3
fsvals[which(rdnbr.vals<=135)] <- 2
t.fs.pwd <- setValues(t.rdnbr0.pwd,fsvals)

plot(t.fs.pwd,col=c('blue','brown','orange','red'))
plot(pwdsf,max.plot=1,border='black',col=NA,add=T,lwd=2)
points(cp[[1]]$GEO.x,cp[[1]]$GEO.y,col='black',pch=19)
points(ch[[1]]$GEO.x[rsel],ch[[1]]$GEO.y[rsel],col='black',pch=1,cex=2)

######
# Define the path to your .shp file
filepath <- "bigdata/Tubbs-shapefile/2017_Tubbs_Fire/2017_Tubbs_Fire.shp"
tubbs.sf <- st_read(filepath)
crs(tubbs.sf)
plot(tubbs.sf,max.plot=1,border='red',col=NA)
tubbs.sf.p <- st_transform(tubbs.sf,crs(pras))
plot(tubbs.sf.p,max.plot=1,border='red',col=NA)

plot(t.rdnbr0)
plot(tubbs.sf.p,max.plot=1,border='red',col=NA,add=T)

t.rdnbr.masked <- mask(t.rdnbr0,tubbs.sf.p)
plot(t.rdnbr.masked)
plot(tubbs.sf.p,max.plot=1,border='red',col=NA,add=T)

filepath <- '/Users/david/Documents/My_Docs/Projects/Pepperwood/PWD_GIS/PPshapefiles/PPshapefile-geo/Pepperwood.shp'
pwdsf <- st_read(filepath)
pwdsf <- st_set_crs(pwdsf, 4326)
plot(pwdsf,max.plot=1)
pwdsf.p <- st_transform(pwdsf,crs(pras))
plot(t.rdnbr.masked)
plot(pwdsf.p,add=T,max.plot=1,border='black',col=NA)

# now try reprojecting the raster
t.rdnbr.masked.p <- project(t.rdnbr.masked,"EPSG:4326")
