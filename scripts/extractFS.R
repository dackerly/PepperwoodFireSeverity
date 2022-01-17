## re-extract burn severity using updated plot coordinates
# can be used to update in the future with higher precision GPS data

rm(list=ls())
library(raster)
source('scripts/Load.data.R')
#get.plot.info()

## load plot center coordinate data from VegPlots master git repo
options(stringsAsFactors=FALSE) 
prefix='https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/2021/2021_updated_shapefiles_and_coordinates/shapefiles/'

# choose which plot size and whether or not full plots of QUADS
fnames <- c('hectares-18-100m-cpt.csv','vegplots-54-20m-cpt.csv','hectares-18-20m-cpt.csv','vegplots-54-5m-cpt.csv')


## NOW EXTRACT FIRE SEVERITY DATA FROM SPATIAL DATA FILES
## MTBS - 30m resolution to no reason to do the vegplot quads
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

f=1
for (f in 1:3) {
  fname <- paste(prefix,fnames[f],sep='')
  
  cp <- read.csv(text=getURL(fname, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  head(cp)  
  
  # convert cp to proper projection
  cps <- SpatialPoints(cp[,c('UTM.x','UTM.y')],proj4string=CRS('+proj=utm +zone=10'))
  cps.sptr <- spTransform(cps,CRS(proj4string(t.dnbr)))
  cpts <- coordinates(cps.sptr)
  
  plot(t.dnbr)
  points(cpts)
  
  plot(k.dnbr)
  points(cpts)

  t.rdnbr.x <- extract(t.rdnbr,cpts)
  t.dnbr.x <- extract(t.dnbr,cpts)
  t.dnbr6.x <- extract(t.dnbr6,cpts)
  k.rdnbr.x <- extract(k.rdnbr,cpts)
  k.dnbr.x <- extract(k.dnbr,cpts)
  k.dnbr6.x <- extract(k.dnbr6,cpts)
  pairs(as.matrix(cbind(t.rdnbr.x,t.dnbr.x,t.dnbr6.x,k.rdnbr.x,k.dnbr.x,k.dnbr6.x)))
  
  if (f<3) {
    fs <- data.frame(Plot=cp$Plot,x=cpts[,1],y=cpts[,2],Tubbs.mtbs.RDNBR=t.rdnbr.x,Tubbs.mtbs.DNBR=t.dnbr.x,Tubbs.mtbs.DNBR6=t.dnbr6.x,Kincade.mtbs.RDNBR=k.rdnbr.x,Kincade.mtbs.DNBR=k.dnbr.x,Kincade.mtbs.DNBR6=k.dnbr6.x)
  } else {
    fs <- data.frame(Plot=cp$Plot,Quad=cp$Quad,x=cpts[,1],y=cpts[,2],Tubbs.mtbs.RDNBR=t.rdnbr.x,Tubbs.mtbs.DNBR=t.dnbr.x,Tubbs.mtbs.DNBR6=t.dnbr6.x,Kincade.mtbs.RDNBR=k.rdnbr.x,Kincade.mtbs.DNBR=k.dnbr.x,Kincade.mtbs.DNBR6=k.dnbr6.x)
  }
  mx <- 140
  if (f==1) mx <- 141 else if (f==4) mx <- 139
  write.csv(fs,paste('data/FSextract/MTBS-',substr(fname,126,mx),'.csv',sep=''))
}

## Tukman burn damage


# Clark
system('cat bigdata/Clark/readme.txt')

r <- raster('bigdata/Clark/Tubbs/LC08_CU_001008_20170925_201701027_RBR.tif')
r
plot(r)
hist(getValues(r))
proj4string(r)

f=1
for (f in 2:3) {
  fname <- paste(prefix,fnames[f],sep='')
  
  cp <- read.csv(text=getURL(fname, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  head(cp)  
  
  # convert cp to proper projection
  cps <- SpatialPoints(cp[,c('UTM.x','UTM.y')],proj4string=CRS('+proj=utm +zone=10'))
  cps.sptr <- spTransform(cps,CRS(proj4string(r)))
  cpts <- coordinates(cps.sptr)
  
  plot(r)
  points(cpts,pch=19,col='red')
  
  tubbs <- extract(r,cpts)
  
  if (f<3) {
    fs <- data.frame(Plot=cp$Plot,x=cpts[,1],y=cpts[,2],Tubbs.clark.RBR=tubbs)
  } else {
    fs <- data.frame(Plot=cp$Plot,Quad=cp$Quad,x=cpts[,1],y=cpts[,2],Tubbs.clark.RBR=tubbs)
  }
  mx <- 140
  if (f==1) mx <- 141 else if (f==4) mx <- 139
  write.csv(fs,paste('data/FSextract/ClarkRBR-',substr(fname,126,mx),'.csv',sep=''))
}

# USFS BARC fire severity
system('cat bigdata/BARC/Readme.txt')

r <- raster('bigdata/BARC/Tubbs/barc256_s220170927_s220171017_20m.tif')
r
plot(r)
hist(getValues(r))
proj4string(r)

f=1
for (f in 1:3) {
  fname <- paste(prefix,fnames[f],sep='')
  
  cp <- read.csv(text=getURL(fname, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  head(cp)  
  
  # convert cp to proper projection
  cps <- SpatialPoints(cp[,c('UTM.x','UTM.y')],proj4string=CRS('+proj=utm +zone=10'))
  cps.sptr <- spTransform(cps,CRS(proj4string(r)))
  cpts <- coordinates(cps.sptr)
  
  plot(r)
  points(cpts,pch=19,col='red')
  
  tubbs <- extract(r,cpts)
  
  if (f<3) {
    fs <- data.frame(Plot=cp$Plot,x=cpts[,1],y=cpts[,2],Tubbs.barc.dNBR17=tubbs)
  } else {
    fs <- data.frame(Plot=cp$Plot,Quad=cp$Quad,x=cpts[,1],y=cpts[,2],Tubbs.barc.dNBR17=tubbs)
  }
  mx <- 140
  if (f==1) mx <- 141 else if (f==4) mx <- 139
  write.csv(fs,paste('data/FSextract/BARC20-256-',substr(fname,126,mx),'.csv',sep=''))
}

## extract 5 m for veg plot quads
r2 <- raster('bigdata/BARC/Tubbs/barc256_s220170927_spot20171018_5m.tif')
r2
hist(getValues(r2))
plot(r2)

f=4
for (f in 4:4) {
  fname <- paste(prefix,fnames[f],sep='')
  
  cp <- read.csv(text=getURL(fname, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  head(cp)  
  
  # convert cp to proper projection
  cps <- SpatialPoints(cp[,c('UTM.x','UTM.y')],proj4string=CRS('+proj=utm +zone=10'))
  cps.sptr <- spTransform(cps,CRS(proj4string(r2)))
  cpts <- coordinates(cps.sptr)
  
  #plot(r2)
  points(cpts,pch=19,col='red')
  
  tubbs <- extract(r2,cpts)
  
  if (f<3) {
    fs <- data.frame(Plot=cp$Plot,x=cpts[,1],y=cpts[,2],Tubbs.barc.dNDVI17=tubbs)
  } else {
    fs <- data.frame(Plot=cp$Plot,Quad=cp$Quad,x=cpts[,1],y=cpts[,2],Tubbs.barc.dNDVI17=tubbs)
  }
  mx <- 140
  if (f==1) mx <- 141 else if (f==4) mx <- 139
  
  write.csv(fs,paste('data/FSextract/BARC5-256-',substr(fname,126,mx),'.csv',sep=''))
  
  # how much variation among plots
  mval <- tapply(fs$Tubbs.barc.dNDVI17,fs$Plot,min,na.rm=T)
  mxval <- tapply(fs$Tubbs.barc.dNDVI17,fs$Plot,max,na.rm=T)
  mnval <- tapply(fs$Tubbs.barc.dNDVI17,fs$Plot,mean,na.rm=T)
  length(mnval)
  plot(mnval,(mxval-mval))
  
  dNDVImean <- tapply(fs$Tubbs.barc.dNDVI17,fs$Plot,mean,na.rm=T)
  fname <- paste(prefix,fnames[2],sep='')
  cp54 <- read.csv(text=getURL(fname, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  dim(cp54)
  head(cp54)
    
  fs <- data.frame(Plot=cp54$Plot,x=cp54$UTM.x,y=cp54$UTM.y,Tubbs.barc.dNDVImn5=mnval)
  head(fs)
  dim(fs)
  write.csv(fs,paste('data/FSextract/BARC5mn-256-',substr(fname,126,140),'.csv',sep=''))
}


# Soil Burn Severity - from Tukman gis files
system('cat bigdata/SBS/Readme.txt')

r2 <- raster('bigdata/SBS/SoilBurnSeverity/Tubbs_SBS.tif')
r2
plot(r2)
hist(getValues(r2))
proj4string(r2)

## extract 5 m for veg plot quads and average
f <- 4
fname <- paste(prefix,fnames[f],sep='')

cp <- read.csv(text=getURL(fname, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
head(cp)  

# convert cp to proper projection
cps <- SpatialPoints(cp[,c('UTM.x','UTM.y')],proj4string=CRS('+proj=utm +zone=10'))
cps.sptr <- spTransform(cps,CRS(proj4string(r2)))
cpts <- coordinates(cps.sptr)

plot(r2)
points(cpts,pch=19,col='red')

tubbs <- extract(r2,cpts)

if (f<3) {
  fs <- data.frame(Plot=cp$Plot,x=cpts[,1],y=cpts[,2],Tubbs.SBS=tubbs)
} else {
  fs <- data.frame(Plot=cp$Plot,Quad=cp$Quad,x=cpts[,1],y=cpts[,2],Tubbs.SBS=tubbs)
}
mx <- 140
if (f==1) mx <- 141 else if (f==4) mx <- 139

write.csv(fs,paste('data/FSextract/SBS5-256-',substr(fname,126,mx),'.csv',sep=''))

# how much variation among plots
mval <- tapply(fs$Tubbs.SBS,fs$Plot,min,na.rm=T)
mxval <- tapply(fs$Tubbs.SBS,fs$Plot,max,na.rm=T)
mnval <- tapply(fs$Tubbs.SBS,fs$Plot,mean,na.rm=T)
mnval[is.nan(mnval)] <- NA
par(mar=c(5,5,1,1))
plot(mnval,(mxval-mval))

fname <- paste(prefix,fnames[2],sep='')
cp54 <- read.csv(text=getURL(fname, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
head(cp54)
cp54$SBSmn <- mnval
head(cp54)
fs <- data.frame(Plot=cp54$Plot,x=cp54$UTM.x,y=cp54$UTM.y,Tubbs.SBS=mnval)
head(fs)
dim(fs)
write.csv(fs,paste('data/FSextract/SBS5mn-256-',substr(fname,126,140),'.csv',sep=''))
