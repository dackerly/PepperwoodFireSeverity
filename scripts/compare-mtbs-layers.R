rm(list=ls())
library(raster)

rdnbr <- raster('bigdata/mtbs/2017_Tubbs/ca3859812261820171009/ca3859812261820171009_20170707_20180710_rdnbr.tif')
rdnbr
proj4string(rdnbr)

tmv <- getValues(rdnbr)
tmv[which(tmv<0)] <- NA
hist(tmv)
tmv[which(tmv>1500)] <- 1500
hist(tmv)

rdnbr2 <- setValues(rdnbr,tmv)
plot(rdnbr2)

dnbr<- raster('bigdata/mtbs/2017_Tubbs/ca3859812261820171009/ca3859812261820171009_20170707_20180710_dnbr.tif')
tmv <- getValues(dnbr)
hist(tmv)
tmv[which(tmv<(-5000))] <- NA
hist(tmv)
tmv[which(tmv>1500)] <- 1500
hist(tmv)
dnbr2 <- setValues(dnbr,tmv)
plot(dnbr2)

plot(getValues(rdnbr2),getValues(dnbr2))
abline(0,1)

dnbr6<- raster('bigdata/mtbs/2017_Tubbs/ca3859812261820171009/ca3859812261820171009_20170707_20180710_dnbr6.tif')


r <- raster('bigdata/BARC/Tubbs/Tubbs_burn_severity_20171018_UTM.tif')
plot(r)

s <- raster('bigdata/BARC/Tubbs/barc256_s220170927_s220171017_20m.tif')
plot(s)

par(mfrow=c(1,2))
plot(r)
plot(s)
par(mfrow=c(1,1))
