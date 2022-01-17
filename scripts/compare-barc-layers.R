library(raster)
od <- getwd()
setwd('/Volumes/Google\ Drive/My\ Drive/My_Drive_Cloud/Drive-Projects/BayArea/SonomaFires-2017/gis/Tukman/DOC/Tubbs/BARC')
(fn <- dir(pattern = '.tif'))

b1 <- raster(fn[1])
b1
plot(b1)

b2 <- raster(fn[2])
b2
plot(b2)

b4 <- raster(fn[4])
b4
plot(b4)

b7 <- raster(fn[7])
b7
plot(b7)

b11 <- raster(fn[11])
b11
plot(b11)

setwd(od)
