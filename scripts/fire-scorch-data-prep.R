## read in post-fire survey data

## file directory
## data/Post-fire-plot-surveys
# FireSurveysNov2017 - pdf scans of field sheets
# dfDensity.csv - number of stems of SE, JU, TR at 5, 10 and 30 cm dbh cutoffs, and TR.LA (don't know what that is): I don't know what survey these data were extracted from
# MH-DA-plot-damage.csv - combined file with extraction of remote sensing values and summary damage data for MH plots, VegPlots, and Rx - another pepperwood study
# plot-species-ba-SATR.csv - 2014 basal area by plot-species, for SA and TR, with % scorch values for trees from post-fire rapid surveys
# plot-scorch-wtd-means.csv - wtd mean scorch - species level scorch values, wtd by basal area. Column 2 is dNBR
# plot-scorch-data-combined.csv - all data combined from different sources
# 

rm(list=ls())
source('scripts/Load.data.R')
fnames <- c('hectares-18-100m-cpt.csv','vegplots-54-20m-cpt.csv','hectares-18-20m-cpt.csv','vegplots-54-5m-cpt.csv')

# Create combined file for 20m vegplots
# Read in locations - these are centerpoints
## load plot center coordinate data from VegPlots master git repo
options(stringsAsFactors=FALSE) 
prefix='https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/GIS/2021_updated_shapefiles_and_coordinates/shapefiles/'

fname <- paste(prefix,fnames[2],sep='')

cp <- read.csv(text=getURL(fname, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
head(cp)  
tail(cp)
dim(cp)

## read in weighted canopy scorch
ps <- read.csv('data/Post-fire-plot-surveys/plot-scorch-wtd-means.csv')
head(ps)
tail(ps)

cp$plot.scorch <- ps$plot.scorch[match(cp$Plot,ps$X)]
head(cp)
tail(cp)
dim(cp)

## COMBINE EXTRACTED fire severity data
(vpf <- dir('data/FSextract/',pattern = '54-20'))

dr <- read.csv(paste('data/FSextract/',vpf[1],sep=''))
dim(dr)
head(dr)

names(dr)

fsCols <- sort(c(grep('Tubbs',names(dr)),grep('Kincade',names(dr))))

## Now look at data sampled at 5m quadrats to see how similar
(vpf <- dir('data/FSextract/',pattern = '54-5'))

dr5 <- read.csv(paste('data/FSextract/',vpf[1],sep=''))
dim(dr5)
head(dr5)
(fsCols5 <- sort(c(grep('Tubbs',names(dr5)),grep('Kincade',names(dr5)))))

dr5m <- data.frame(Plot=dr$Plot)
names(dr5m)

# check col aligment, for extra 'X' cols created by csv write/read
names(dr5)
for (i in 5:22) {
  dr5m <- cbind(dr5m,tapply(dr5[,i],dr5$Plot,mean,na.rm=T))
}
names(dr5m)[2:19] <- paste(names(dr5)[5:22],'qm',sep='.')
head(dr5m)
tail(dr5m)

## compare values sampled at midPlot to those averaged over 16 quads
names(dr)
names(dr5m)
<<<<<<< HEAD

for (i in 2:19) print(cor(dr5m[,i],dr[,i+2],use='pair'))
# plot lowest correlation
plot(dr5m[,2],dr[,4])
plot(dr5m[,16],dr[,18])
plot(dr5m[,17],dr[,19])
plot(dr5m[,19],dr[,21])

# combine all data, using qm averages for precision
fsCols <- sort(c(grep('Tubbs',names(dr5m)),grep('Kincade',names(dr5m))))

=======

for (i in 2:19) print(cor(dr5m[,i],dr[,i+2],use='pair'))
# plot lowest correlation
plot(dr5m[,2],dr[,4])
plot(dr5m[,16],dr[,18])
plot(dr5m[,17],dr[,19])
plot(dr5m[,19],dr[,21])

# combine all data, using qm averages for precision
fsCols <- sort(c(grep('Tubbs',names(dr5m)),grep('Kincade',names(dr5m))))

cp <- cbind(cp,dr5m[,fsCols])
head(cp)
dim(cp)

tcols <- c(which(names(cp)=='plot.scorch'),grep('Tubbs',names(cp)))
pairs(cp[,tcols[1:7]])

# MTBS RdNBR and dNBR are essentially identical, so picking RdNBR
# prefer to use continuous over categorical - can always make categorical later
pairs(cp[,tcols[c(1,2,5:7)]])
cor(cp[,tcols[c(1,2,5:7)]],use='pair')

# BARC dNBR not well correlated with others - drop for now
pairs(cp[,tcols[c(1,2,5:6)]])
cor(cp[,tcols[c(1,2,5:6)]],use='pair')

# MTBS RdNBR and dNBR are essentiallyidentical, so picking RdNBR
# prefer to use continuous over categorical - can always make categorical later
pairs(cp[,tcols[c(1,2,5:7)]])
cor(cp[,tcols[c(1,2,5:7)]],use='pair')

# BARC dNBR not well correlated with others - drop for now
pairs(cp[,tcols[c(1,2,5:6)]])
cor(cp[,tcols[c(1,2,5:6)]],use='pair')

# check by calculating average correlation of each parameter against the others
ctable <- cor(cp[,tcols],use='pair')
apply(ctable,1,mean)

pc <- princomp(cp[1:50,tcols[c(1,2,6)]],cor = TRUE)
biplot(pc,xlim=c(-0.3,0.65),ylim=c(-0.4,0.6))
abline(h=0,lty=2);abline(v=0,lty=2)
pc$scores[,c(1,2)]

pc2 <- princomp(cp[,tcols[c(2:4,6)]],cor = TRUE)
pc2
biplot(pc2,xlim=c(-0.3,0.5),ylim=c(-0.3,0.4))
abline(h=0,lty=2);abline(v=0,lty=2)

names(cp)[tcols]
names(cp)

# FINAL Selection: 
# plot.scorch
# MTBS.RdNBR.30.qm
# Kincade.RdNBR.30.qm
# Tubbs.Tukman.CanDam.10.qm
# Tubbs.Clark.RBR.30.qm

names(cp)[c(2,3,4,9,10,13,16,17)]
write.csv(cp[,c(2,3,4,9,10,13,16,17)],'data/FS_selected.csv')
