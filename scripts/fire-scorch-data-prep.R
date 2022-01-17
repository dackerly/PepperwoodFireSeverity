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
prefix='https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/2021/2021_updated_shapefiles_and_coordinates/shapefiles/'

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
i=2
for (i in 2:length(vpf)) 
{
  dx <- read.csv(paste('data/FSextract/',vpf[i],sep=''))
  print(dim(dx))
  namehold <- names(dr)
  dr <- cbind(dr,dx[,-c(1:4)])
  head(dr)
  names(dr) <- c(namehold,names(dx)[-c(1:4)])
}
head(dr)
dim(dr)
tcols <- grep('Tubbs',names(dr))
pairs(dr[,tcols])

# combine all data
cp <- cbind(cp,dr[,-c(1:4)])
head(cp)
dim(cp)

tcols <- c(which(names(cp)=='plot.scorch'),grep('Tubbs',names(cp)))
pairs(cp[,tcols])
cor(cp[,tcols],use='pair')

ctable <- cor(cp[,tcols],use='pair')
apply(ctable,1,mean)

plot(cp$plot.scorch,cp$Tubbs.mtbs.DNBR6,xlim=c(-5,100),ylim=c(-0.1,4.1))
table(cp$Tubbs.mtbs.DNBR6)

pc <- princomp(cp[1:50,tcols[1:7]],cor = TRUE)
biplot(pc,xlim=c(-0.3,0.6),ylim=c(-0.4,0.6))
abline(h=0,lty=2);abline(v=0,lty=2)
pc$scores[,c(1,2)]

pc2 <- princomp(cp[,tcols[2:7]],cor = TRUE)
biplot(pc2,xlim=c(-0.3,0.5),ylim=c(-0.3,0.4))
abline(h=0,lty=2);abline(v=0,lty=2)
