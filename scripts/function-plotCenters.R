## function to load data points for fire severity data extraction
# center points for vegplots 20 m plots

# file name strings used in this function
#fnames <- c('hectares-18-100m-cpt.csv','vegplots-54-20m-cpt.csv','hectares-18-20m-cpt.csv','vegplots-54-5m-cpt.csv')
plotCenters <- function(fname='vegplots-54-20m-cpt.csv',plotSize=20)
{
  library(sp)

  cp <- list()
  ## load plot center coordinate data from VegPlots master git repo
  options(stringsAsFactors=FALSE) 
  prefix='https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/GIS/2021_updated_shapefiles_and_coordinates/shapefiles/'
  
  fn <- paste(prefix,fname,sep='')
  df <- read.csv(text=getURL(fn, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  head(df)
  
  cp[[1]] <- df
  
  # convert cp to proper projection
  cps <- SpatialPoints(df[,c('UTM.x','UTM.y')],proj4string=CRS('+proj=utm +zone=10'))
  cp[[2]] <- cps
  
  cp[[3]] <- plotSize
  
  return(cp)
}
