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
getwd()
ps <- read.csv('data/Post-fire-plot-surveys/plot-scorch-data-combined.csv',as.is=T)
names(ps)
