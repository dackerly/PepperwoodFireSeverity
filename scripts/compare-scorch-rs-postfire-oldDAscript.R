## compare field scorch with remote sensing

getwd()
fs <- read.csv('/Volumes/Google Drive/My Drive/My_Drive_Cloud/Drive-Projects/Pepperwood/Fire_2017/Post-fire-field-projects/Pepperwood-Fire-Rapid-Surveys/Post-fire-plot-surveys/plot-scorch-data-combined.csv',as.is=T)
head(fs)
tail(fs)

names(fs)
pairs(fs[,c('MT_RBR_MEAN','MT_Canopy_Damage_Percent_MEAN','MT_Soil_Burn_Severity_MEAN','RBR','dNBR','plot.scorch')])

pairs(fs[,c('MT_Soil_Burn_Severity_MEAN','MT_RBR_MEAN','MT_Canopy_Damage_Percent_MEAN','plot.scorch')])
cor(fs[,c('MT_Soil_Burn_Severity_MEAN','MT_RBR_MEAN','MT_Canopy_Damage_Percent_MEAN','plot.scorch')],use='pair')
cor(fs[,c('MT_Soil_Burn_Severity_MEAN','MT_RBR_MEAN','MT_Canopy_Damage_Percent_MEAN','plot.scorch')],use='pair',method='spearman')

plot(fs[,c('MT_RBR_MEAN','MT_Canopy_Damage_Percent_MEAN')],pch=19,cex=1.5)
op=par(mfrow=c(1,2))
plot(fs[,c('MT_Canopy_Damage_Percent_MEAN','plot.scorch')],pch=19,cex=1.5)
plot(fs[,c('MT_RBR_MEAN','plot.scorch')],pch=19,cex=1.5)
par(op)

plot(fs$plot.scorch,5-fs$TR.weighted.mean,ylim=c(1,4),xlab='Plot scorch (immediate post-fire)',ylab='Tree damage severity score',pch=19,cex=1.5)
fit <- lm(I(5-fs$TR.weighted.mean)~fs$plot.scorch)
abline(fit)
summary(fit)

plot(fs$MT_RBR_MEAN,5-fs$TR.weighted.mean,ylim=c(1,4),xlab='RBR',ylab='Tree damage severity score',pch=19,cex=1.5)
fit <- lm(I(5-fs$TR.weighted.mean)~fs$MT_RBR_MEAN)
abline(fit)
summary(fit)

plot(fs$MT_Canopy_Damage_Percent_MEAN,5-fs$TR.weighted.mean,ylim=c(1,4),xlab='Canopy damage',ylab='Tree damage severity score',pch=19,cex=1.5)
fit <- lm(I(5-fs$TR.weighted.mean)~fs$MT_Canopy_Damage_Percent_MEAN)
abline(fit)
summary(fit)

plot(fs$MT_Canopy_Damage_Percent_MEAN,100*(fs$TR_dead/fs$TR_total),xlab='Canopy damage',ylab='% tree mortality',pch=19,cex=1.5)
fit <- lm(I(5-fs$TR.weighted.mean)~fs$MT_Canopy_Damage_Percent_MEAN)
abline(fit)
summary(fit)

plot(fs$MT_Canopy_Damage_Percent_MEAN,fs$TR.weighted.mean)
plot(fs$plot.scorch,fs$SA.weighted.mean)

# now look at Tukman data at all Pepperwood survey points
md <- read.csv('/Users/david/Google Drive/Drive-Projects/Pepperwood/Fire_2017/Post-fire-field-projects/Pepperwood-Fire-Rapid-Surveys/Post-fire-plot-surveys/MH-DA-plot-damage.csv',as.is=T)
dim(md)
names(md)
plot(md$RBR_MEAN,md$Canopy_Damage_Percent_MEAN)
boxplot(md$Canopy_Damage_Percent_MEAN~md$NPSBurnSevVegetation)



# now look at all Tukman's data
rs <- read.csv('/Volumes/Google Drive/My Drive/My_Drive_Cloud/Drive-Projects/Pepperwood/Fire_2017/Tukman_rs_damage/Pepperwood_Plots_Canopy_Damage_Polygons.csv',as.is=T)
head(rs)
dim(rs)
rsamp <- sample(nrow(rs),1000)
plot(rs[rsamp,c('Canopy_Damage_Percent_MEAN','RBR_MEAN')])
     