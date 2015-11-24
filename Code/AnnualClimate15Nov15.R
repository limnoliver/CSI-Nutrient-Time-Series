setwd("~/Dropbox/CSI/CSI_LIMNO_Manuscripts-presentations/CSI_Nitrogen MSs/Time series/GeoTS")
temp = read.csv("HU12_ppt_merge.csv", 
                  header = TRUE, 
                  sep = ",", 
                  quote = "", 
                  dec = ".", 
                  strip.white = TRUE, 
                  comment.char = "")
precip = read.csv("HU12_tmean_merge.csv", 
                                header = TRUE, 
                                sep = ",", 
                                quote = "", 
                                dec = ".", 
                                strip.white = TRUE,
                                comment.char = "")
                                
temp$ann.mean<-rowMeans(temp[,3:14])
precip$ann.mean<-rowMeans(precip[,3:14])

temp<-temp[-c(3:14)]
precip<-precip[-c(3:14)]

temp.1990<- temp[temp$Year > 1989, ]
precip.1990<- precip[precip$Year > 1989, ]

#start year at zero for models
temp.1990$year.zeroed<- temp.1990$Year - 1990
precip.1990$year.zeroed<- precip.1990$Year - 1990

# create a mixed model where the temperature or precip is the response, year is the predictor, and slopes and intercepts are allowed to vary by hu12 (can't vary by lake because there are no IWS climate data)

library(lme4)
library(MuMIn)
library(arm)
library(effects)

#slope
temp.1990.model = lmer(scale(ann.mean) ~ scale(year.zeroed) + (scale(year.zeroed)|ZoneID), data = temp.1990, REML=FALSE)
#intercept
temp.1990.model.int = lmer(scale(ann.mean) ~ scale(year.zeroed) + (1|ZoneID), data = temp.1990, REML=FALSE)

#slope
precip.1990.model = lmer(scale(ann.mean) ~ scale(year.zeroed) + (scale(year.zeroed)|ZoneID), data = precip.1990, REML=FALSE)
#intercept
precip.1990.model.int = lmer(scale(ann.mean) ~ scale(year.zeroed) + (1|ZoneID), data = precip.1990, REML=FALSE)

##neither slope model converges, try it with subset of hu12s that are included in limno models, (get from lakes_hu12id_forgeo script)

hu12list<-read.csv("hu12withlimnodata.csv", header=T)
hu12list$ZoneID<-hu12list$x

limnorelevant.temp<-merge(hu12list, temp.1990, by="ZoneID", all.x=TRUE, all.y=FALSE)

limnorelevant.temp$X=NULL
limnorelevant.temp$x=NULL

limnorelevant.precip<-merge(hu12list, precip.1990, by="ZoneID", all.x=TRUE, all.y=FALSE)

limnorelevant.precip$X=NULL
limnorelevant.precip$x=NULL


##rerun models with just limno data hu12s

#start year at zero for models
limnorelevant.temp$year.zeroed<- limnorelevant.temp$Year - 1990
limnorelevant.precip$year.zeroed<- limnorelevant.precip$Year - 1990

#slope
temp.1990.model.limno = lmer(scale(ann.mean) ~ scale(year.zeroed) + (scale(year.zeroed)|ZoneID), data = limnorelevant.temp, REML=FALSE)
#intercept
temp.1990.model.limno.int = lmer(scale(ann.mean) ~ scale(year.zeroed) + (1|ZoneID), data = limnorelevant.temp, REML=FALSE)

#slope
precip.1990.model.limno = lmer(scale(ann.mean) ~ scale(year.zeroed) + (scale(year.zeroed)|ZoneID), data = limnorelevant.precip, REML=FALSE)
#intercept
precip.1990.model.limno.int = lmer(scale(ann.mean) ~ scale(year.zeroed) + (1|ZoneID), data = limnorelevant.precip, REML=FALSE)


## extract random coefficients

annual.temp.all = coef(temp.1990.model)
annual.precip.all = coef(precip.1990.model)
annual.temp.limnno = coef(temp.1990.model.limno)
annual.precip.limnno = coef(precip.1990.model.limno)



##are slopes different than zero

## create a histogram of slopes for the change in TP and TN
hist(annual.temp.all$ZoneID[,2], breaks = 20, col=rgb(.2,.5,.5,.5), 
     ylim = c(0,2000), main = "", xlab = "% Change per year", ylab = "Number of lakes")
hist(annual.precip.all$ZoneID[,2], breaks = 20, col=rgb(.2,.5,.5,.5), 
     ylim = c(0,3000), main = "", xlab = "% Change per year", ylab = "Number of lakes")
hist(annual.temp.limnno$ZoneID[,2], breaks = 20, col=rgb(.5,.2,.2,0.5), ylim = c(0,10), main = "", xlab = "% Change per year", ylab = "Number of lakes")
hist(annual.precip.limnno$ZoneID[,2], breaks = 20, col=rgb(.5,.2,.2,0.5), ylim = c(0,10), main = "", xlab = "% Change per year", ylab = "Number of lakes")

load("~/Dropbox/CSI/CSI_LIMNO_Manuscripts-presentations/CSI_Nitrogen MSs/Time series/Code/stoich_mixedmodel_19NOV15.RData")

#compare climate slopes to nutrient slopes

#data frame with all hu12 and lakeid info
lakeid.hu12.zones<-data.lake.specific[,c(1,3,4,20)]

#make everything into data frames instead of lists to merge
tp.df<-data.frame(lagoslakeid=rownames(blup.tp.stand$lagoslakeid), intercepts.tp=blup.tp.stand$lagoslakeid[[1]], slopes.tp = blup.tp.stand$lagoslakeid[[2]])
tn.df<-data.frame(lagoslakeid=rownames(blup.tn.stand$lagoslakeid), intercepts.tn=blup.tn.stand$lagoslakeid[[1]], slopes.tn = blup.tn.stand$lagoslakeid[[2]])
temp.df<-data.frame(ZoneID=rownames(annual.temp.limnno$ZoneID), intercepts.temp=annual.temp.limnno$ZoneID[[1]], slopes.temp = annual.temp.limnno$ZoneID[[2]])
precip.df<-data.frame(ZoneID=rownames(annual.precip.limnno$ZoneID), intercepts.precip=annual.precip.limnno$ZoneID[[1]], slopes.precip = annual.precip.limnno$ZoneID[[2]])

with.tp<-merge(tp.df, lakeid.hu12.zones, by="lagoslakeid", all.x=TRUE, all.y=FALSE)
with.tp.tn<-merge(with.tp, tn.df, by="lagoslakeid", all.x=TRUE, all.y=TRUE)
with.tp.tn.temp<-merge(with.tp.tn, temp.df, by="ZoneID", all.x=TRUE, all.y=TRUE)
with.tp.tn$ZoneID<-with.tp.tn$hu12_zoneid
with.tp.tn.temp.precip<-merge(with.tp.tn.temp, precip.df, by="ZoneID", all.x=TRUE, all.y=TRUE)

plot(slopes.precip~slopes.tn, data = with.tp.tn.temp.precip)
plot(slopes.precip~slopes.tp, data = with.tp.tn.temp.precip)
plot(slopes.temp~slopes.tp, data = with.tp.tn.temp.precip)
plot(slopes.temp~slopes.tn, data = with.tp.tn.temp.precip)

plot(slopes.tp~intercepts.tp, data=with.tp.tn.temp.precip)
plot(slopes.temp~intercepts.temp, data=with.tp.tn.temp.precip)

## why are slopes and intercepts perfectly correlated??  try other approach, including separate random
#terms for slope and intercept
temp.1990.model.limno.noint = lmer(scale(ann.mean) ~ scale(year.zeroed) + (0+scale(year.zeroed)|ZoneID), data = limnorelevant.temp, REML=FALSE)
temp.df.noint<-data.frame(ZoneID=rownames(annual.temp.limnno$ZoneID), intercepts.temp=annual.temp.limnno$ZoneID[[1]], slopes.temp = annual.temp.limnno$ZoneID[[2]])

hist(temp.df.noint$slopes.temp)

plot(intercepts.temp~slopes.temp, data=temp.df.nocorr)

#nostand model

tp.df.nostand<-data.frame(lagoslakeid=rownames(blup.tp$lagoslakeid), intercepts.tp=blup.tp$lagoslakeid[[1]], slopes.tp = blup.tp$lagoslakeid[[2]])
plot(intercepts.tp~slopes.tp, data=tp.df.nostand)

## this means we can't use intercepts from here, use beginnign concentration or mean nut
#to figure out whether tp slope is related to tp concentration in the lake

## identify lakes that have slopes different than zero

blup = coef(tn.model.stand)
blup.se = se.ranef(tn.model.stand)
blup.low = list()
blup.high = list()
n.diff.mean = list()
n.diff.zero = list()
prop.non.overlap = c()
prop.diff.zero = c()
for (i in 1:2) {
  blup.low[[i]] = as.numeric(blup$lagoslakeid[,i] - (1.96*blup.se$lagoslakeid[,i]))
  blup.high[[i]] = as.numeric(blup$lagoslakeid[,i] + (1.96*blup.se$lagoslakeid[,i]))
  grand.low = as.numeric(fixef(tn.model)[i])-(1.96*as.numeric(se.fixef(tn.model)[i]))
  grand.high = as.numeric(fixef(tn.model)[i])+(1.96*as.numeric(se.fixef(tn.model)[i]))
  n.diff.mean[[i]] =  (blup.low[[i]]>=grand.low&blup.low[[i]]<=grand.high)|(blup.high[[i]]>=grand.low&blup.high[[i]]<=grand.high)|(blup.low[[i]]<=grand.low&blup.high[[i]]>=grand.high)
  n.diff.zero[[i]]  = blup.low[[i]]>0|blup.high[[i]]<0
  prop.non.overlap[i]=length(which(n.diff.mean[[i]]==FALSE))/length(n.diff.mean[[i]])
  prop.diff.zero[i] = length(which(n.diff.zero[[i]]==TRUE))/length(n.diff.zero[[i]])
}

## save a list identifying which slopes are different from zero
## from both the TN and TP models

tn.diff.zero = n.diff.zero[[2]]
tp.diff.zero = n.diff.zero[[2]]

## create a plot of TN ~ TP slopes, where color is dependent on whether 
## TN (green), TP (red), or TN & TP (brown) are different from zero

plot(blup.tn.stand$lagoslakeid[,2]~blup.tp.stand$lagoslakeid[,2], 
     xlab = "% Change in TP per year", 
     ylab = "% Change in TN per year", 
     cex = 1.5, cex.lab = 1.2)
abline(0,1)
abline(h=0, col = rgb(.2,.5,.5), lwd = 3)
abline(v=0, col = rgb(.5,.2,.5), lwd = 3)


