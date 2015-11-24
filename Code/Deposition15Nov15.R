setwd("~/Dropbox/CSI/CSI-LIMNO_DATA/LAGOSGeoData/LAGOS_VER1.03")

##pull HUC12 ID and TN/NO3 deposition 2005 data from CHAG export 

HUC12CHAG = read.table("hu12_chag.txt", 
                 header = TRUE, 
                 sep = "\t", 
                 quote = "", 
                 dec = ".", 
                 strip.white = TRUE, 
                 comment.char = "",
                 as.is=TRUE)

##take out each year separately to add column for year for models

depo.1990<-HUC12CHAG[c("hu12_zoneid","hu12_dep_totaln_1990_mean", "hu12_dep_no3_1990_mean")]
depo.1995<-HUC12CHAG[c("hu12_zoneid","hu12_dep_totaln_1995_mean", "hu12_dep_no3_1995_mean")]
depo.2000<-HUC12CHAG[c("hu12_zoneid","hu12_dep_totaln_2000_mean", "hu12_dep_no3_2000_mean")]
depo.2005<-HUC12CHAG[c("hu12_zoneid","hu12_dep_totaln_2005_mean", "hu12_dep_no3_2005_mean")]
depo.2010<-HUC12CHAG[c("hu12_zoneid","hu12_dep_totaln_2010_mean", "hu12_dep_no3_2010_mean")]

huc12l<-1:20257
yr.1990<-rep(1990, length(huc12l))
yr.1995<-rep(1995, length(huc12l))
yr.2000<-rep(2000, length(huc12l))
yr.2005<-rep(2005, length(huc12l))
yr.2010<-rep(2010, length(huc12l))

new.1990<-cbind(depo.1990, yr.1990)
new.1995<-cbind(depo.1995, yr.1995)
new.2000<-cbind(depo.2000, yr.2000)
new.2005<-cbind(depo.2005, yr.2005)
new.2010<-cbind(depo.2010, yr.2010)

names(new.1990)<-c("hu12_zoneid", "TNDepo", "NO3Depo", "Year")
names(new.1995)<-c("hu12_zoneid", "TNDepo", "NO3Depo", "Year")
names(new.2000)<-c("hu12_zoneid", "TNDepo", "NO3Depo", "Year")
names(new.2005)<-c("hu12_zoneid", "TNDepo", "NO3Depo", "Year")
names(new.2010)<-c("hu12_zoneid", "TNDepo", "NO3Depo", "Year")

all.depo<-rbind(new.1990, new.1995, new.2000, new.2005, new.2010)
all.depo.nona<-na.omit(all.depo)


#start year at zero for models
all.depo.nona$year.zeroed<- all.depo.nona$Year - 1990

# create a mixed model where the temperature or precip is the response, year is the predictor, and slopes and intercepts are allowed to vary by hu12 (can't vary by lake because there are no IWS climate data)

library(lme4)
library(MuMIn)
library(arm)
library(effects)

#slope
tn.depo.model = lmer(scale(TNDepo) ~ scale(year.zeroed) + (scale(year.zeroed)|hu12_zoneid), data = all.depo.nona, REML=FALSE)
#intercept
tn.depo.model.int = lmer(scale(TNDepo) ~ scale(year.zeroed) + (1|hu12_zoneid), data = all.depo.nona, REML=FALSE)

#slope
no3.depo.model = lmer(scale(NO3Depo) ~ scale(year.zeroed) + (scale(year.zeroed)|hu12_zoneid), data = all.depo.nona, REML=FALSE)
#intercept
no3.depo.model.int = lmer(scale(NO3Depo) ~ scale(year.zeroed) + (1|hu12_zoneid), data = all.depo.nona, REML=FALSE)



##try it with subset of hu12s that are included in limno models, (get from lakes_hu12id_forgeo script)

hu12list<-read.csv("hu12withlimnodata.csv", header=T)
hu12list$hu12_zoneid<-hu12list$x

limnorelevant.depo<-merge(hu12list, all.depo.nona, by="hu12_zoneid", all.x=TRUE, all.y=FALSE)

limnorelevant.depo$X=NULL
limnorelevant.depo$x=NULL


##rerun models with just limno data hu12s

#slope
tn.depo.model.limno = lmer(scale(TNDepo) ~ scale(year.zeroed) + (scale(year.zeroed)|hu12_zoneid), data = limnorelevant.depo, REML=FALSE)
#intercept
tn.depo.model.limno.int = lmer(scale(TNDepo) ~ scale(year.zeroed) + (1|hu12_zoneid), data = limnorelevant.depo, REML=FALSE)

#slope
no3.depo.model.limno = lmer(scale(NO3Depo) ~ scale(year.zeroed) + (scale(year.zeroed)|hu12_zoneid), data = limnorelevant.depo, REML=FALSE)
#intercept
no3.depo.model.limno.int = lmer(scale(NO3Depo) ~ scale(year.zeroed) + (1|hu12_zoneid), data = limnorelevant.depo, REML=FALSE)
