setwd("~/Dropbox/CSI-LIMNO_DATA/LAGOSData/Version1.054.1")
data = read.table("lagos_epi_nutr_10541.txt", 
                  header = TRUE, 
                  sep = "\t", 
                  quote = "", 
                  dec = ".", 
                  strip.white = TRUE, 
                  comment.char = "", 
                  colClasses=c(sampledate = "POSIXct"))
data.lake.specific = read.table("lagos_lakes_10541.txt", 
                                header = TRUE, 
                                sep = "\t", 
                                quote = "", 
                                dec = ".", 
                                strip.white = TRUE,
                                comment.char = "")
#Calculate TN from TKN
data$tn_calculated = data$tkn + data$no2no3

for (i in 1:nrow(data)){
  if (is.na(data$tn[i]) == TRUE) {
    data$tn_combined[i] = data$tn_calculated[i]
  } else {
    data$tn_combined[i] = data$tn[i]
  }
}

#create a data subset which only includes instances where
#both TP and TN_combined exist

stoich = data[is.na(data$tn_combined)==FALSE & is.na(data$tp)==FALSE, ]

#create a column tn_tp=TN:TP ratio (by mass)
stoich$tn_tp = stoich$tn_combined/stoich$tp

#create molar version of ratio and nutrients
stoich$tn_tp_umol = stoich$tn_tp * (30.97/14.01)
stoich$tn_umol = stoich$tn_combined/14.01
stoich$tp_umol = stoich$tp/30.97

#limit observations from JUN 15-AUG 15
# first, just keep samples in JUN, JUL, AUG, SEP
stoich.summer = stoich[stoich$samplemonth == 6|stoich$samplemonth == 7|stoich$samplemonth==8|stoich$samplemonth==9, ]

# now, get rid of first half of June and second half of Sept
stoich.summer$sampleday = format(stoich.summer$sampledate, "%d")
stoich.summer$sampleday = as.numeric(stoich.summer$sampleday)

keep.june = which(stoich.summer$samplemonth == 6 & stoich.summer$sampleday >= 15)
keep.july = which(stoich.summer$samplemonth == 7)
keep.august = which(stoich.summer$samplemonth == 8)
keep.september = which(stoich.summer$samplemonth==9 & stoich.summer$sampleday <= 15)
keep.all = c(keep.june, keep.july, keep.august, keep.september)
keep.all = as.numeric(keep.all)

stoich.summer = stoich.summer[keep.all, ]

#remove all points where TN:TP > 1000. This is based on data from Downing & McCauley, where
#the max epilimnetic value from the world's lakes was ~ 552. A rough doubling of that gives TN:TP (molar)
#of 1,000, to be conservative. This gets rid of 64 observations

stoich.summer = stoich.summer[stoich.summer$tn_tp_umol < 1000, ]

#a function that allows you to calculate SE (and other stats)
#by groups in a data frame
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

#find mean, coefficient of variance (covar) and number of observations
#for TN:TP, TN, TP in each lake for every year of observation
intra.annual.tntp = aggregate(stoich.summer$tn_tp_umol, stoich.summer[,c("lagoslakeid", "sampleyear")], FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), nobs=length(x)))
intra.annual.tn = aggregate(stoich.summer$tn_umol, stoich.summer[,c("lagoslakeid", "sampleyear")], FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), nobs=length(x)))
intra.annual.tp = aggregate(stoich.summer$tp_umol, stoich.summer[,c("lagoslakeid", "sampleyear")], FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), nobs=length(x)))

year.means.tntp = data.frame(lagoslakeid = intra.annual.tntp$lagoslakeid, 
                        sampleyear = intra.annual.tntp$sampleyear, 
                        tn_tp_umol = intra.annual.tntp$x[,1], 
                        sd = intra.annual.tntp$x[,2], 
                        covar = intra.annual.tntp$x[,3], 
                        nobs = intra.annual.tntp$x[,4])
year.means.tn = data.frame(lagoslakeid = intra.annual.tn$lagoslakeid, 
                             sampleyear = intra.annual.tn$sampleyear, 
                             tn_umol = intra.annual.tn$x[,1], 
                             sd = intra.annual.tn$x[,2], 
                             covar = intra.annual.tn$x[,3], 
                             nobs = intra.annual.tn$x[,4])
year.means.tp = data.frame(lagoslakeid = intra.annual.tp$lagoslakeid, 
                           sampleyear = intra.annual.tp$sampleyear, 
                           tp_umol = intra.annual.tp$x[,1], 
                           sd = intra.annual.tp$x[,2], 
                           covar = intra.annual.tp$x[,3], 
                           nobs = intra.annual.tp$x[,4])

year.means = data.frame(lagoslakeid = year.means.tntp$lagoslakeid, 
                        sampleyear = year.means.tntp$sampleyear, 
                        tn_umol = year.means.tn$tn_umol,
                        tp_umol = year.means.tp$tp_umol,
                        tn_tp_umol = year.means.tntp$tn_tp_umol, 
                        nobs = year.means.tntp$nobs)
#limit analysis to only 1990-present
modern = year.means[year.means$sampleyear > 1989, ]

#limit analysis to only lakes that have >= 15 years of data post-1990
#first, calculate how many years of observation each year has (plus calculate mean stoich over all years)
duration = aggregate(modern$tn_tp_umol, by=list(modern$lagoslakeid), FUN=function(x) c(mean=mean(x),sd=sd(x),covar=sd(x)/mean(x), nobs=length(x)))

#create a list of lagoslakeid of each lake that has >= 15 years of data
modern.15 = modern[modern$lagoslakeid %in% duration$Group.1[duration$x[,4]>=15], ]

# create a mixed model where the nutrient is the response, year is the 
# the predictor, and slopes and intercepts are allowed to vary by lake (grouping)

library(lme4)
library(MuMIn)
library(arm)
library(effects)

modern.15$sampleyear_cor = modern.15$sampleyear - 1990

## create a column where tn, tp, tn:tp are standardized by the 
## initial (1990 or first year) concentration in the lake

for (i in 1:length(modern.15$lagoslakeid)){
  lake = modern.15$lagoslakeid[i]
  modern.15$tn_umol_stand[i] = (modern.15$tn_umol[i] - modern.15$tn_umol[modern.15$lagoslakeid == lake & modern.15$sampleyear_cor == min(modern.15$sampleyear_cor[modern.15$lagoslakeid == lake])])/modern.15$tn_umol[modern.15$lagoslakeid == lake & modern.15$sampleyear_cor == min(modern.15$sampleyear_cor[modern.15$lagoslakeid == lake])] 
  modern.15$tp_umol_stand[i] = (modern.15$tp_umol[i] - modern.15$tp_umol[modern.15$lagoslakeid == lake & modern.15$sampleyear_cor == min(modern.15$sampleyear_cor[modern.15$lagoslakeid == lake])])/modern.15$tp_umol[modern.15$lagoslakeid == lake & modern.15$sampleyear_cor == min(modern.15$sampleyear_cor[modern.15$lagoslakeid == lake])] 
  modern.15$tn_tp_umol_stand[i] = (modern.15$tn_tp_umol[i] - modern.15$tn_tp_umol[modern.15$lagoslakeid == lake & modern.15$sampleyear_cor == min(modern.15$sampleyear_cor[modern.15$lagoslakeid == lake])])/modern.15$tn_tp_umol[modern.15$lagoslakeid == lake & modern.15$sampleyear_cor == min(modern.15$sampleyear_cor[modern.15$lagoslakeid == lake])] 
}

## run models with standardized data to make slopes directly comparable
tn.model.stand = lmer(tn_umol_stand ~ sampleyear_cor + (sampleyear_cor|lagoslakeid), data = modern.15, REML=FALSE)
tn.model.stand.int = lmer(tn_umol_stand ~ sampleyear_cor + (1|lagoslakeid), data = modern.15, REML=FALSE)
tp.model.stand = lmer(tp_umol_stand ~ sampleyear_cor + (sampleyear_cor|lagoslakeid), data = modern.15, REML=FALSE)
tntp.model.stand = lmer(tn_tp_umol_stand ~ sampleyear_cor + (sampleyear_cor|lagoslakeid), data = modern.15, REML=FALSE)

tn.model.stand.uncorr = lmer(tn_umol_stand ~ sampleyear_cor + (1|lagoslakeid) + (0+sampleyear_cor|lagoslakeid), data = modern.15, REML=FALSE)

## extract random coefficients

blup.tn.stand = coef(tn.model.stand)
blup.tp.stand = coef(tp.model.stand)
blup.tntp.stand = coef(tntp.model.stand)

## plot change in TN vs change in TP

plot(blup.tn.stand$lagoslakeid[,2]~blup.tp.stand$lagoslakeid[,2], 
     xlab = "% Change in TP per year", ylab = "% Change in TN per year")
abline(0,1)
abline(h=0, col = "red")
abline(v=0, col = "red")

## create a histogram of slopes for the change in TP and TN
pdf("hist_TN_TP_change.pdf")
hist(blup.tn.stand$lagoslakeid[,2], breaks = 20, col=rgb(.2,.5,.5,.5), 
     ylim = c(0,25), main = "", xlab = "% Change per year", ylab = "Number of lakes")
hist(blup.tp.stand$lagoslakeid[,2], breaks = 20, col=rgb(.5,.2,.2,0.5), add = TRUE)
legend(0.03, 20, c("TN", "TP"), fill= c(rgb(.2,.5,.5,.5), rgb(.5,.2,.2,0.5)))
dev.off()

## identify lakes that have slopes different than zero
## by using 1.96*SE of each BLUP

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
pdf("TN_TP_change.pdf")
plot(blup.tn.stand$lagoslakeid[,2]~blup.tp.stand$lagoslakeid[,2], 
     xlab = "% Change in TP per year", 
     ylab = "% Change in TN per year", 
     cex = 1.5, cex.lab = 1.2)
abline(0,1)
abline(h=0, col = rgb(.2,.5,.5), lwd = 3)
abline(v=0, col = rgb(.5,.2,.5), lwd = 3)

#first plot points where TN is different from zero
points(blup.tn.stand$lagoslakeid[,2][tn.diff.zero==TRUE]~blup.tp.stand$lagoslakeid[,2][tn.diff.zero==TRUE], 
       col = rgb(.2,.5,.5, .5), pch=16, cex = 1.5)
points(blup.tn.stand$lagoslakeid[,2][tp.diff.zero==TRUE]~blup.tp.stand$lagoslakeid[,2][tp.diff.zero==TRUE], 
       col = rgb(.5,.2,.5, .5), pch=16, cex=1.5)
points(blup.tn.stand$lagoslakeid[,2][tn.diff.zero==TRUE & tp.diff.zero==TRUE]~blup.tp.stand$lagoslakeid[,2][tn.diff.zero==TRUE& tp.diff.zero==TRUE], 
       col = rgb(.2,.2,.2,.5), pch=16, cex = 1.5)
dev.off()

# Create a map that plots change by location

require(maps)

map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE,col = "gray")
tn.blups = data.frame(lagoslakeid = rownames(blup.tn.stand$lagoslakeid), intercepts = blup.tn.stand$lagoslakeid[[1]], slopes = blup.tn.stand$lagoslakeid[[2]])
locations = data.lake.specific[,c(1,3,4)]
tn.blups = merge(tn.blups, locations, by = "lagoslakeid", all.x=TRUE)
tp.blups = data.frame(lagoslakeid = rownames(blup.tp.stand$lagoslakeid), intercepts = blup.tp.stand$lagoslakeid[[1]], slopes = blup.tp.stand$lagoslakeid[[2]])

#first plot points where TN is different from zero
points(tn.blups$nhd_long[tn.diff.zero==TRUE], tn.blups$nhd_lat[tn.diff.zero==TRUE],
       col = rgb(.2,.5,.5, .5), pch=16, cex = 1.5)
points(blup.tn.stand$lagoslakeid[,2][tp.diff.zero==TRUE]~blup.tp.stand$lagoslakeid[,2][tp.diff.zero==TRUE], 
       col = rgb(.5,.2,.5, .5), pch=16, cex=1.5)
points(blup.tn.stand$lagoslakeid[,2][tn.diff.zero==TRUE & tp.diff.zero==TRUE]~blup.tp.stand$lagoslakeid[,2][tn.diff.zero==TRUE& tp.diff.zero==TRUE], 
       col = rgb(.2,.2,.2,.5), pch=16, cex = 1.5)

#######################
## Model validation
#######################

## create individual linear models to validate mixed model estimates
lakes = unique(modern.15$lagoslakeid)
intercept = c()
slope = c()
p.val = c()
for (i in 1:length(unique(modern.15$lagoslakeid))) {
  model = lm(tn_umol_stand[lagoslakeid==lakes[i]]~sampleyear_cor[lagoslakeid==lakes[i]], data = modern.15)
  intercept[i] = summary(model)$coefficients[1, 1]
  slope[i] = summary(model)$coefficients[2, 1]
  p.val[i] = summary(model)$coefficients[2, 4]
}

## from the simple linear models, 27/90 of TN models have significant change
## this number does not change when you use standardized data 

## check whether the lm outputs are related to the lmer outputs

tn.lm = data.frame(lagoslakeid = lakes, lm.intercepts = intercept, lm.slopes = slope)
lmer.vs.lm = merge(tn.lm, tn.blups, by = "lagoslakeid")

#slopes
plot(slopes~lm.slopes, data = lmer.vs.lm)
hist(lmer.vs.lm$slopes, col = rgb(.5, .2, .2, .5), breaks = 20)
hist(lmer.vs.lm$lm.slopes, col = rgb(.2,.2,.2, .5), breaks = 20, add = TRUE)

#intercepts
plot(intercepts~lm.intercepts, data=lmer.vs.lm)
abline(0,1)

#look at how slopes and intercepts are related in mixed and linear models
plot(lm.slopes~lm.intercepts, data=lmer.vs.lm)
plot(slopes~intercepts, data=lmer.vs.lm)

## ---------------------------------------------------
## Evaluate R2 of the mixed model
## conditional R2 = 
## marginal R2 = 

r.squaredGLMM(tn.model.stand)


## ---------------------------------------------------
## Test H0: is the variance of the random effect == 0?
## uses package RLRsim to test random effects

install.packages("RLRsim")
library(RLRsim)

# mA is the full model
mA = lmer(tn_umol_stand~sampleyear_cor + (1|lagoslakeid) + (0+sampleyear_cor|lagoslakeid), data = modern.15)
# m0 is the full model minus the effect being tested
m0 = update(mA, . ~ . - (0+sampleyear_cor|lagoslakeid))
# m.slope is the model with only the effect being tested
m.slope = update(mA, . ~ . - (1|lagoslakeid))
# note that the exctRLRT function must list models in specific order
test = exactRLRT(m.slope, mA, m0)




