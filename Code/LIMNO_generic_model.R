# load required packages
# 
library(lme4)
library(MuMIn)
library(arm)
library(effects)
require(maps)
library(merTools)
library(ggplot2)
library(RLRsim)

# load data that has already been filtered to meet the timeseries requirements
setwd("C:/Users/Samantha/Dropbox/CSI_LIMNO_Manuscripts-presentations/CSI_Nitrogen MSs/Time series/Data")

data.tn = read.table("timeseries_data_tn.txt", header = TRUE)
data.tp = read.table("timeseries_data_tp.txt", header = TRUE)
data.tntp = read.table("timeseries_data_tntp.txt", header = TRUE)
data.chl = read.table("timeseries_data_chl.txt", header = TRUE)


# merge data with HUC 4 regions

setwd("C:/Users/Samantha/Dropbox/CSI-LIMNO_DATA/LAGOSGeoData/LAGOS_VER1.03")
lake.info = read.table("lagoslakes_10400.txt", header = TRUE,
                  sep = "\t", 
                  quote = "", 
                  dec = ".", 
                  strip.white = TRUE, 
                  comment.char = "")

data.tn = merge(data.tn, lake.info[,c("lagoslakeid", "hu4_zoneid")])
data.tp = merge(data.tp, lake.info[,c("lagoslakeid", "hu4_zoneid")])
data.tntp = merge(data.tntp, lake.info[,c("lagoslakeid", "hu4_zoneid")])
data.chl = merge(data.chl, lake.info[,c("lagoslakeid", "hu4_zoneid")])


#2-level unconditional
model.2u <- function(nutrient, data) {
  return(lmer(log(nutrient) ~ 1 + (1|lagoslakeid), data = data, REML=TRUE))
}

tn.2u = model.2u(data.tn$tn_umol, data.tn)
tp.2u = model.2u(data.tp$tp_umol, data.tp)
tntp.2u = model.2u(data.tntp$tn_tp_umol, data.tntp)
chl.2u = model.2u(data.chl$chl, data.chl)


#3-level unconditional
model.3u <- function(nutrient, data) {
  return(lmer(log(nutrient) ~ 1 + (1|lagoslakeid) + (1|hu4_zoneid), data = data, REML=TRUE))
}

tn.3u = model.3u(data.tn$tn_umol, data.tn)
tp.3u = model.3u(data.tp$tp_umol, data.tp)
tntp.3u = model.3u(data.tntp$tn_tp_umol, data.tntp)
chl.3u = model.3u(data.chl$chl, data.chl)

#3-level, random intercepts + fixed time effect

model.3f <- function(nutrient, data) {
  return(lmer(log(nutrient) ~ sampleyear_cor + (1|lagoslakeid) + (1|hu4_zoneid), data = data, REML=TRUE))
}

tn.3f = model.3f(data.tn$tn_umol, data.tn)
tp.3f = model.3f(data.tp$tp_umol, data.tp)
tntp.3f = model.3f(data.tntp$tn_tp_umol, data.tntp)
chl.3f = model.3f(data.chl$chl, data.chl)

#3-level, random intercepts + fixed time effect + random time:lake effect

model.3fr2 <- function(nutrient, data) {
  return(lmer(log(nutrient) ~ sampleyear_cor + (sampleyear_cor||lagoslakeid) + (1|hu4_zoneid), data = data, REML=TRUE))
}

tn.3fr2 = model.3fr2(data.tn$tn_umol, data.tn)
tp.3fr2 = model.3fr2(data.tp$tp_umol, data.tp)
tntp.3fr2 = model.3fr2(data.tntp$tn_tp_umol, data.tntp)
chl.3fr2 = model.3fr2(data.chl$chl, data.chl)

#3-level, random intercepts + fixed time effect + random time:region

model.3fr3 <- function(nutrient, data) {
  return(lmer(log(nutrient) ~ sampleyear_cor + (1|lagoslakeid) + (sampleyear_cor||hu4_zoneid), data = data, REML=TRUE))
}

tn.3fr3 = model.3fr3(data.tn$tn_umol, data.tn)
tp.3fr3 = model.3fr3(data.tp$tp_umol, data.tp)
tntp.3fr3 = model.3fr3(data.tntp$tn_tp_umol, data.tntp)
chl.3fr3 = model.3fr3(data.chl$chl, data.chl)

#3-level, random intercepts + fixed time effect + random time:region + random time:lake

model.3fr23 <- function(nutrient, data) {
  return(lmer(log(nutrient) ~ sampleyear_cor + (sampleyear_cor||lagoslakeid) + (sampleyear_cor||hu4_zoneid), data = data, REML=TRUE))
}

tn.3fr23 = model.3fr23(data.tn$tn_umol, data.tn)
tp.3fr23 = model.3fr23(data.tp$tp_umol, data.tp)
tntp.3fr23 = model.3fr23(data.tntp$tn_tp_umol, data.tntp)
chl.3fr23 = model.3fr23(data.chl$chl, data.chl)


# test whether random effects should be included

## TEST: first test time:lagoslakeid

# full model nutrient ~ sampleyear_cor + (sampleyear_cor||lagoslakeid) + (sampleyear_cor||hu4_zoneid)

#m = only random effect you want to test
#mA = full model
#m0 = all random except one you want to test

model.m <- function(nutrient, data) {
  return(lmer(log(nutrient) ~ sampleyear_cor + (0+sampleyear_cor|lagoslakeid), data = data, REML=TRUE))
}

tn.m <- model.m(data.tn$tn_umol, data.tn)
tp.m <- model.m(data.tp$tp_umol, data.tp)
tntp.m <- model.m(data.tntp$tn_tp_umol, data.tntp)
chl.m <- model.m(data.chl$chl, data.chl)

model.m0 <- function(nutrient, data) {
  return(lmer(log(nutrient) ~ sampleyear_cor + (1|lagoslakeid) + (sampleyear_cor||hu4_zoneid), data = data, REML=TRUE))
}

tn.m0 = model.m0(data.tn$tn_umol, data.tn)
tp.m0 <- model.m0(data.tp$tp_umol, data.tp)
tntp.m0 <- model.m0(data.tntp$tn_tp_umol, data.tntp)
chl.m0 <- model.m0(data.chl$chl, data.chl)

#bootstrap model test

tn.r2.test = exactRLRT(tn.m, tn.3fr23, tn.m0)
tp.r2.test = exactRLRT(tp.m, tp.3fr23, tp.m0)
tntp.r2.test = exactRLRT(tntp.m, tntp.3fr23, tntp.m0)
chl.r2.test = exactRLRT(chl.m, chl.3fr23, chl.m0)

## RESULTS: in all models, the random effect time:lake is significant

## TEST: whether time|region should be included as a random effect

model.m <- function(nutrient, data) {
  return(lmer(log(nutrient) ~ sampleyear_cor + (0+sampleyear_cor|hu4_zoneid), data = data, REML=TRUE))
}

tn.m <- model.m(data.tn$tn_umol, data.tn)
tp.m <- model.m(data.tp$tp_umol, data.tp)
tntp.m <- model.m(data.tntp$tn_tp_umol, data.tntp)
chl.m <- model.m(data.chl$chl, data.chl)

model.m0 <- function(nutrient, data) {
  return(lmer(log(nutrient) ~ sampleyear_cor + (sampleyear_cor||lagoslakeid) + (1|hu4_zoneid), data = data, REML=TRUE))
}

tn.m0 = model.m0(data.tn$tn_umol, data.tn)
tp.m0 <- model.m0(data.tp$tp_umol, data.tp)
tntp.m0 <- model.m0(data.tntp$tn_tp_umol, data.tntp)
chl.m0 <- model.m0(data.chl$chl, data.chl)

#bootstrap model test

tn.r3.test = exactRLRT(tn.m, tn.3fr23, tn.m0)
tp.r3.test = exactRLRT(tp.m, tp.3fr23, tp.m0)
tntp.r3.test = exactRLRT(tntp.m, tntp.3fr23, tntp.m0)
chl.r3.test = exactRLRT(chl.m, chl.3fr23, chl.m0)

## RESULTS: all time:region effects are significant

## Extract fixed and random effects, as well as variance components,
## from simulations that allow calculation of variance for each parameter

mod.resim <- function(mod, dat) {
  
  ran = REsim(mod,n.sims=1000)
  fix = FEsim(mod,n.sims=1000)
  lake.ran = subset(ran, groupFctr=="lagoslakeid")
  region.ran = subset(ran, groupFctr=="hu4_zoneid")
  names(lake.ran)[2] = "lagoslakeid"
  names(region.ran)[c(2)] = "hu4_zoneid"
  region.ran = region.ran[,c(2:6)]
  names(region.ran)[3:5] = c("hu4_mean", "hu4_med", "hu4_sd")
  lake.regions = unique(dat[,c('lagoslakeid', 'hu4_zoneid')])
  lake.ran = merge(lake.ran, lake.regions)
  lake.ran = merge(lake.ran, region.ran, by = c("hu4_zoneid", "term"), all.x = TRUE)
  
  #calculate lake-specific intercept and slope that includes fixed and random (lake and region) effects
  lake.ran$coef_mean[lake.ran$term=="(Intercept)"] = lake.ran$mean[lake.ran$term=="(Intercept)"] + lake.ran$hu4_mean[lake.ran$term=="(Intercept)"] + fix$mean[1]
  lake.ran$coef_mean[lake.ran$term=="sampleyear_cor"] = lake.ran$mean[lake.ran$term=="sampleyear_cor"] + lake.ran$hu4_mean[lake.ran$term=="sampleyear_cor"] + fix$mean[2]
  #calculate lake-specific sd of random effects that includes sd of fixed and random effects
  lake.ran$coef_sd[lake.ran$term=="(Intercept)"] = sqrt((lake.ran$sd[lake.ran$term=="(Intercept)"])^2 + (lake.ran$hu4_sd[lake.ran$term=="(Intercept)"])^2 + (fix$sd[1])^2)
  lake.ran$coef_sd[lake.ran$term=="sampleyear_cor"] = sqrt((lake.ran$sd[lake.ran$term=="sampleyear_cor"])^2 + (lake.ran$hu4_sd[lake.ran$term=="sampleyear_cor"])^2 + (fix$sd[2])^2)
  
  #calculate region-specific intercept and slope that includes fixed and random (region) effects
  region.ran$coef_mean[region.ran$term=="(Intercept)"] = region.ran$hu4_mean[region.ran$term=="(Intercept)"]  + fix$mean[1]
  region.ran$coef_mean[region.ran$term=="sampleyear_cor"] = region.ran$hu4_mean[region.ran$term=="sampleyear_cor"] + fix$mean[2]

  #calculate region-specific sd of random effects that includes sd of fixed and random effects
  region.ran$coef_sd[region.ran$term=="(Intercept)"] = sqrt((region.ran$hu4_sd[region.ran$term=="(Intercept)"])^2 + (fix$sd[1])^2)
  region.ran$coef_sd[region.ran$term=="sampleyear_cor"] = sqrt((region.ran$hu4_sd[region.ran$term=="sampleyear_cor"])^2 + (fix$sd[2])^2)
  
  #find the bounds of the 90% CI
  lake.ran$ymin = lake.ran$coef_mean - (qnorm(.95)*lake.ran$coef_sd)
  lake.ran$ymax = lake.ran$coef_mean + (qnorm(.95)*lake.ran$coef_sd)
  
  region.ran$ymin = region.ran$coef_mean - (qnorm(.95)*region.ran$coef_sd)
  region.ran$ymax = region.ran$coef_mean + (qnorm(.95)*region.ran$coef_sd)

  return(list(lake.ran, region.ran, ran, fix))
}

# 1 = lake-specific random effects
# 2 = region-specific random effects
# 3 = random effects
# 4 = fixed effects

change.db.tn = mod.resim(tn.3fr23, data.tn)
change.db.tp = mod.resim(tp.3fr23, data.tp)
change.db.tntp = mod.resim(tntp.3fr23, data.tntp)       
change.db.chl = mod.resim(chl.3fr23, data.chl)

# plotting data from mod.resim

# look at random effects:

plotREsim(change.db.chl[[3]][change.db.chl[[3]]$term=="sampleyear_cor",])

tn.01 = lmer(log(tn_umol) ~ 1 + (1|hu4_zoneid), data = data.tn, REML = TRUE)


par(mfrow=c(2,2))
plot(fitted(tn.full),resid(tn.full,type="pearson"),col="blue")
abline(h=0,lwd=2)
qqnorm(resid(tn.full)) #normality of the residuals
qqline(resid(tn.full))

tn.02 = lmer(log(tn_umol) ~ 1 + (1|lagoslakeid) + (1|hu4_zoneid), data = data.tn, REML=TRUE)
tn.01b = lmer(log(tn_umol) ~ 1 + (1|hu4_zoneid), data = data.tn, REML = TRUE)
exactLRT(m = tn.02, m0 = tn.01)
# compare random effects structures between lagoslakeid and adding region

b.01 <- bootMer(tn.01, use.u = FALSE, type = "parametric", FUN = function(x) as.numeric(logLik(x)), nsim = 1000)
b.02 <- bootMer(tn.02, FUN = function(x) as.numeric(logLik(x)), nsim = 1000, use.u = FALSE, type = "parametric")

# the observed LRT value
lrt <- as.numeric(-2 * logLik(tn.01) + 2 * logLik(tn.02))
# the 100 bootstrapped LRT
lrt.b <- -2 * b.01$t + 2 * b.02$t

# plot
quant <- quantile(lrt.b, probs = c(0.025, 0.975))
plot(1, lrt, xlab = "", ylab = "Likelihood ratio test", 
     xaxt = "n", ylim = c(quant[1] + 1, quant[2] + 1))
abline(h = 0, lty = 2, lwd = 2, col = "red")
segments(1, quant[1], 1, quant[2], lend = 1)
#2-level with time

model.1 <- function(nutrient, data) {
  return(lmer(log(nutrient) ~ sampleyear_cor + (sampleyear_cor||lagoslakeid), data = data))
}

tn.1 = model.1(data.tn$tn_umol, data.tn)
tp.1 = model.1(data.tp$tp_umol, data.tp)

plotREsim(REsim(tn.01, n.sims = 1000))


#3-level unconditional
model.02 <- function(nutrient, data) {
  return(lmer(log(nutrient) ~ 1 + (1|lagoslakeid) + (1|hu4), data = data))
}

tn.02 = model.02(data.tn$tn_umol, data.tn)
tp.02 = model.02(data.tp$tp_umol, data.tp)


#3-level with time

# get huc4 IDs into databases
huc4 = data.lake.specific[,c("lagoslakeid", "hu4")]
data.tn = merge(data.tn, huc4, by = "lagoslakeid", all.x = TRUE)
data.tp = merge(data.tp, huc4, by = "lagoslakeid", all.x = TRUE)

model.2 <- function(nutrient, data) {
  return(lmer(log(nutrient) ~ sampleyear_cor + (sampleyear_cor||lagoslakeid) + (sampleyear_cor||hu4), data = data))
}

tn.2 = model.2(data.tn$tn_umol, data.tn)
tp.2 = model.2(data.tp$tp_umol, data.tp)






