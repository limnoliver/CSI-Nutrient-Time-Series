# load required packages
library(lme4)
library(RLRsim)
library(merTools)
library(arm)
library(randomForest)

# ===============================================
# PURPOSE:
# this code models nutrient (TN, TP, TN:TP) and chlorophyll 
# change through time using 3-level hierarchical models
# Additionally, to assess the drivers of nutrient and chlorophyll trends,
# this code uses random forest to classify lakes as either
# increasing, decreasing or not changing for each response variable
# ================================================
# Data import and cleaning

# modify below code to source published LTER data
# but for now, data will look like: 

infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/333/3/e955b964c44276632edd9f3629022077" 
infile1 <- sub("^https","http",infile1) 
all.nut.data <-read.csv(infile1, header=F, skip=1, sep=",",  
                        col.names=c("lagoslakeid",     
                                    "sampleyear",     
                                    "value",     
                                    "variable"), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(all.nut.data$lagoslakeid)!="factor") all.nut.data$lagoslakeid<- as.factor(all.nut.data$lagoslakeid)
if (class(all.nut.data$value)=="factor") all.nut.data$value <-as.numeric(levels(all.nut.data$value))[as.integer(all.nut.data$value) ]
if (class(all.nut.data$variable)!="factor") all.nut.data$variable<- as.factor(all.nut.data$variable)

# create a sample year column corrected to 1990

all.nut.data$sampleyear_cor = all.nut.data$sampleyear - 1990

# create individual dataframes that represent each nutrient

data.tn = subset(all.nut.data, all.nut.data$variable == "tn_umol")
data.tp = subset(all.nut.data, all.nut.data$variable == "tp_umol")
data.tntp = subset(all.nut.data, all.nut.data$variable == "tn_tp_umol")
data.chl = subset(all.nut.data, all.nut.data$variable == "chl_ugL")

# merge data with HUC 4 regions
# modify below code to source LTER geo data

infile2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/333/3/cc35382bc48fc688750d0c358167f3e1" 
infile2 <- sub("^https","http",infile2) 
lake.info <-read.csv(infile2,header=F, skip=1, sep=",", 
                           col.names=c("lagoslakeid",     
                                       "hu4_zoneid",     
                                       "iws_zoneid",     
                                       "nhd_lat",     
                                       "nhd_long",     
                                       "lake_area_ha",     
                                       "maxdepth",     
                                       "iws_areaha",     
                                       "lakeconnectivity",     
                                       "iws_slope_mean",     
                                       "iws_urban",     
                                       "iws_crop",     
                                       "iws_pasture",     
                                       "iws_forest",     
                                       "iws_lakes_lakes4ha_overlapping_area_pct",     
                                       "iws_lakes_lakes4ha_isolated_overlapping_area_pct",     
                                       "iws_lakes_lakes4ha_drlakestream_overlapping_area_pct",     
                                       "iws_streamdensity_headwaters_density_mperha",     
                                       "iws_streamdensity_midreaches_density_mperha",     
                                       "iws_streamdensity_rivers_density_mperha",     
                                       "hu4_baseflowindex_mean",     
                                       "hu4_runoff_mean",     
                                       "hu4_dep_totaln_1990_mean",     
                                       "hu4_dep_totaln_2010_mean",     
                                       "hu4_dep_change",     
                                       "hu4_prism_ppt_30yr_normal_800mm2_annual_mean",     
                                       "hu4_prism_tmean_30yr_normal_800mm2_annual_mean",     
                                       "hu4_tmean_1990",     
                                       "hu4_tmean_2011",     
                                       "hu4_tmean_change",     
                                       "hu4_ppt_1990",     
                                       "hu4_ppt_2011",     
                                       "hu4_ppt_change",     
                                       "hu4_damdensity_pointspersqkm",     
                                       "hu4_roaddensity_density_mperha",     
                                       "hu4_slope_mean",     
                                       "hu4_urban",     
                                       "hu4_crop",     
                                       "hu4_pasture",     
                                       "hu4_forest",     
                                       "hu4_lakes_lakes4ha_avgsize_ha",     
                                       "hu4_lakes_lakes4ha_overlapping_area_pct",     
                                       "hu4_lakes_lakes4ha_isolated_overlapping_area_pct",     
                                       "hu4_lakes_lakes4ha_drlakestream_overlapping_area_pct",     
                                       "hu4_latewisconsinglaciation_glaciation"), check.names=TRUE)

data.tn = merge(data.tn, lake.info[,c("lagoslakeid", "hu4_zoneid")])
data.tp = merge(data.tp, lake.info[,c("lagoslakeid", "hu4_zoneid")])
data.tntp = merge(data.tntp, lake.info[,c("lagoslakeid", "hu4_zoneid")])
data.chl = merge(data.chl, lake.info[,c("lagoslakeid", "hu4_zoneid")])

names(data.tn)[3] = "tn_umol"
names(data.tp)[3] = "tp_umol"
names(data.tntp)[3] = "tn_tp_umol"
names(data.chl)[3] = "chl_ugL"

# =======================================
# create hierarchical linear models

# 3-level unconditional
# used to generate results of Table S2

model.3u <- function(nutrient, data) {
  return(lmer(log(nutrient) ~ 1 + (1|lagoslakeid) + (1|hu4_zoneid), data = data, REML=TRUE))
}

tn.3u = model.3u(data.tn$tn_umol, data.tn)
tp.3u = model.3u(data.tp$tp_umol, data.tp)
tntp.3u = model.3u(data.tntp$tn_tp_umol, data.tntp)
chl.3u = model.3u(data.chl$chl_ugL, data.chl)

# 3-level, random intercepts + fixed year effect + random year:region + random year:lake
# Main model used throughout paper to detect trends in nutrients and chl

model.3fr23 <- function(nutrient, data) {
  return(lmer(log(nutrient) ~ sampleyear_cor + (sampleyear_cor||lagoslakeid) + (sampleyear_cor||hu4_zoneid), data = data, REML=TRUE))
}

tn.3fr23 = model.3fr23(data.tn$tn_umol, data.tn)
tp.3fr23 = model.3fr23(data.tp$tp_umol, data.tp)
tntp.3fr23 = model.3fr23(data.tntp$tn_tp_umol, data.tntp)
chl.3fr23 = model.3fr23(data.chl$chl_ugL, data.chl)

# ===================================================
# test if random effects are appropriate

# TEST: first test year|lagoslakeid

# m = only random effect you want to test
# mA = full model
# m0 = all random except one you want to test

model.m <- function(nutrient, data) {
  return(lmer(log(nutrient) ~ sampleyear_cor + (0+sampleyear_cor|lagoslakeid), data = data, REML=TRUE))
}

tn.m <- model.m(data.tn$tn_umol, data.tn)
tp.m <- model.m(data.tp$tp_umol, data.tp)
tntp.m <- model.m(data.tntp$tn_tp_umol, data.tntp)
chl.m <- model.m(data.chl$chl_ugL, data.chl)

model.m0 <- function(nutrient, data) {
  return(lmer(log(nutrient) ~ sampleyear_cor + (1|lagoslakeid) + (sampleyear_cor||hu4_zoneid), data = data, REML=TRUE))
}

tn.m0 = model.m0(data.tn$tn_umol, data.tn)
tp.m0 <- model.m0(data.tp$tp_umol, data.tp)
tntp.m0 <- model.m0(data.tntp$tn_tp_umol, data.tntp)
chl.m0 <- model.m0(data.chl$chl_ugL, data.chl)

# bootstrap model test
# full model refers to '3fr23' models created above
# that include random effects at lake and region level

tn.r2.test = exactRLRT(tn.m, tn.3fr23, tn.m0)
tp.r2.test = exactRLRT(tp.m, tp.3fr23, tp.m0)
tntp.r2.test = exactRLRT(tntp.m, tntp.3fr23, tntp.m0)
chl.r2.test = exactRLRT(chl.m, chl.3fr23, chl.m0)

# RESULTS: in all models, the random effect time:lake is significant

# TEST2: whether year|region should be included as a random effect

model.m <- function(nutrient, data) {
  return(lmer(log(nutrient) ~ sampleyear_cor + (0+sampleyear_cor|hu4_zoneid), data = data, REML=TRUE))
}

tn.m <- model.m(data.tn$tn_umol, data.tn)
tp.m <- model.m(data.tp$tp_umol, data.tp)
tntp.m <- model.m(data.tntp$tn_tp_umol, data.tntp)
chl.m <- model.m(data.chl$chl_ugL, data.chl)

model.m0 <- function(nutrient, data) {
  return(lmer(log(nutrient) ~ sampleyear_cor + (sampleyear_cor||lagoslakeid) + (1|hu4_zoneid), data = data, REML=TRUE))
}

tn.m0 <- model.m0(data.tn$tn_umol, data.tn)
tp.m0 <- model.m0(data.tp$tp_umol, data.tp)
tntp.m0 <- model.m0(data.tntp$tn_tp_umol, data.tntp)
chl.m0 <- model.m0(data.chl$chl_ugL, data.chl)

# bootstrap model test

tn.r3.test <- exactRLRT(tn.m, tn.3fr23, tn.m0)
tp.r3.test <- exactRLRT(tp.m, tp.3fr23, tp.m0)
tntp.r3.test <- exactRLRT(tntp.m, tntp.3fr23, tntp.m0)
chl.r3.test <- exactRLRT(chl.m, chl.3fr23, chl.m0)

# RESULTS: all time:region effects are significant

# =====================================================
# Extract fixed and random effects, as well as variance components,
# from posterior simulations. Use simulartions to estimate uncertainty around
# fixed and random coefficients

mod.resim <- function(mod, dat) {
  
  ran = REsim(mod,n.sims=1000) # simulate random effects
  fix = FEsim(mod,n.sims=1000) # simulate fixed effects
  lake.ran = subset(ran, groupFctr=="lagoslakeid") # extract lake-level random effects
  region.ran = subset(ran, groupFctr=="hu4_zoneid") # extract region-level random effects
  names(lake.ran)[2] = "lagoslakeid"
  names(region.ran)[c(2)] = "hu4_zoneid"
  region.ran = region.ran[,c(2:6)]
  names(region.ran)[3:5] = c("hu4_mean", "hu4_med", "hu4_sd")
  lake.regions = unique(dat[,c('lagoslakeid', 'hu4_zoneid')])
  lake.ran = merge(lake.ran, lake.regions)
  lake.ran = merge(lake.ran, region.ran, by = c("hu4_zoneid", "term"), all.x = TRUE)
  
  # calculate lake-specific intercept and slope that includes fixed and random (lake and region) effects
  # this means adding the random lake and region effect to the fixed effect
  lake.ran$coef_mean[lake.ran$term=="(Intercept)"] = lake.ran$mean[lake.ran$term=="(Intercept)"] + 
                                                     lake.ran$hu4_mean[lake.ran$term=="(Intercept)"] + 
                                                     fix$mean[1]
  lake.ran$coef_mean[lake.ran$term=="sampleyear_cor"] = lake.ran$mean[lake.ran$term=="sampleyear_cor"] + lake.ran$hu4_mean[lake.ran$term=="sampleyear_cor"] + fix$mean[2]
  
  # calculate sd of lake-specific intercept and slope
  # must square sd of individual effects, sum, and then take square root
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

# returned list is comparised of: 
# 1 = lake-specific random effects
# 2 = region-specific random effects
# 3 = random effects
# 4 = fixed effects

change.db.tn = mod.resim(tn.3fr23, data.tn)
change.db.tp = mod.resim(tp.3fr23, data.tp)
change.db.tntp = mod.resim(tntp.3fr23, data.tntp)       
change.db.chl = mod.resim(chl.3fr23, data.chl)

# example of plotting data from mod.resim
# look at random effects:

plotREsim(change.db.chl[[3]][change.db.chl[[3]]$term=="sampleyear_cor",])

# example of extracting coefficients from mixed model
# e.g., what was done to get numbers for table 2

change.db.tn[[4]][1,2] # tn intercept
change.db.tn[[4]][1,2] # tn slope
# calculate the number of lakes with tn trend < 0
length(which(change.db.tn[[1]]$ymax[change.db.tn[[1]]$term == "sampleyear_cor"] < 0))

# create dataframe with all lake slopes (TP, TN, CHL, TNTP)
# for fig generation
temp.tn = change.db.tn[[1]][,c(2,3,11,13,14)]
names(temp.tn)[3:5] = c("tn_coef_mean", "tn_ymin", "tn_ymax")

temp.tp = change.db.tp[[1]][,c(2,3,11,13,14)]
names(temp.tp)[3:5] = c("tp_coef_mean", "tp_ymin", "tp_ymax")

temp.tntp = change.db.tntp[[1]][,c(2,3,11,13,14)]
names(temp.tntp)[3:5] = c("tntp_coef_mean", "tntp_ymin", "tntp_ymax")

temp.chl = change.db.chl[[1]][,c(2,3,11,13,14)]
names(temp.chl)[3:5] = c("chl_coef_mean", "chl_ymin", "chl_ymax")

change.db.all = merge(temp.tn[temp.tn$term=="sampleyear_cor",c(2:5)], temp.tp[temp.tp$term=="sampleyear_cor",c(2:5)], by = "lagoslakeid", all = TRUE)
change.db.all = merge(change.db.all, temp.chl[temp.chl$term == "sampleyear_cor",c(2:5)], by = "lagoslakeid", all = TRUE)
change.db.all = merge(change.db.all, temp.tntp[temp.tntp$term == "sampleyear_cor",c(2:5)], by = "lagoslakeid", all = TRUE)

# generate categorical response from ymin and ymax values
change.db.all$tn_change[!is.na(change.db.all$tn_coef)] = "no change"
change.db.all$tn_change[change.db.all$tn_ymax<0] = "decreasing"
change.db.all$tn_change[change.db.all$tn_ymin>0] = "increasing"
change.db.all$tn_change = as.factor(change.db.all$tn_change)

change.db.all$tp_change[!is.na(change.db.all$tp_coef)] = "no change"
change.db.all$tp_change[change.db.all$tp_ymax<0] = "decreasing"
change.db.all$tp_change[change.db.all$tp_ymin>0] = "increasing"
change.db.all$tp_change = as.factor(change.db.all$tp_change)

change.db.all$tntp_change[!is.na(change.db.all$tntp_coef)] = "no change"
change.db.all$tntp_change[change.db.all$tntp_ymax<0] = "decreasing"
change.db.all$tntp_change[change.db.all$tntp_ymin>0] = "increasing"
change.db.all$tntp_change = as.factor(change.db.all$tntp_change)

change.db.all$chl_change[!is.na(change.db.all$chl_coef)] = "no change"
change.db.all$chl_change[change.db.all$chl_ymax<0] = "decreasing"
change.db.all$chl_change[change.db.all$chl_ymin>0] = "increasing"
change.db.all$chl_change = as.factor(change.db.all$chl_change)

write.table(change.db.all, "lmer_change_db.txt")

