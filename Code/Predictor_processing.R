## Processing predictor variables for time series manuscript
## Predictors include observation-level, lake-level and region-level

# notes - not all lakes in the analysis have watershed data, so best to use 500m buffer

setwd("~/Dropbox/CSI_LIMNO_Manuscripts-presentations/CSI_Nitrogen MSs/Time series/Data")

###############################
## observation-level data
###############################
## climate
## processing elsewhere
setwd("C:/Users/Samantha/Dropbox/CSI_LIMNO_Manuscripts-presentations/CSI_Nitrogen MSs/Time series/Data")
temp = read.table("temp.txt")
precip = read.table("precip.txt")

#get rid of all observations before 1990
temp = subset(temp, temp$sampleyear>1989)
precip = subset(precip, precip$Year>1989)

#rename precip and temp variables so they are distinguishable

names(temp)[15:18]=c("temp_winter", "temp_spring", "temp_summer", "temp_fall")
names(precip)[15:22]=c("precip_winter", "precip_spring", "precip_summer", "precip_fall", 
                       "precip_spring_tot", "precip_summer_tot", "precip_fall_tot", "precip_winter_tot")
temp = temp[,c(1:2, 15:18)]
precip = precip[,c(1,2,15:22)]
names(precip)[1] = "hu12_zoneid"
names(precip)[2] = "sampleyear" 

#merge temp and precip dataframes

climate = merge(temp, precip, by = c("hu12_zoneid", "sampleyear"))

# get rid of precip totals

climate = climate[,c(1:10)]

# only include hu12 units that contain a lake in the analysis

climate = subset(climate, climate$hu12_zoneid %in% lev1$hu12_zoneid)

#center and scale data
# 
# climate.stand = as.data.frame(scale(climate[,c(3:10)]))
# climate.stand$hu12_zoneid = climate$hu12_zoneid
# climate.stand$sampleyear = climate$sampleyear

# create data frame that has hu4 id and sample years
lake.years = data[,c("hu12_zoneid", "sampleyear", "lagoslakeid")]
lake.years = lake.years[order(lake.years$lagoslakeid, lake.years$sampleyear),]
climate.stand = merge(lake.years, climate, by = c("hu12_zoneid", "sampleyear"))
climate.stand = climate.stand[order(climate.stand$lagoslakeid, climate.stand$sampleyear), ]

library(plyr)
library(reshape2)
library(data.table)
library(dplyr)
library(tidyr)
## center and scale climate data by lake

temp = list()
for (i in c(4:11)) {

df = data.frame(var = climate.stand[,i], 
                lagoslakeid = climate.stand$lagoslakeid)
scaled = tapply(df$var, list(df$lagoslakeid), scale)
scaled = melt(scaled)
scaled <- scaled %>% unnest(value) %>%
  group_by(Var1)
temp[[i]] = scaled$value
}

for (i in c(4:11)) {
  climate.stand[,i] = temp[[i]]
}

# remove hu12 to avoid comflict when merging

names(climate.stand)
climate.stand = climate.stand[,c(2:11)]
####################################
## lake-level data ##
####################################

setwd("~/Dropbox/CSI-LIMNO_DATA/LAGOSGeoData/LAGOS_VER1.03")

# landuse/land cover data calculated at either the watershed scale
iws.lulc = read.table("iws_lulc.txt", header = TRUE)

# reduce variables down to 2001 LULC data
# also include topography metrics to test

iws.lulc = iws.lulc[,c(1,49:80,147:156)]

# create urban, row crop, pasture, wetland categories

iws.lulc$iws_urban = iws.lulc$iws_nlcd2001_pct_22 + iws.lulc$iws_nlcd2001_pct_23 + iws.lulc$iws_nlcd2001_pct_24 
iws.lulc$iws_crop = iws.lulc$iws_nlcd2001_pct_82 
iws.lulc$iws_pasture = iws.lulc$iws_nlcd2001_pct_81
iws.lulc$iws_wetland = iws.lulc$iws_nlcd2001_pct_90 + iws.lulc$iws_nlcd2001_pct_95

# now narrow predictors

iws.lulc = iws.lulc[,c(1,34:47)]

# get rid of terrain metrics, with the exception of mean slope and TRI

iws.lulc = iws.lulc[,c(1,4,8,10:15)]

# limit IWS data to those with lakes in the analysis

iws.lulc = subset(iws.lulc, iws.lulc$iws_lagoslakeid %in% lev1$lagoslakeid)

# log transform terrain metrics

iws.lulc$iws_slope_mean = log(iws.lulc$iws_slope_mean)
iws.lulc$iws_tri_mean = log(iws.lulc$iws_tri_mean)

# center and scale iws metrics

iws.lulc.stand = as.data.frame(scale(iws.lulc[,c(2:3, 6:9)]))
iws.lulc.stand$lagoslakeid = iws.lulc$iws_lagoslakeid

# landuse/land cover data calculated at the 100m buffer scale
buff100.lulc = read.table("lakes4ha_buffer100m_lulc.txt", header = TRUE)

buff100.lulc = buff100.lulc[,c(1,46:77,144:152)]

# create urban, row crop, pasture, wetland categories

buff100.lulc$buff100_urban = buff100.lulc$lakes4ha_buffer100m_nlcd2001_pct_22 + buff100.lulc$lakes4ha_buffer100m_nlcd2001_pct_23 + buff100.lulc$lakes4ha_buffer100m_nlcd2001_pct_24 
buff100.lulc$buff100_crop = buff100.lulc$lakes4ha_buffer100m_nlcd2001_pct_82 
buff100.lulc$buff100_pasture = buff100.lulc$lakes4ha_buffer100m_nlcd2001_pct_81
buff100.lulc$buff100_wetland = buff100.lulc$lakes4ha_buffer100m_nlcd2001_pct_90 + buff100.lulc$lakes4ha_buffer100m_nlcd2001_pct_95

buff100.lulc = buff100.lulc[,c(1,34:46)]

# landuse/land cover data calculated at the 500m buffer scale
buff500.lulc = read.table("lakes4ha_buffer500m_lulc.txt", header = TRUE)
buff500.lulc = buff500.lulc[,c(1,49:80,147:155)]

# create urban, row crop, pasture, wetland categories

buff500.lulc$buff500_urban = buff500.lulc$lakes4ha_buffer500m_nlcd2001_pct_22 + buff500.lulc$lakes4ha_buffer500m_nlcd2001_pct_23 + buff500.lulc$lakes4ha_buffer500m_nlcd2001_pct_24 
buff500.lulc$buff500_crop = buff500.lulc$lakes4ha_buffer500m_nlcd2001_pct_82 
buff500.lulc$buff500_pasture = buff500.lulc$lakes4ha_buffer500m_nlcd2001_pct_81
buff500.lulc$buff500_wetland = buff500.lulc$lakes4ha_buffer500m_nlcd2001_pct_90 + buff500.lulc$lakes4ha_buffer500m_nlcd2001_pct_95

buff500.lulc = buff500.lulc[,c(1,34:46)]

# limit data to those buffers of lakes in the analysis

buff500.lulc = subset(buff500.lulc, buff500.lulc$lagoslakeid %in% lev1$lagoslakeid)

# lake morphology characteristics
setwd("~/Dropbox/CSI-LIMNO_DATA/LAGOSData/Version1.054.1")
lakes = read.table("lagos_lakes_10541.txt", 
                                header = TRUE, 
                                sep = "\t", 
                                quote = "", 
                                dec = ".", 
                                strip.white = TRUE,
                                comment.char = "")
lakes = lakes[,c(1:6, 10:14,20,32)]
lakes$wsatola = lakes$iws_areaha/lakes$lake_area_ha

# limit lakes to only those with TN/TP data

lakes = subset(lakes, lakes$lagoslakeid %in% lev1$lagoslakeid)

# standardize lake morph data

lakes.stand = lakes[,c(5,6,8,9,14)]
lakes.stand = log(lakes.stand)
lakes.stand = as.data.frame(scale(lakes.stand))
lakes.stand$lagoslakeid = lakes$lagoslakeid

###################################
## region-level data ##
###################################

setwd("~/Dropbox/CSI-LIMNO_DATA/LAGOSGeoData/LAGOS_VER1.03")
# huc4 level atmospheric deposition, climate, 
huc4 = read.table("hu4_chag.txt", header = TRUE)
huc4 = huc4[,c(1,2:5,54:77,78:81, 82:105)]

# create a mean atmospheric dep variable across all years

huc4$tn_dep_mean = rowMeans(huc4[,c(12,16,20,24,28)])

# create a variable that measures the absolute difference between start and 
# end atmospheric deposition

huc4$tn_dep_abs_change = huc4$hu4_dep_totaln_1990_mean - huc4$hu4_dep_totaln_2010_mean

# drop extra vars
huc4 = huc4[,c(1:5,30:59)]

# only include huc4s where there is a lake in the analysis

huc4 = subset(huc4, huc4$hu4_zoneid %in% data.lev1$hu4_zoneid)

# exclude all vars except tn dep

huc4.dep = huc4[,c(1, 34:35)]

# center and scale dep data

huc4.dep.stand = as.data.frame(scale(huc4.dep[,c(2:3)]))
huc4.dep.stand$hu4_zoneid = huc4$hu4_zoneid



##################################
## merge nutrient and predictor data
#################################

# load processed nutrient data
setwd("~/Dropbox/Time series/Bayes code")
nutrients = read.table("modern.e10.txt", header = TRUE)

# load processed level-1 data
# setwd("~/Dropbox/Time series/Bayes code/Output_11FEB16")
# lev1 = read.table("model_output.txt", header = TRUE)

# merge nutrients and lake-specific data, repeating from lake-specific
# dataset for each row of nutrient dataframe

data = merge(nutrients, lakes.stand, by = "lagoslakeid", all.x = TRUE)

# add hu12 id to data

data = merge(data, lev1[,c(1,15)], by = "lagoslakeid", all.x = TRUE)

# merge observations with climate data

data = merge(data, climate.stand, by = c("lagoslakeid", "sampleyear"),all.x=TRUE)

# merge observations with iws lulc data

data = merge(data, iws.lulc.stand, by = "lagoslakeid", all.x = TRUE)

# add huc4 zone id to the data frame

data = merge(data, data.lev1[,c(1,2)], by = "lagoslakeid")
 
# merge observations with huc4 data

data = merge(data, huc4.dep.stand, by = "hu4_zoneid", all.x = TRUE)

# drop observations after 2011 (no climate data)

data = subset(data, !is.na(data$precip_fall))

# add PCA axis #1 to data frame

data$clim_pca = as.numeric(pca.out)

# write a text file
setwd("C:/Users/Samantha/Dropbox/Time series/Bayes code")
write.table(data, "timeseries_data.txt")

#merge observations with landuse/topography data at various scales

names(iws.lulc)[1] = "nhdid"
data = merge(data, iws.lulc, by = "nhdid", all.x = TRUE)
names(buff100.lulc)[10] = "lagoslakeid"
data = merge(data, buff100.lulc, by = "lagoslakeid", all.x = TRUE)
names(buff500.lulc)[10] = "lagoslakeid" 
data = merge(data, buff500.lulc, by = "lagoslakeid", all.x = TRUE)


# merge the bayes model output with lake-level predictors to 
# explore the data

data.lev1 = merge(lev1, buff100.lulc, by = "lagoslakeid", all.x = TRUE)
data.lev1 = merge(data.lev1, buff500.lulc, by = "lagoslakeid", all.x = TRUE)
data.lev1 = merge(data.lev1, iws.lulc, by = "nhdid", all.x = TRUE)
lakes.short = lakes[,c(1,11)]
data.lev1 = merge(data.lev1, lakes.short, by = "lagoslakeid", all.x = TRUE)
data.lev1 = merge(data.lev1, huc4, by = "hu4_zoneid", all.x = TRUE)
########################################
## data exploration to narrow predictors
########################################

library(lme4)

data.stand = as.data.frame(scale(data[,c(5,21:32)]))
data.stand$lagoslakeid = data$lagoslakeid
data.stand$tn_umol = data$tn_umol
data.stand$tp_umol = data$tp_umol

## center tn and tp data by lake, essentially to get rid of intercepts
scale.center = function(x) scale(x, center = TRUE, scale = FALSE)

test = tapply(data.stand$tn_umol, list(data.stand$lagoslakeid), scale.center)
test = tapply(data.stand$tp_umol, list(data.stand$lagoslakeid), scale.center)
library(plyr)
library(reshape2)
library(data.table)
library(dplyr)
library(tidyr)

test2 = melt(test)
test3 = setDT(test2)


test3 <- test3 %>% unnest(value) %>%
          group_by(Var1)



data.stand$tp_umol_c = test3$value


## nitrogen
plot(log(tn_umol) ~ temp_winter, data = data.stand)
plot(log(tp_umol) ~ precip_spring, data = data.stand)

model = lmer(tn_umol_c ~ sampleyear + (sampleyear|lagoslakeid), data = data.stand)



precip_summer = lmer(log(tn_umol) ~ precip_summer + (precip_summer|lagoslakeid), data = data.stand)
temp_winter = lmer(log(tn_umol) ~ temp_winter + (temp_winter|lagoslakeid), data = data.stand)
test = lmer(log(tn_umol) ~ temp_winter*precip_spring + (temp_winter+precip_spring|lagoslakeid), data = data.stand)
temp_spring
sem.model.fits(test)

## put all climate variables and interactions in mixed model

tn_climate_full = lmer(log(tn_umol) ~ precip_winter*precip_spring*precip_summer*temp_winter*temp_spring*temp_summer+ (1|lagoslakeid), data = data.stand)

## look at correlation between climate metrics

library(corrplot)

data.cor = cor(data.stand[,c(2:9,15,16)], use="complete.obs")
corrplot(data.cor, order = "hclust", method = "number")
## pull out the BLUPS

test_blups = coef(test)
blups = data.frame(intercepts = test_blups$lagoslakeid[[1]], 
                     slopes_temp_winter = test_blups$lagoslakeid[[2]],
                     slopes_precip_spring = test_blups$lagoslakeid[[3]],
                     lagoslakeid = row.names(test_blups$lagoslakeid))
blups_pred = merge(blups, data.lev1, by = "lagoslakeid", all.x = TRUE)

precip_blups= coef(precip_spring)
precip_blups = data.frame(intercepts = precip_blups$lagoslakeid[[1]], 
                          slopes_precip_spring = precip_blups$lagoslakeid[[2]], 
                          lagoslakeid = row.names(precip_blups$lagoslakeid))
precip_blups_pred = merge(precip_blups, data.lev1, by = "lagoslakeid", all.x = TRUE)

pdf("tn_precip_crops.pdf")
par(mar=c(5,6,4,2))
plot(precip_blups_pred$slopes_precip_spring ~ precip_blups_pred$iws_crop, 
     xlab = "% Crop in Watershed", ylab = "Lake-Specific TN~Precip Slope", 
     cex.lab = 2, cex.axis = 1.5)

dev.off()

## phosphorus
plot(log(tp_umol) ~ temp_spring, data = data.stand)
precip_spring = lmer(log(tp_umol) ~ precip_spring + (precip_spring|lagoslakeid), data = data.stand)
precip_summer = lmer(log(tp_umol) ~ precip_summer + (precip_summer|lagoslakeid), data = data.stand)
temp_winter = lmer(log(tp_umol) ~ temp_winter + (temp_winter|lagoslakeid), data = data.stand)
plot(precip_spring ~ temp_winter, data = data.stand)
null = lmer(log(tp_umol) ~ 1 + (1|lagoslakeid), data = data.stand)
precip = lmer(precip_summer ~ sampleyear + (sampleyear|lagoslakeid), data = data.stand)
temp = lmer(temp_winter ~ sampleyear + (sampleyear|lagoslakeid), data = data.stand)

precip_blups = coef(precip_summer)
precip_blups = data.frame(intercepts = precip_blups$lagoslakeid[[1]], 
                          slopes_precip_summer = precip_blups$lagoslakeid[[2]], 
                          lagoslakeid = row.names(precip_blups$lagoslakeid))
precip_blups_pred = merge(precip_blups, data.lev1, by = "lagoslakeid", all.x = TRUE)

pdf("tp_precip_depth.pdf")  
par(mar=c(5,6,4,2))
plot(precip_blups_pred$slopes_precip_summer ~ log(precip_blups_pred$maxdepth), 
     xlab = "log(Maximum Depth)", ylab = "Lake-Specific TP~Precip Slope", 
     cex.lab = 2, cex.axis = 1.5)
dev.off()

precip_summer_depth = lmer(log(tp_umol) ~ precip_summer:)

pdf("tp_precip_summer.pdf")
plot(log(tp_umol) ~ precip_summer, data = data.stand)

############################
## PCA for climate data
############################

# 

library(stats)

clim = data[,c(16:23)]

# get rid of fall data - very little evidence for relationship 
# between fall climate and summer nutrients

clim = clim[,c(1:3,5:7)]

# run a PCA analysis on climate data

clim.pca = prcomp(clim, center = FALSE, scale = FALSE, na.rm = TRUE)
pca.out = clim.pca$x[,1]

# look at variance in each axis

summary(clim.pca)



# test pca axis against nutrients

library(lme4)

pca1.lmer = lmer(log(sub.data$tp_umol) ~ as.numeric(pca.out[,1]) + (as.numeric(pca.out[,1])|sub.data$lagoslakeid))
plot(log(sub.data$tp_umol) ~ as.numeric(pca.out[,2]))
pca2.lmer = lmer(log(sub.data$tp_umol) ~ as.numeric(pca.out[,2]) + (as.numeric(pca.out[,2])|sub.data$lagoslakeid))
 
tp.stand = scale(log(sub.data$tp_umol), center = TRUE)
pca1.lmer = lmer(tp.stand ~ as.numeric(pca.out[,1]) + (as.numeric(pca.out[,1])|sub.data$lagoslakeid))

## run PCA on climate data that has been centered and scaled for each lake

clim.temp = climate.stand[,c(3:10)]
clim.pca = prcomp(clim.temp, center = FALSE, scale = FALSE, na.rm = TRUE)
pca.out = clim.pca$x[,1]

# pull data out of lmer
test.tn = lmer(log(tn_umol) ~ precip_spring + (precip_spring || lagoslakeid), data = data)
test.tp = lmer(log(tp_umol) ~ precip_winter + (precip_winter || lagoslakeid), data = data)

temp.tn = data.frame(tn.slopes = coef(test.tn)$lagoslakeid[,2], 
                     tn.intercepts = coef(test.tn)$lagoslakeid[,1],
                  lagoslakeid = as.numeric(rownames(coef(test.tn)$lagoslakeid)))
temp.tp = data.frame(tp.slopes = coef(test.tp)$lagoslakeid[,2], 
                     tp.intercepts = coef(test.tp)$lagoslakeid[,1],
                     lagoslakeid = as.numeric(rownames(coef(test.tp)$lagoslakeid)))

temp.data = data[,c(1,2,10:15,24:32)]
temp.data = unique(temp.data)
temp = merge(temp.tn, temp.data, by = "lagoslakeid")
temp = merge(temp, temp.tp, by = "lagoslakeid")

intercepts = c()
slopes = c()
huc4 = c("HU4_10", "HU4_27", "HU4_33", "HU4_36", "HU4_37", "HU4_38","HU4_49", "HU4_51","HU4_56","HU4_57","HU4_60","HU4_61","HU4_62","HU4_63","HU4_64", "HU4_65")
for (i in c(1:16)) { 
  lm.temp = lm(tn.slopes ~ iws_crop, data = temp[temp$hu4_zoneid == huc4[i], ])
  intercepts[i] = as.numeric(coef(lm.temp)[1])
  slopes[i] = as.numeric(coef(lm.temp)[2])
}

huc.analysis = data.frame(huc4_zoneid = huc4, slopes = slopes, intercepts = intercepts)
dep = unique(temp.data[,c(15:17)])
test = merge(huc.analysis, dep, by = "huc4_zoneid", all.x = TRUE)
plot(tn.slopes~iws_crop, data = temp)
plot(test$slopes~test$tn_dep_abs_change)

test.3ln = lmer(log(tn_umol) ~ precip_spring*iws_crop + (precip_spring || lagoslakeid) + (iws_crop||hu4_zoneid), data = data)
test.3lp = lmer(log(tp_umol) ~ iws_crop + precip_winter*maxdepth + (precip_winter || lagoslakeid) + (iws_crop||hu4_zoneid), data = data)

test.year = lmer(log(tn_umol) ~ sampleyear_cor + (sampleyear_cor || lagoslakeid), data = data)

#############################################################
# which precip predictors are best for predicting TN and TP?
#############################################################

n.winter = lmer(log(tn_umol) ~ precip_winter + (precip_winter||lagoslakeid), data = data)
# 0.013, t = 2.43

n.spring = lmer(log(tn_umol) ~ precip_spring + (precip_spring||lagoslakeid), data = data)
#0.014, t = 2.66

n.summer = lmer(log(tn_umol) ~ precip_summer + (precip_summer||lagoslakeid), data = data)
#-0.016, t = 3.05

n.fall = lmer(log(tn_umol) ~ precip_fall + (precip_fall||lagoslakeid), data = data)
#.007, 1.18



p.winter = lmer(log(tp_umol) ~ precip_winter + (precip_winter||lagoslakeid), data = data)
# 0.029, t = 4.47

p.spring = lmer(log(tp_umol) ~ precip_spring + (precip_spring||lagoslakeid), data = data)
#-.009, t = -1.32

p.summer = lmer(log(tp_umol) ~ precip_summer + (precip_summer||lagoslakeid), data = data)
#.001, t = .227

p.fall = lmer(log(tp_umol) ~ precip_fall + (precip_fall||lagoslakeid), data = data)
#-0.01, t = -1.74

## figure out how to specify 3-level model in lmer

test.p = lmer(log(tp_umol) ~ sampleyear_cor + (sampleyear_cor||lagoslakeid) + (sampleyear_cor||hu4_zoneid), data = data)
test.n = lmer(log(tn_umol) ~ sampleyear_cor + (sampleyear_cor||lagoslakeid) + (sampleyear_cor||hu4_zoneid), data = data)

## pull out the region with the most data and figure out of the estimates seem reasonable
## hu4_10 has the most data

data.hu10 = subset(data, data$hu4_zoneid == "HU4_10")
plot(log(tn_umol) ~ sampleyear_cor, data = data.hu10, col = data.hu10$lagoslakeid)
plot(log(tn_umol) ~ sampleyear_cor, data = data.hu10[data.hu10$lagoslakeid == 7595, ])

#add regional coefficients

abline(as.numeric(coef(test.n)$hu4_zoneid[rownames(coef(test.n)$hu4_zoneid)=="HU4_10", ]))

# get a list of all lakes within hu4_10
lakes_10 = unique(data.hu10$lagoslakeid)

# add slopes and intercepts for each lake, red if negative
# green if positive

ranefs_10_lake = ranef(test.n)$lagoslakeid
ranefs_10_lake = subset(ranefs_10_lake, rownames(ranefs_10_lake) %in% lakes_10)
ranefs_10_region = ranef(test.n)$hu4_zoneid
ranefs_10_region = subset(ranefs_10_region, rownames(ranefs_10_region) == "HU4_10")
coef_pop = summary(test.n)$coef[,1]
coefs_slope_lake = coef_pop[2] + ranefs_10_lake[,2] + ranefs_10_region[,2]
coefs_int_lake = coef_pop[1] + ranefs_10_lake[,1] + ranefs_10_region[,1]

for (i in 1:67) {
  if (as.numeric(coefs_slope_lake[i]) < 0){
    abline(coefs_int_lake[i], coefs_slope_lake[i], col = "red")
  }else {
    abline(coefs_int_lake[i], coefs_slope_lake[i], col = "green")
  }
}
# add region line
abline(as.numeric(coef(test.n)$hu4_zoneid[rownames(coef(test.n)$hu4_zoneid)=="HU4_10", ]),
       lwd = 4)
#add global line
abline(coef_pop, lwd = 8)
#plot all data, color by region
plot(log(tn_umol) ~ sampleyear_cor, data = data, col = data$hu4_zoneid)

for (i in 1:42) {
  if (as.numeric(coef(test.n)$hu4_zoneid[i,2]) < 0){
    abline(as.numeric(coef(test.n)$hu4_zoneid[i,1]), as.numeric(coef(test.n)$hu4_zoneid[i,2]), col = "red")
  }else {
  abline(as.numeric(coef(test.n)$hu4_zoneid[i,1]), as.numeric(coef(test.n)$hu4_zoneid[i,2]), col = "green")
  }
}
abline(4.17295, -0.01037, col = "black", lwd=4)

# unconditional 
test.n.uc = lmer(log(tn_umol) ~ 1+(1|lagoslakeid)+(1|hu4_zoneid), data = data)
test.p.uc = lmer(log(tp_umol) ~ 1+(1|lagoslakeid)+(1|hu4_zoneid), data = data)

# time as fixed effect, random slopes
test.n.time = lmer(log(tn_umol) ~ sampleyear_cor +(1|lagoslakeid)+(1|hu4_zoneid), data = data)
test.p.time = lmer(log(tp_umol) ~ sampleyear_cor +(1|lagoslakeid)+(1|hu4_zoneid), data = data)

