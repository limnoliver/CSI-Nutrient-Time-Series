library(tidyr)
## This file prepares the predictor variables data frame for the random forest analysis
## first merges all relevant variables from various LAGOS geo tables

## for lake-specific predictors, include:
## 1) LULC at buffer scale (% ag, % forest, % urban, %wetland)
## 2) lake-specific features (depth, area)
## 3) connectivity type
## 4) topography
## 5) model parameters: number of years of observation, median year, mean concentration

## for region-specific slopes, include:
## 1) LULC at huc 4 scale (% ag, %forest, % urban)
## 2) climate at the huc4 scale (summer, yearly temp and precip)
## 3) connectivity/hydrology at huc 4 scale (water in region, runoff, etc)
## 4) atmospheric deposition at huc 4 scale
## 5) topography at the huc 4 scale
## 6) model parameters: number of lakes, mean concentration in region

## get list of 2913 unique lakes that have nutrient data
setwd("C:/Users/Samantha/Dropbox/CSI_LIMNO_Manuscripts-presentations/CSI_Nitrogen MSs/Time series/Publication")
lakes = read.csv("LAGOS_summer_meanvals.csv", header = TRUE)
lakes = unique(lakes$lagoslakeid)

##############################################
## import lake-scale data
##############################################

#############
## import lake-specific data (e.g., depth, area, etc)

setwd("C:/Users/Samantha/Dropbox/CSI-LIMNO_DATA/LAGOSData/Version1.054.1")
all.lakes = read.table("lagos_lakes_10541.txt", 
                       header = TRUE, 
                       sep = "\t", 
                       quote = "", 
                       dec = ".", 
                       strip.white = TRUE,
                       comment.char = "")

## limit to variables of interest
all.lakes = all.lakes[,c(1,14,3:5,12,38)]

## limit to lakes in time series analysis
lakes = subset(all.lakes, all.lakes$lagoslakeid %in% lakes)

###########
## import watershed-scale land use/land cover data
setwd("~/Dropbox/CSI-LIMNO_DATA/LAGOSGeoData/LAGOS_VER1.03")
setwd("C:/Users/Samantha/Dropbox/CSI-LIMNO_DATA/LAGOSGeoData/LAGOS_VER1.03")

# landuse/land cover data calculated at the watershed scale
iws.lulc = read.table("iws_lulc.txt", header = TRUE)

# create urban, row crop, pasture, wetland variables
# from 2001 LULC data

iws.lulc$iws_urban = iws.lulc$iws_nlcd2001_pct_22 + iws.lulc$iws_nlcd2001_pct_23 + iws.lulc$iws_nlcd2001_pct_24 
iws.lulc$iws_crop = iws.lulc$iws_nlcd2001_pct_82 
iws.lulc$iws_pasture = iws.lulc$iws_nlcd2001_pct_81
iws.lulc$iws_forest = iws.lulc$iws_nlcd2001_pct_41 + iws.lulc$iws_nlcd2001_pct_42 + iws.lulc$iws_nlcd2001_pct_43

# reduce variables down to 2001 LULC data
# also include topography metrics to test

iws.lulc = iws.lulc[,c(1,8,146,149,155,156,159:162)]

# rename ID var to allow merge with lake.info

names(iws.lulc)[6] = "lagoslakeid" 

# filter IWS to lakes in time series analysis

geo.data = merge(lakes, iws.lulc, by = "lagoslakeid", all.x = TRUE)

#################
# import watershed-scale connectivity metrics
setwd("C:/Users/Samantha/Dropbox/CSI-LIMNO_DATA/LAGOSGeoData/LAGOS_VER1.03")
iws.conn = read.table("iws_conn.txt", header = TRUE)
iws.conn = iws.conn[,c(7,12,27,82,84,86,152)]
names(iws.conn)[7] = "lagoslakeid"

# merge with other geo data

geo.data = merge(geo.data, iws.conn, by = "lagoslakeid", all.x = TRUE)

###################################
## import huc 4-scale data
###################################

setwd("C:/Users/Samantha/Dropbox/CSI-LIMNO_DATA/LAGOSGeoData/LAGOS_VER1.03")

###############
# import climate, hydrology, atmospheric, and geology data
hu4.chag = read.table("hu4_chag.txt", header = TRUE)

# calculate trends in atmospheric deposition
atm.dep = hu4.chag[,c(1,60,64,68,72,76)]
atm.dep = gather(atm.dep, key = "year", value = "deposition",2:6)
temp = gsub("(hu4_dep_totaln_)(\\d+)(_mean)", replacement = "\\2", atm.dep$year)
atm.dep$year = temp

dep.change = c()
for (i in 1:length(unique(atm.dep$hu4_zoneid))){
  dat = atm.dep[atm.dep$hu4_zoneid == unique(atm.dep$hu4_zoneid)[i], ]
  dat.lm = lm(dat$deposition~as.numeric(dat$year))
  dep.change[i] = as.numeric(dat.lm$coefficients[2])
}

dep.change = data.frame(hu4_zoneid = unique(atm.dep$hu4_zoneid),
                        hu4_dep_change = dep.change)

hu4.chag = hu4.chag[,c(1,4,60,76,84,92,100)]
hu4.chag = merge(hu4.chag, dep.change, by = "hu4_zoneid", all.x = TRUE)

geo.data = merge(geo.data, hu4.chag, by = "hu4_zoneid", all.x = TRUE)

####################
# calculate climate trends for each hu4
setwd("C:/Users/Samantha/Dropbox/CSI_LIMNO_Manuscripts-presentations/CSI_Nitrogen MSs/Time series/Data")
hu4.ppt.annual = read.csv("HU4_ppt_annual.csv", header=TRUE)
hu4.tmean.annual = read.csv("HU4_tmean_annual.csv", header = TRUE)

ppt.change = c()
for (i in 1:length(unique(hu4.ppt.annual$ZoneID))){
  dat = hu4.ppt.annual[hu4.ppt.annual$ZoneID == unique(hu4.ppt.annual$ZoneID)[i], ]
  dat.lm = lm((dat$annual/100)~as.numeric(dat$year))
  ppt.change[i] = as.numeric(dat.lm$coefficients[2])
}
ppt.change = data.frame(hu4_zoneid = unique(hu4.ppt.annual$ZoneID),
                        hu4_ppt_change = ppt.change)

tmean.change = c()
for (i in 1:length(unique(hu4.tmean.annual$ZoneID))){
  dat = hu4.tmean.annual[hu4.tmean.annual$ZoneID == unique(hu4.tmean.annual$ZoneID)[i], ]
  dat.lm = lm((dat$annual/100)~as.numeric(dat$year))
  tmean.change[i] = as.numeric(dat.lm$coefficients[2])
}
tmean.change = data.frame(hu4_zoneid = unique(hu4.tmean.annual$ZoneID),
                          hu4_tmean_change = tmean.change)

geo.data = merge(geo.data, ppt.change, by = "hu4_zoneid", all.x = TRUE)
geo.data = merge(geo.data, tmean.change, by = "hu4_zoneid", all.x = TRUE)

# add ppt and tmean from beginning (1990) and end (2011)
# of time series. Note timeseries goes until 2013, but 
# most limno data and all geo data was collected in 2011, so ends there

names(hu4.tmean.annual)[2] = "hu4_zoneid"
names(hu4.ppt.annual)[2] = "hu4_zoneid"

tmean.1990 = hu4.tmean.annual[hu4.tmean.annual$year == 1990, c("hu4_zoneid", "annual")]
names(tmean.1990)[2] = "hu4_tmean_1990"
tmean.2011 = hu4.tmean.annual[hu4.tmean.annual$year == 2011, c("hu4_zoneid", "annual")]
names(tmean.2011)[2] = "hu4_tmean_2011"
ppt.1990 = hu4.ppt.annual[hu4.ppt.annual$year == 1990, c("hu4_zoneid", "annual")]
names(ppt.1990)[2] = "hu4_ppt_1990"
ppt.2011 = hu4.ppt.annual[hu4.ppt.annual$year == 2011, c("hu4_zoneid", "annual")]
names(ppt.2011)[2] = "hu4_ppt_2011"

# merge annual temp and precip with other geo data
geo.data = merge(geo.data, tmean.1990, by = "hu4_zoneid", all.x = TRUE)
geo.data = merge(geo.data, tmean.2011, by = "hu4_zoneid", all.x = TRUE)
geo.data = merge(geo.data, ppt.1990, by = "hu4_zoneid", all.x = TRUE)
geo.data = merge(geo.data, ppt.2011, by = "hu4_zoneid", all.x = TRUE)

#############################
# import connectivity metrics
setwd("C:/Users/Samantha/Dropbox/CSI-LIMNO_DATA/LAGOSGeoData/LAGOS_VER1.03")
hu4.conn = read.table("hu4_conn.txt", header = TRUE)
hu4.conn = hu4.conn[,c(1,4,7,12,27,79)]

geo.data = merge(geo.data, hu4.conn, by = "hu4_zoneid", all.x = TRUE)

# import land use/land cover data
hu4.lulc = read.table("hu4_lulc.txt", header = TRUE)
hu4.lulc$hu4_urban = hu4.lulc$hu4_nlcd2001_pct_22 + hu4.lulc$hu4_nlcd2001_pct_23 + hu4.lulc$hu4_nlcd2001_pct_24
hu4.lulc$hu4_crop = hu4.lulc$hu4_nlcd2001_pct_82
hu4.lulc$hu4_pasture = hu4.lulc$hu4_nlcd2001_pct_81
hu4.lulc$hu4_forest = hu4.lulc$hu4_nlcd2001_pct_41 + hu4.lulc$hu4_nlcd2001_pct_42 + hu4.lulc$hu4_nlcd2001_pct_43

hu4.lulc = hu4.lulc[,c(1,8,158,161,167:170)]

geo.data = merge(geo.data, hu4.lulc, by = "hu4_zoneid", all.x = TRUE)
