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
all.lakes = all.lakes[,c(1,3:5,12,38)]

## limit to lakes in time series analysis
lakes = subset(all.lakes, all.lakes$lagoslakeid %in% lakes)

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

geo.data = merge(iws.lulc, iws.lulc$lagoslakeid %in% lakes)



