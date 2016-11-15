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

##################################################################
## create list of lakes and regions from all nutrient analyses
##################################################################

setwd("C:/Users/Samantha/Dropbox/CSI_LIMNO_Manuscripts-presentations/CSI_Nitrogen MSs/Time series/Data")
all.lakes = read.table( "lmer_change_db.txt", header = TRUE)


# merge with lake-specific info including
# lat/long, depth, area,

all.lakes = merge(all.lakes,
                  lake.info[,c("lagoslakeid", "nhd_lat", "nhd_long", "lake_area_ha", "maxdepth",
                               "hu4_zoneid", "iws_areaha", "lakeconnectivity")],
                  by = "lagoslakeid",
                  all.x = TRUE)

################################################
## create data frame of lake-specific predictors
################################################

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

