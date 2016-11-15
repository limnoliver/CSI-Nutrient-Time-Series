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