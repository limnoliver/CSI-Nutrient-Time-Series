#calculate seasonal precip, temp and drought data for time series project

##############################
### Palmer drought index ###
##############################

# monthly Palmer data are located at 
# CSI_LIMNO_Manuscripts-presentations/CSI_Secchi trends paper/Data/palmer
# Palmer data are calculated by climate region, so there is code in this folder
# to link palmer data to lakes

#############################
### Precip and temp ###
#############################

# monthly precip and temp data are available in GEO-Shared2/GeoBulkReLoad/Z_MISC-folders/zz-files from Nicole/PRISM monthly 06-26-2014
# Noah has extraxted and combined the temp and precip data for hu12 and hu8, and those are
# already in the folder. For the Secchi paper, they used hu8 because it is closer to the 
# the scale at which the data are calculated (vs downscaled to hu12 scale)

# below code pulled from Sarah's climate work

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

# calculate seasonal climate metrics
# winter = December-February (90 in non-leap year, 91 in leap year), 
# spring = March-May (92 days), summer = June-August (92 days), 
# fall = September-November (91 days)
# weight according to the number of days in each month

for (i in 1:(length(temp$Year))) {
  if (temp$Year[i] > 1970) {
    if (temp$Year[i] %in% seq(1972,2011,4) == TRUE) {
      temp$winter[i] = ((temp$December[i-1]*31) + (temp$January[i]*31) + (temp$February[i]*29))/91
      temp$spring[i] = ((temp$March[i]*31) + (temp$April[i]*30) + (temp$May[i]*31))/92
      temp$summer[i] = ((temp$June[i]*30) + (temp$July[i]*31) + (temp$August[i]*31))/92
      temp$fall[i] = ((temp$September[i]*30) + (temp$October[i]*31) + (temp$November[i]*30))/91
      
    } else {
      temp$winter[i] = ((temp$December[i-1]*31) + (temp$January[i]*31) + (temp$February[i]*28))/90
      temp$spring[i] = ((temp$March[i]*31) + (temp$April[i]*30) + (temp$May[i]*31))/92
      temp$summer[i] = ((temp$June[i]*30) + (temp$July[i]*31) + (temp$August[i]*31))/92
      temp$fall[i] = ((temp$September[i]*30) + (temp$October[i]*31) + (temp$November[i]*30))/91
      
    }
  } else {
      temp$winter[i] = NA
      temp$spring[i] = NA
      temp$summer[i] = NA
      temp$fall[i] = NA
    }
}

for (i in 1:(length(precip$Year))) {
  if (precip$Year[i] > 1989) {
    if (precip$Year[i] %in% seq(1992,2011,4) == TRUE) {
      precip$winter[i] = ((precip$December[i-1]*31) + (precip$January[i]*31) + (precip$February[i]*29))/91
      precip$spring[i] = ((precip$March[i]*31) + (precip$April[i]*30) + (precip$May[i]*31))/92
      precip$summer[i] = ((precip$June[i]*30) + (precip$July[i]*31) + (precip$August[i]*31))/92
      precip$fall[i] = ((precip$September[i]*30) + (precip$October[i]*31) + (precip$November[i]*30))/91
      
    } else {
      precip$winter[i] = ((precip$December[i-1]*31) + (precip$January[i]*31) + (precip$February[i]*28))/90
      precip$spring[i] = ((precip$March[i]*31) + (precip$April[i]*30) + (precip$May[i]*31))/92
      precip$summer[i] = ((precip$June[i]*30) + (precip$July[i]*31) + (precip$August[i]*31))/92
      precip$fall[i] = ((precip$September[i]*30) + (precip$October[i]*31) + (precip$November[i]*30))/91
      
    }
  } else {
    precip$winter[i] = NA
    precip$spring[i] = NA
    precip$summer[i] = NA
    precip$fall[i] = NA
  }
}

write.table(precip, "precip.txt")
write.table(temp, "temp.txt")

# calculate seasonal total precip insteas of mean precip

precip$spring_tot = rowSums(precip[,c(5:7)])
precip$summer_tot = rowSums(precip[,c(8:10)])
precip$fall_tot = rowSums(precip[,c(11:13)])

for (i in 1:length(precip$Year)) {
 if (precip$Year[i] > 1989) {
  precip$winter_tot[i] = precip$December[i-1] + precip$January[i] + precip$February[i]
 }
}

#save as CSV - load latest Bayes data, and read csvs
