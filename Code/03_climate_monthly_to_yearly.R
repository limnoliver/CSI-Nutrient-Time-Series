## code to convert monthly climate variables into yearly climate variables
## code can also convert monthly climate vars to seasonal climate vars

setwd("C:/Users/Samantha/Dropbox/GEO-Shared2/GeoBulkReLoad/Z_MISC-folders/zz-files from Nicole/PRISM monthly 06-26-2014")

# convert monthly vals to annual vals
# note that this code accounts for number of days per month 
# and leap years
climate.month.to.year <- function(variable, scale) {
temp = read.table(paste(scale, "_", variable, "_merge.txt", sep = ""), header = TRUE)
  for (i in 1:nrow(temp)){
  if (temp$year[i] %in% seq(1972,2011,4) == TRUE) {
    temp$annual[i] = ((temp$January[i]*31) + 
                     (temp$February[i]*29) +
                     (temp$March[i]*31) + 
                     (temp$April[i]*30) + 
                     (temp$May[i]*31) +
                     (temp$June[i]*30) + 
                     (temp$July[i]*31) + 
                     (temp$August[i]*31) +
                     (temp$September[i]*30) + 
                     (temp$October[i]*31) + 
                     (temp$November[i]*30) +
                     (temp$December[i]*31))/366
    

  } else {
    temp$annual[i] = ((temp$January[i]*31) + 
                        (temp$February[i]*28) +
                        (temp$March[i]*31) + 
                        (temp$April[i]*30) + 
                        (temp$May[i]*31) +
                        (temp$June[i]*30) + 
                        (temp$July[i]*31) + 
                        (temp$August[i]*31) +
                        (temp$September[i]*30) + 
                        (temp$October[i]*31) + 
                        (temp$November[i]*30) +
                        (temp$December[i]*31))/365
  }
  }
return(temp)
}

# convert monthly vals to seasonal vals,
# where winter = Dec, Jan, Feb
# spring = March, April, May
# summer = June, Jul, Aug
# fall = Sep, Oct, Nov
# note that the first year in record (1970) has NA values because 
# winter is calculated with previous years' December value, and there is no
# 1969 data
climate.month.to.season <- function(variable, scale) {
  temp = read.table(paste(scale, "_", variable, "_merge.txt", sep = ""), header = TRUE)
  for (i in 1:(nrow(temp))) {
    if (temp$year[i] > 1970) {
      if (temp$year[i] %in% seq(1972,2011,4) == TRUE) {
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
  return(temp)
}

hu4.tmean.annual = climate.month.to.year("tmean", "HU4")
hu4.ppt.annual = climate.month.to.year("ppt", "HU4")
hu4.tmean.season = climate.month.to.season("tmean", "HU4")
