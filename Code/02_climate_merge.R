# This code takes the climate files, which includes a separate file
# for each month/year combination at each spatial scale 
# and for each variable, and merges them so year, huc4 and each month 
# are all columns

require(tidyr)

# extract monthly climate data from HUC4 climate data
# input your variable and scale of interest
# variable options include tmean, tmin, tmax, ppt
# scale options include COUNTY, EDU, HU4, HU8, HU12, STATE

# WARNING: may have to change wd function in the merge.climate.dat function

merge.climate.dat <- function(variable, scale) {

# reset working directory for correct scale and variable
setwd(paste("C:/Users/Samantha/Dropbox/GEO-Shared2/GeoBulkReLoad/Z_MISC-folders/zz-files from Nicole/PRISM monthly 06-26-2014", "/", variable, "/", scale, sep = ""))

# create a vector of month names to convert numeric month to month name
month.names = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  
# read all files in working directory
all.files = list.files()

# read the first file in the wd to create the initial dataframe that will be built upon
  temp = read.csv(all.files[1])
  temp = temp[,c("ZoneID", "MEAN")]
  temp$year = as.numeric(gsub("(HU4_PRISM_)([[:alpha:]]+[[:punct:]])(\\d{4})(\\d{2})(.csv)", "\\3", all.files[1], perl = TRUE))
  temp$month = month.names[as.numeric(gsub("(HU4_PRISM_)([[:alpha:]]+[[:punct:]])(\\d{4})(\\d{2})(.csv)", "\\4", all.files[1], perl = TRUE))]
  
  merged.dat = temp

# loop through all files in wd  
  for (i in 2:length(all.files)){
    temp = read.csv(all.files[i])
    #keep only columns of interest
    temp = temp[,c("ZoneID", "MEAN")]
    #extract year and month names from the file name
    temp$year = as.numeric(gsub("(HU4_PRISM_)([[:alpha:]]+[[:punct:]])(\\d{4})(\\d{2})(.csv)", "\\3", all.files[i], perl = TRUE))
    temp$month = month.names[as.numeric(gsub("(HU4_PRISM_)([[:alpha:]]+[[:punct:]])(\\d{4})(\\d{2})(.csv)", "\\4", all.files[i], perl = TRUE))]
    #add new file rows to previous file
    merged.dat = rbind(temp, merged.dat)
    }
#convert merged.dat from long file to wide file, with month as column names
  merged.dat = spread(merged.dat, month, MEAN)
  return(merged.dat)
}

tmean.hu4 = merge.climate.dat("tmean", "HU4")
ppt.hu4 = merge.climate.dat("tmean", "HU4")

setwd("C:/Users/Samantha/Dropbox/CSI_LIMNO_Manuscripts-presentations/CSI_Nitrogen MSs/Time series/Data")
write.table(tmean.hu4, "HU4_tmean_merge.txt")
write.table(ppt.hu4, "HU4_ppt_merge.txt")

