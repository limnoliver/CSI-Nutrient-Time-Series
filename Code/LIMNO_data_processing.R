library(reshape)
setwd('~/Dropbox/CSI-LIMNO_DATA/LAGOSData/Version1.054.1')
setwd("C:/Users/Samantha/Dropbox/CSI-LIMNO_DATA/LAGOSData/Version1.054.1")
data = read.table("lagos_epi_nutr_10541.txt", 
                  header = TRUE, 
                  sep = "\t", 
                  quote = "", 
                  dec = ".", 
                  strip.white = TRUE, 
                  comment.char = "", 
                  colClasses=c(sampledate = "POSIXct"))

data.lake.specific = read.table("lagos_lakes_10541.txt", 
                                header = TRUE, 
                                sep = "\t", 
                                quote = "", 
                                dec = ".", 
                                strip.white = TRUE,
                                comment.char = "")
#Calculate TN from TKN
data$tn_calculated = data$tkn + data$no2no3
data$tn_combined = data$tn
data$tn_combined[which(is.na(data$tn_combined) == TRUE)] = data$tn_calculated[which(is.na(data$tn_combined) == TRUE)]

# create a data subset of TN and TP (independent) where there has to be 
# TN or TP observations, respectively

tn = data[is.na(data$tn_combined)==FALSE, ]
tp = data[is.na(data$tp)==FALSE, ]
chl = data[is.na(data$chla)==FALSE, ]
stoich = data[is.na(data$tn_combined)==FALSE & is.na(data$tp)==FALSE, ]

#create molar version of nutrients
tn$tn_umol = tn$tn_combined/14.01
tp$tp_umol = tp$tp/30.97
stoich$tn_tp = stoich$tn_combined/stoich$tp
stoich$tn_tp_umol = stoich$tn_tp * (30.97/14.01)

#limit observations from JUN 15-AUG 15
# first, just keep samples in JUN, JUL, AUG, SEP
tn.summer = tn[tn$samplemonth == 6|tn$samplemonth == 7|tn$samplemonth==8|tn$samplemonth==9, ]
tp.summer = tp[tp$samplemonth == 6|tp$samplemonth == 7|tp$samplemonth==8|tp$samplemonth==9, ]
chl.summer = chl[chl$samplemonth == 6|chl$samplemonth == 7|chl$samplemonth==8|chl$samplemonth==9, ]
stoich.summer = stoich[stoich$samplemonth == 6|stoich$samplemonth == 7|stoich$samplemonth==8|stoich$samplemonth==9, ]

# now, get rid of first half of June and second half of Sept
tn.summer$sampleday = format(tn.summer$sampledate, "%d")
tn.summer$sampleday = as.numeric(tn.summer$sampleday)

tp.summer$sampleday = format(tp.summer$sampledate, "%d")
tp.summer$sampleday = as.numeric(tp.summer$sampleday)

chl.summer$sampleday = format(chl.summer$sampledate, "%d")
chl.summer$sampleday = as.numeric(chl.summer$sampleday)

stoich.summer$sampleday = format(stoich.summer$sampledate, "%d")
stoich.summer$sampleday = as.numeric(stoich.summer$sampleday)

keep.summer <- function(x) {
keep.june = which(x$samplemonth == 6 & x$sampleday >= 15)
keep.july = which(x$samplemonth == 7)
keep.august = which(x$samplemonth == 8)
keep.september = which(x$samplemonth==9 & x$sampleday <= 15)
keep.all = c(keep.june, keep.july, keep.august, keep.september)
keep.all = as.numeric(keep.all)

return(x[keep.all, ])
}

tn.summer = keep.summer(tn.summer)
tp.summer = keep.summer(tp.summer)
chl.summer = keep.summer(chl.summer)
stoich.summer = keep.summer(stoich.summer)

#remove all points where TN:TP > 1000. This is based on data from Downing & McCauley, where
#the max epilimnetic value from the world's lakes was ~ 552. A rough doubling of that gives TN:TP (molar)
#of 1,000, to be conservative. 
stoich.summer = stoich.summer[stoich.summer$tn_tp_umol < 1000, ]


#find mean, coefficient of variance (covar) and number of observations
#for TN:TP, TN, TP in each lake for every year of observation
intra.annual.tn = aggregate(tn.summer$tn_umol, tn.summer[,c("lagoslakeid", "sampleyear")], FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), nobs=length(x)))
intra.annual.tp = aggregate(tp.summer$tp_umol, tp.summer[,c("lagoslakeid", "sampleyear")], FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), nobs=length(x)))
intra.annual.tn.secchi = aggregate(tn.summer$secchi, tn.summer[,c("lagoslakeid", "sampleyear")], FUN=function(x) c(mean=mean(x), sd=sd(x), covar=sd(x)/mean(x), nobs=length(x)))
intra.annual.tp.secchi = aggregate(tp.summer$secchi, tp.summer[,c("lagoslakeid", "sampleyear")], FUN=function(x) c(mean=mean(x), sd=sd(x), covar=sd(x)/mean(x), nobs=length(x)))
intra.annual.chl = aggregate(chl.summer$chla, chl.summer[,c("lagoslakeid", "sampleyear")], FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), nobs=length(x)))
intra.annual.tntp = aggregate(stoich.summer$tn_tp_umol, stoich.summer[,c("lagoslakeid", "sampleyear")], FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), nobs=length(x)))

year.means.tn.secchi = data.frame(lagoslakeid = intra.annual.tn.secchi$lagoslakeid, 
                                  sampleyear = intra.annual.tn.secchi$sampleyear, 
                                  secchi = intra.annual.tn.secchi$x[,1], 
                                  sd = intra.annual.tn.secchi$x[,2], 
                                  covar = intra.annual.tn.secchi$x[,3], 
                                  nobs = intra.annual.tn.secchi$x[,4])
year.means.tp.secchi = data.frame(lagoslakeid = intra.annual.tp.secchi$lagoslakeid, 
                                  sampleyear = intra.annual.tp.secchi$sampleyear, 
                                  secchi = intra.annual.tp.secchi$x[,1], 
                                  sd = intra.annual.tp.secchi$x[,2], 
                                  covar = intra.annual.tp.secchi$x[,3], 
                                  nobs = intra.annual.tp.secchi$x[,4])

year.means.tn = data.frame(lagoslakeid = year.means.tn$lagoslakeid, 
                           sampleyear = year.means.tn$sampleyear, 
                           tn_umol = year.means.tn$tn_umol,
                           secchi = year.means.tn.secchi$secchi,
                           nobs = year.means.tn$nobs)


year.means.tp = data.frame(lagoslakeid = year.means.tp$lagoslakeid, 
                           sampleyear = year.means.tp$sampleyear, 
                           tp_umol = year.means.tp$tp_umol,
                           secchi = year.means.tp.secchi$secchi,
                           nobs = year.means.tp$nobs)

year.means.tntp = data.frame(lagoslakeid = intra.annual.tntp$lagoslakeid, 
                             sampleyear = intra.annual.tntp$sampleyear, 
                             tn_tp_umol = intra.annual.tntp$x[,1], 
                             nobs = intra.annual.tntp$x[,4])

year.means.chl = data.frame(lagoslakeid = intra.annual.chl$lagoslakeid, 
                            sampleyear = intra.annual.chl$sampleyear, 
                            chl = intra.annual.chl$x[,1], 
                            nobs = intra.annual.chl$x[,4])

# merge with Iowa data that was received from J. Downing and has been 
# processed in teh file "Iowa lakes import.R"

setwd("C:/Users/Samantha/Dropbox/CSI_LIMNO_Manuscripts-presentations/CSI_Nitrogen MSs/Time series/Data")
iowa = read.csv("Iowa_LAGOS merged.csv", header = TRUE)

year.means.tn = rbind(year.means.tn, iowa[,c(2,3,4,7,8)])
year.means.tp = rbind(year.means.tp, iowa[,c(2,3,5,7,8)])
year.means.tntp = rbind(year.means.tntp, iowa[,c(2,3,6,8)])
year.means.chl = rbind(year.means.chl, iowa[,c(2,3,8,9)])
#limit analysis to only 1990-present
modern.tn = year.means.tn[year.means.tn$sampleyear > 1989, ]
modern.tp = year.means.tp[year.means.tp$sampleyear > 1989, ]
modern.tntp = year.means.tntp[year.means.tntp$sampleyear > 1989, ]
modern.chl = year.means.chl[year.means.chl$sampleyear > 1989, ]

#limit to lakes that have at least one observation in each decade of time extent
# one in 1990-2000, 2001-2011

occurance.filter <- function(x) {
  keep.10 = c()
  for (i in 1:length(unique(x$lagoslakeid))) {
    lake = unique(x$lagoslakeid)[i]
    years = x$sampleyear[x$lagoslakeid == lake]
    
    test.10 = c(length(which(years %in% c(1990:2000)))>0, 
                length(which(years %in% c(2001:2011)))>0)
    
    
    if (length(which(test.10 == FALSE)) > 0) {
      keep.10[i] = FALSE
    } else {
      keep.10[i] = TRUE
    }
  }
  lakes.10 = unique(x$lagoslakeid)[keep.10 == TRUE]
  return(x[x$lagoslakeid %in% lakes.10, ])
}

modern.tn.e10 = occurance.filter(modern.tn)
modern.tp.e10 = occurance.filter(modern.tp)
modern.tntp.e10 = occurance.filter(modern.tntp)
modern.chl.e10 = occurance.filter(modern.chl)

#limit to lakes that cover at least 5 years in duration

duration.filter <- function(x) {
  lakes = unique(x$lagoslakeid)
  years = c(1990:2011)
  keep = c()
  for (j in 1:length(unique(x$lagoslakeid))) {
    lake.years = x$sampleyear[x$lagoslakeid == lakes[j]]
    spread = max(lake.years) - min(lake.years)
    if (spread > 4){
      keep[j] = TRUE
    } else {
      keep[j] = FALSE
    }
  }
  lakes = unique(x$lagoslakeid)[keep == TRUE]
  return(x[x$lagoslakeid %in% lakes, ])
}
modern.chl.e10 = duration.filter(modern.chl.e10)
modern.tp.e10 = duration.filter(modern.tp.e10)
modern.tn.e10 = duration.filter(modern.tn.e10)
modern.tntp.e10 = duration.filter(modern.tntp.e10)

# create a mixed model where the nutrient is the response, year is the 
# the predictor, and slopes and intercepts are allowed to vary by lake (grouping)

modern.tp.e10$sampleyear_cor = modern.tp.e10$sampleyear - 1990
modern.tn.e10$sampleyear_cor = modern.tn.e10$sampleyear - 1990
modern.chl.e10$sampleyear_cor = modern.chl.e10$sampleyear - 1990
modern.tntp.e10$sampleyear_cor = modern.tntp.e10$sampleyear - 1990

#write data files
setwd("C:/Users/Samantha/Dropbox/CSI_LIMNO_Manuscripts-presentations/CSI_Nitrogen MSs/Time series/Data")
write.table(modern.tn.e10, "timeseries_data_tn.txt")
write.table(modern.tp.e10, "timeseries_data_tp.txt")
write.table(modern.chl.e10, "timeseries_data_chl.txt")
write.table(modern.tntp.e10, "timeseries_data_tntp.txt")