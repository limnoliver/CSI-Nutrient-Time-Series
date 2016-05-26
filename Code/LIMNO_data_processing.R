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
data$tn_combined[is.na(data$tn_combined) == TRUE] = data$tn_calculated[is.na(data$tn_combined) == TRUE]

for (i in 1:nrow(data)){
  if (is.na(data$tn[i]) == TRUE) {
    data$tn_combined[i] = data$tn_calculated[i]
  } else {
    data$tn_combined[i] = data$tn[i]
  }
}

#create a data subset which only includes instances where
#both TP and TN_combined exist

stoich = data[is.na(data$tn_combined)==FALSE & is.na(data$tp)==FALSE, ]

#create a column tn_tp=TN:TP ratio (by mass)
stoich$tn_tp = stoich$tn_combined/stoich$tp

#create molar version of ratio and nutrients
stoich$tn_tp_umol = stoich$tn_tp * (30.97/14.01)
stoich$tn_umol = stoich$tn_combined/14.01
stoich$tp_umol = stoich$tp/30.97

#limit observations from JUN 15-AUG 15
# first, just keep samples in JUN, JUL, AUG, SEP
stoich.summer = stoich[stoich$samplemonth == 6|stoich$samplemonth == 7|stoich$samplemonth==8|stoich$samplemonth==9, ]

# now, get rid of first half of June and second half of Sept
stoich.summer$sampleday = format(stoich.summer$sampledate, "%d")
stoich.summer$sampleday = as.numeric(stoich.summer$sampleday)

keep.june = which(stoich.summer$samplemonth == 6 & stoich.summer$sampleday >= 15)
keep.july = which(stoich.summer$samplemonth == 7)
keep.august = which(stoich.summer$samplemonth == 8)
keep.september = which(stoich.summer$samplemonth==9 & stoich.summer$sampleday <= 15)
keep.all = c(keep.june, keep.july, keep.august, keep.september)
keep.all = as.numeric(keep.all)

stoich.summer = stoich.summer[keep.all, ]

#remove all points where TN:TP > 1000. This is based on data from Downing & McCauley, where
#the max epilimnetic value from the world's lakes was ~ 552. A rough doubling of that gives TN:TP (molar)
#of 1,000, to be conservative. This gets rid of 64 observations

stoich.summer = stoich.summer[stoich.summer$tn_tp_umol < 1000, ]

#find mean, coefficient of variance (covar) and number of observations
#for TN:TP, TN, TP in each lake for every year of observation
intra.annual.tntp = aggregate(stoich.summer$tn_tp_umol, stoich.summer[,c("lagoslakeid", "sampleyear")], FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), nobs=length(x)))
intra.annual.tn = aggregate(stoich.summer$tn_umol, stoich.summer[,c("lagoslakeid", "sampleyear")], FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), nobs=length(x)))
intra.annual.tp = aggregate(stoich.summer$tp_umol, stoich.summer[,c("lagoslakeid", "sampleyear")], FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), nobs=length(x)))
intra.annual.secchi = aggregate(stoich.summer$secchi, stoich.summer[,c("lagoslakeid", "sampleyear")], FUN=function(x) c(mean=mean(x), sd=sd(x), covar=sd(x)/mean(x), nobs=length(x)))
year.means.tntp = data.frame(lagoslakeid = intra.annual.tntp$lagoslakeid, 
                        sampleyear = intra.annual.tntp$sampleyear, 
                        tn_tp_umol = intra.annual.tntp$x[,1], 
                        sd = intra.annual.tntp$x[,2], 
                        covar = intra.annual.tntp$x[,3], 
                        nobs = intra.annual.tntp$x[,4])
year.means.tn = data.frame(lagoslakeid = intra.annual.tn$lagoslakeid, 
                             sampleyear = intra.annual.tn$sampleyear, 
                             tn_umol = intra.annual.tn$x[,1], 
                             sd = intra.annual.tn$x[,2], 
                             covar = intra.annual.tn$x[,3], 
                             nobs = intra.annual.tn$x[,4])
year.means.tp = data.frame(lagoslakeid = intra.annual.tp$lagoslakeid, 
                           sampleyear = intra.annual.tp$sampleyear, 
                           tp_umol = intra.annual.tp$x[,1], 
                           sd = intra.annual.tp$x[,2], 
                           covar = intra.annual.tp$x[,3], 
                           nobs = intra.annual.tp$x[,4])
year.means.secchi = data.frame(lagoslakeid = intra.annual.secchi$lagoslakeid, 
                           sampleyear = intra.annual.secchi$sampleyear, 
                           secchi = intra.annual.secchi$x[,1], 
                           sd = intra.annual.secchi$x[,2], 
                           covar = intra.annual.secchi$x[,3], 
                           nobs = intra.annual.secchi$x[,4])

year.means = data.frame(lagoslakeid = year.means.tntp$lagoslakeid, 
                        sampleyear = year.means.tntp$sampleyear, 
                        tn_umol = year.means.tn$tn_umol,
                        tp_umol = year.means.tp$tp_umol,
                        tn_tp_umol = year.means.tntp$tn_tp_umol,
                        secchi = year.means.secchi$secchi,
                        nobs = year.means.tntp$nobs)

# merge with Iowa data that was received from J. Downing and has been 
# processed in the file "Iowa lakes import.R" 

setwd("C:/Users/Samantha/Dropbox/CSI_LIMNO_Manuscripts-presentations/CSI_Nitrogen MSs/Time series/Data")
iowa = read.csv("Iowa_LAGOS merged.csv", header = TRUE)

year.means = rbind(year.means, iowa[,c(2:8)])

#limit analysis to only 1990-present
modern = year.means[year.means$sampleyear > 1989, ]

#limit to lakes that have at least one observation in each decade of time extent
# one in 1990-2000, 2001-2011
keep.10 = c()

for (i in 1:length(unique(modern$lagoslakeid))) {
  lake = unique(modern$lagoslakeid)[i]
  years = modern$sampleyear[modern$lagoslakeid == lake]
  
  test.10 = c(length(which(years %in% c(1990:2000)))>0, 
              length(which(years %in% c(2001:2011)))>0)
  
  
  if (length(which(test.10 == FALSE)) > 0) {
    keep.10[i] = FALSE
  } else {
    keep.10[i] = TRUE
  }
}

lakes.10 = unique(modern$lagoslakeid)[keep.10 == TRUE]
modern.e10 = modern[modern$lagoslakeid %in% lakes.10, ]


# create a mixed model where the nutrient is the response, year is the 
# the predictor, and slopes and intercepts are allowed to vary by lake (grouping)

modern.15$sampleyear_cor = modern.15$sampleyear - 1990
modern.10$sampleyear_cor = modern.10$sampleyear - 1990
modern$sampleyear_cor = modern$sampleyear - 1990

## create a column where tn, tp, tn:tp are standardized by the 
## initial (1990 or first year) concentration in the lake

for (i in 1:length(modern.15$lagoslakeid)){
  lake = modern.15$lagoslakeid[i]
  modern.15$tn_umol_stand[i] = (modern.15$tn_umol[i] - modern.15$tn_umol[modern.15$lagoslakeid == lake & modern.15$sampleyear_cor == min(modern.15$sampleyear_cor[modern.15$lagoslakeid == lake])])/modern.15$tn_umol[modern.15$lagoslakeid == lake & modern.15$sampleyear_cor == min(modern.15$sampleyear_cor[modern.15$lagoslakeid == lake])] 
  modern.15$tp_umol_stand[i] = (modern.15$tp_umol[i] - modern.15$tp_umol[modern.15$lagoslakeid == lake & modern.15$sampleyear_cor == min(modern.15$sampleyear_cor[modern.15$lagoslakeid == lake])])/modern.15$tp_umol[modern.15$lagoslakeid == lake & modern.15$sampleyear_cor == min(modern.15$sampleyear_cor[modern.15$lagoslakeid == lake])] 
  modern.15$tn_tp_umol_stand[i] = (modern.15$tn_tp_umol[i] - modern.15$tn_tp_umol[modern.15$lagoslakeid == lake & modern.15$sampleyear_cor == min(modern.15$sampleyear_cor[modern.15$lagoslakeid == lake])])/modern.15$tn_tp_umol[modern.15$lagoslakeid == lake & modern.15$sampleyear_cor == min(modern.15$sampleyear_cor[modern.15$lagoslakeid == lake])] 
  modern.15$secchi_stand[i] = (modern.15$secchi[i] - modern.15$secchi[modern.15$lagoslakeid == lake & modern.15$sampleyear_cor == min(modern.15$sampleyear_cor[modern.15$lagoslakeid == lake])])/modern.15$secchi[modern.15$lagoslakeid == lake & modern.15$sampleyear_cor == min(modern.15$sampleyear_cor[modern.15$lagoslakeid == lake])] 
}

for (i in 1:length(modern.10$lagoslakeid)){
  lake = modern.10$lagoslakeid[i]
  modern.10$tn_umol_stand[i] = (modern.10$tn_umol[i] - modern.10$tn_umol[modern.10$lagoslakeid == lake & modern.10$sampleyear_cor == min(modern.10$sampleyear_cor[modern.10$lagoslakeid == lake])])/modern.10$tn_umol[modern.10$lagoslakeid == lake & modern.10$sampleyear_cor == min(modern.10$sampleyear_cor[modern.10$lagoslakeid == lake])] 
  modern.10$tp_umol_stand[i] = (modern.10$tp_umol[i] - modern.10$tp_umol[modern.10$lagoslakeid == lake & modern.10$sampleyear_cor == min(modern.10$sampleyear_cor[modern.10$lagoslakeid == lake])])/modern.10$tp_umol[modern.10$lagoslakeid == lake & modern.10$sampleyear_cor == min(modern.10$sampleyear_cor[modern.10$lagoslakeid == lake])] 
  modern.10$tn_tp_umol_stand[i] = (modern.10$tn_tp_umol[i] - modern.10$tn_tp_umol[modern.10$lagoslakeid == lake & modern.10$sampleyear_cor == min(modern.10$sampleyear_cor[modern.10$lagoslakeid == lake])])/modern.10$tn_tp_umol[modern.10$lagoslakeid == lake & modern.10$sampleyear_cor == min(modern.10$sampleyear_cor[modern.10$lagoslakeid == lake])] 
  modern.10$secchi_stand[i] = (modern.10$secchi[i] - modern.10$secchi[modern.10$lagoslakeid == lake & modern.10$sampleyear_cor == min(modern.10$sampleyear_cor[modern.10$lagoslakeid == lake])])/modern.10$secchi[modern.10$lagoslakeid == lake & modern.10$sampleyear_cor == min(modern.10$sampleyear_cor[modern.10$lagoslakeid == lake])] 
}