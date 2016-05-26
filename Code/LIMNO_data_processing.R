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

year.means.tn = rbind(year.means.tn, iowa[,c(2,3,4,7,8)])
year.means.tp = rbind(year.means.tp, iowa[,c(2,3,5,7,8)])

#limit analysis to only 1990-present
modern.tn = year.means.tn[year.means.tn$sampleyear > 1989, ]
modern.tp = year.means.tp[year.means.tp$sampleyear > 1989, ]


#limit analysis to only 1990-present
modern = year.means[year.means$sampleyear > 1989, ]

#limit analysis to only lakes that have >= 15 years of data post-1990
#first, calculate how many years of observation each lake has (plus calculate mean stoich over all years)
duration = aggregate(modern$tn_tp_umol, by=list(modern$lagoslakeid), FUN=function(x) c(mean=mean(x),sd=sd(x),covar=sd(x)/mean(x), nobs=length(x)))

#create a list of lagoslakeid of each lake that has >= 15 years of data
modern.15 = modern[modern$lagoslakeid %in% duration$Group.1[duration$x[,4]>=15], ]

#create a list of lagoslakeid of each lake that has >= 10 years of data 
modern.10 = modern[modern$lagoslakeid %in% duration$Group.1[duration$x[,4]>=10], ]

#further limit the modern.10 analysis to lakes where there is no more than
#5 consecutive years with no observations

# i = years, j = lakes
lakes = unique(modern.10$lagoslakeid)
years = c(1990:2011)
keep = c()
for (j in 1:length(unique(modern.10$lagoslakeid))) {
  lake.years = modern.10$sampleyear[modern.10$lagoslakeid == lakes[j]]
  overlap = years %in% lake.years
  overlap = data.frame(lengths = rle(overlap)[[1]], values = rle(overlap)[[2]])
  test = which(overlap$lengths >=5 & overlap$values == FALSE)
  if (length(test) > 0){
    keep[j] = FALSE
  } else {
    keep[j] = TRUE
  }
}

#we gain 17 lakes (90 to 107) by changing the filter rule
keep.id = lakes[keep == TRUE]
modern.10 = modern.10[which(modern.10$lagoslakeid %in% keep.id), ]

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