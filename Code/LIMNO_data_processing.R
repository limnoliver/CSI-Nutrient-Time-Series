library(reshape)
setwd("~/Dropbox/CSI-LIMNO_DATA/LAGOSData/Version1.054.1")
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

for (i in 1:nrow(data)){
  if (is.na(data$tn[i]) == TRUE) {
    data$tn_combined[i] = data$tn_calculated[i]
  } else {
    data$tn_combined[i] = data$tn[i]
  }
}

# create a data subset of TN and TP (independent) where there has to be 
# TN or TP observations, respectively

tn = data[is.na(data$tn_combined)==FALSE, ]
tp = data[is.na(data$tp)==FALSE, ]

#create molar version of nutrients
tn$tn_umol = tn$tn_combined/14.01
tp$tp_umol = tp$tp/30.97

#limit observations from JUN 15-AUG 15
# first, just keep samples in JUN, JUL, AUG, SEP
tn.summer = tn[tn$samplemonth == 6|tn$samplemonth == 7|tn$samplemonth==8|tn$samplemonth==9, ]
tp.summer = tp[tp$samplemonth == 6|tp$samplemonth == 7|tp$samplemonth==8|tp$samplemonth==9, ]

# now, get rid of first half of June and second half of Sept
tn.summer$sampleday = format(tn.summer$sampledate, "%d")
tn.summer$sampleday = as.numeric(tn.summer$sampleday)

tp.summer$sampleday = format(tp.summer$sampledate, "%d")
tp.summer$sampleday = as.numeric(tp.summer$sampleday)

keep.june = which(tn.summer$samplemonth == 6 & tn.summer$sampleday >= 15)
keep.july = which(tn.summer$samplemonth == 7)
keep.august = which(tn.summer$samplemonth == 8)
keep.september = which(tn.summer$samplemonth==9 & tn.summer$sampleday <= 15)
keep.all = c(keep.june, keep.july, keep.august, keep.september)
keep.all = as.numeric(keep.all)

tn.summer = tn.summer[keep.all, ]

keep.june = which(tp.summer$samplemonth == 6 & tp.summer$sampleday >= 15)
keep.july = which(tp.summer$samplemonth == 7)
keep.august = which(tp.summer$samplemonth == 8)
keep.september = which(tp.summer$samplemonth==9 & tp.summer$sampleday <= 15)
keep.all = c(keep.june, keep.july, keep.august, keep.september)
keep.all = as.numeric(keep.all)

tp.summer = tp.summer[keep.all, ]

#find mean, coefficient of variance (covar) and number of observations
#for TN:TP, TN, TP in each lake for every year of observation
intra.annual.tn = aggregate(tn.summer$tn_umol, tn.summer[,c("lagoslakeid", "sampleyear")], FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), nobs=length(x)))
intra.annual.tp = aggregate(tp.summer$tp_umol, tp.summer[,c("lagoslakeid", "sampleyear")], FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), nobs=length(x)))
intra.annual.tn.secchi = aggregate(tn.summer$secchi, tn.summer[,c("lagoslakeid", "sampleyear")], FUN=function(x) c(mean=mean(x), sd=sd(x), covar=sd(x)/mean(x), nobs=length(x)))
intra.annual.tp.secchi = aggregate(tp.summer$secchi, tp.summer[,c("lagoslakeid", "sampleyear")], FUN=function(x) c(mean=mean(x), sd=sd(x), covar=sd(x)/mean(x), nobs=length(x)))


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

#limit analysis to only 1990-present
modern.tn = year.means.tn[year.means.tn$sampleyear > 1989, ]
modern.tp = year.means.tp[year.means.tp$sampleyear > 1989, ]

#limit analysis to only lakes that have >= 15 years of data post-1990
#first, calculate how many years of observation each lake has (plus calculate mean stoich over all years)
duration.tn = aggregate(modern.tn$tn_umol, by=list(modern.tn$lagoslakeid), FUN=function(x) c(mean=mean(x),sd=sd(x),covar=sd(x)/mean(x), nobs=length(x)))

#limit to lakes that have at least one observation in each decade of time extent
# one in 1990-2000, 2001-2011
keep.10 = c()

for (i in 1:length(unique(modern.tn$lagoslakeid))) {
  lake = unique(modern.tn$lagoslakeid)[i]
  years = modern.tn$sampleyear[modern.tn$lagoslakeid == lake]
  
  test.10 = c(length(which(years %in% c(1990:2000)))>0, 
              length(which(years %in% c(2001:2011)))>0)
  
 
  if (length(which(test.10 == FALSE)) > 0) {
    keep.10[i] = FALSE
  } else {
    keep.10[i] = TRUE
  }
}
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