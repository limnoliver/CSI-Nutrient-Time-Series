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

#remove all points where TN:TP > 1000. This is based on data from Downing & McCauley, where
#the max epilimnetic value from the world's lakes was ~ 552. A rough doubling of that gives TN:TP (molar)
#of 1,000, to be conservative. This gets rid of 64 observations

stoich.summer = stoich.summer[stoich.summer$tn_tp_umol < 1000, ]

#a function that allows you to calculate SE (and other stats)
#by groups in a data frame
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

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