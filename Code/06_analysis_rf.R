require(randomForest)
require(plyr)
## use random forest to test which lakes are changing in tn, tp, tn:tp, and chl

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

# reduce variables down to 2001 LULC data
# also include topography metrics to test

iws.lulc = iws.lulc[,c(1,49:80,147:156)]

# create urban, row crop, pasture, wetland categories

iws.lulc$iws_urban = iws.lulc$iws_nlcd2001_pct_22 + iws.lulc$iws_nlcd2001_pct_23 + iws.lulc$iws_nlcd2001_pct_24
iws.lulc$iws_crop = iws.lulc$iws_nlcd2001_pct_82
iws.lulc$iws_pasture = iws.lulc$iws_nlcd2001_pct_81
iws.lulc$iws_forest = iws.lulc$iws_nlcd2001_pct_41 + iws.lulc$iws_nlcd2001_pct_42 + iws.lulc$iws_nlcd2001_pct_43

# now narrow predictors

iws.lulc = iws.lulc[,c(1,34:47)]

# get rid of terrain metrics, with the exception of mean slope

iws.lulc = iws.lulc[,c(1,4,10:15)]


# rename ID var to allow merge with lake.info

names(iws.lulc)[4] = "lagoslakeid"
iws.lulc = iws.lulc[,c(2:8)]





# merge lake info and iws lulc

#use this one for coef
lake.predictors = merge(all.lakes, iws.lulc, by = "lagoslakeid", all.x = TRUE)

# calculate the number of years of observation

counts = count(data.tn, vars = "lagoslakeid")
names(counts)[2] = "tn_nyear"
lake.predictors = merge(lake.predictors, counts, by = "lagoslakeid", all.x = TRUE)

counts = count(data.tp, vars = "lagoslakeid")
names(counts)[2] = "tp_nyear"
lake.predictors = merge(lake.predictors, counts, by = "lagoslakeid", all.x = TRUE)

counts = count(data.tntp, vars = "lagoslakeid")
names(counts)[2] = "tntp_nyear"
lake.predictors = merge(lake.predictors, counts, by = "lagoslakeid", all.x = TRUE)

counts = count(data.chl, vars = "lagoslakeid")
names(counts)[2] = "chl_nyear"
lake.predictors = merge(lake.predictors, counts, by = "lagoslakeid", all.x = TRUE)

# and the median year of observation

med = tapply(data.tn$sampleyear, data.tn$lagoslakeid, FUN = median)
med = data.frame(lagoslakeid = row.names(med), tn_medyear = as.numeric(med))
lake.predictors = merge(lake.predictors, med, by = "lagoslakeid", all.x = TRUE)

med = tapply(data.tp$sampleyear, data.tp$lagoslakeid, FUN = median)
med = data.frame(lagoslakeid = row.names(med), tp_medyear = as.numeric(med))
lake.predictors = merge(lake.predictors, med, by = "lagoslakeid", all.x = TRUE)

med = tapply(data.tntp$sampleyear, data.tntp$lagoslakeid, FUN = median)
med = data.frame(lagoslakeid = row.names(med), tntp_medyear = as.numeric(med))
lake.predictors = merge(lake.predictors, med, by = "lagoslakeid", all.x = TRUE)

med = tapply(data.chl$sampleyear, data.chl$lagoslakeid, FUN = median)
med = data.frame(lagoslakeid = row.names(med), chl_medyear = as.numeric(med))
lake.predictors = merge(lake.predictors, med, by = "lagoslakeid", all.x = TRUE)

# and the mean observed concentration

mean.val = tapply(data.tn$tn_umol, data.tn$lagoslakeid, FUN = mean)
mean.val = data.frame(lagoslakeid = row.names(mean.val), tn_meanval = as.numeric(mean.val))
lake.predictors = merge(lake.predictors, mean.val, by = "lagoslakeid", all.x = TRUE)

mean.val = tapply(data.tp$tp_umol, data.tp$lagoslakeid, FUN = mean)
mean.val = data.frame(lagoslakeid = row.names(mean.val), tp_meanval = as.numeric(mean.val))
lake.predictors = merge(lake.predictors, mean.val, by = "lagoslakeid", all.x = TRUE)

mean.val = tapply(data.tntp$tn_tp_umol, data.tntp$lagoslakeid, FUN = mean)
mean.val = data.frame(lagoslakeid = row.names(mean.val), tntp_meanval = as.numeric(mean.val))
lake.predictors = merge(lake.predictors, mean.val, by = "lagoslakeid", all.x = TRUE)

mean.val = tapply(data.chl$chl, data.chl$lagoslakeid, FUN = mean)
mean.val = data.frame(lagoslakeid = row.names(mean.val), chl_meanval = as.numeric(mean.val))
lake.predictors = merge(lake.predictors, mean.val, by = "lagoslakeid", all.x = TRUE)

## find lakes that are missing lake depth data, and merge with predicted data from Oliver et al 2016
setwd("C:/Users/Samantha/Dropbox/CSI_LIMNO_Manuscripts-presentations/CSI_lake depth/R Output")
setwd("~/Dropbox/CSI_LIMNO_Manuscripts-presentations/CSI_lake depth/R Output")
pred.depths = read.table("lake_depth_data.txt", header = TRUE)

missing.depth = lake.predictors$lagoslakeid[is.na(lake.predictors$maxdepth)]
filled.depths = pred.depths[pred.depths$lagoslakeid %in% missing.depth, ]
filled.depths = filled.depths[,c(1,8)]
names(filled.depths)[2] = "maxdepth"

lake.predictors$maxdepth[lake.predictors$lagoslakeid %in% filled.depths$lagoslakeid]

lake.predictors$maxdepth[lake.predictors$lagoslakeid %in% filled.depths$lagoslakeid] = filled.depths$maxdepth

lake.predictors = merge(lake.predictors, lake.info[,c("lagoslakeid", "lagosname1")], by = "lagoslakeid", all.x = TRUE)

#now add iws connectivity

iws.conn = read.table("iws_conn.txt", header = TRUE)

iws.conn = iws.conn[,c(4,7,12,27,82,84,86,152)]
names(iws.conn)[8] = "lagoslakeid"
#get rid of lake average size because too many NAs
iws.conn = iws.conn[,c(2:8)]

lake.predictors = merge(lake.predictors, iws.conn, by = "lagoslakeid", all.x = TRUE)

#add lake blups

lake.predictors = merge(lake.predictors, lake.blups, by = "lagoslakeid", all.x = TRUE)

#now use lake predictors in rf of lake BLUPS


# now add climate and hydrology variables that are calculated at the
# hu12 scale

setwd("~/Dropbox/CSI-LIMNO_DATA/LAGOSGeoData/LAGOS_VER1.03")
setwd("C:/Users/Samantha/Dropbox/CSI-LIMNO_DATA/LAGOSGeoData/LAGOS_VER1.03")

chag = read.table("hu4_chag.txt", header = TRUE)
hu4.conn = read.table("hu4_conn.txt", header = TRUE)
hu4.lulc = read.table("hu4_lulc.txt", header = TRUE)

atm.dep = chag[,c(1,60,64,68,72,76)]
atm.dep = gather(atm.dep, key = "year", value = "deposition",2:6)
test = gsub("(hu4_dep_totaln_)(\\d+)(_mean)", replacement = "\\2", atm.dep$year)
atm.dep$year = test

#calculate trend in atm dep for each region

dep.change = c()
for (i in 1:length(unique(atm.dep$hu4_zoneid))){
  dat = atm.dep[atm.dep$hu4_zoneid == unique(atm.dep$hu4_zoneid)[i], ]
  dat.lm = lm(dat$deposition~as.numeric(dat$year))
  dep.change[i] = as.numeric(dat.lm$coefficients[2])
}

dep.change = data.frame(hu4_zoneid = unique(atm.dep$hu4_zoneid),
                        dep_change = dep.change)

lake.predictors = merge(lake.predictors, dep.change, "hu4_zoneid", all.x = TRUE)

# calculate climate trends for each hu4
setwd("C:/Users/Samantha/Dropbox/CSI_LIMNO_Manuscripts-presentations/CSI_Nitrogen MSs/Time series/Data")
hu4.ppt.annual = read.table("HU4_ppt_annual.txt", header = TRUE)
hu4.tmean.annual = read.table("HU4_tmean_annual.txt", header = TRUE)

ppt.change = c()
for (i in 1:length(unique(hu4.ppt.annual$ZoneID))){
  dat = hu4.ppt.annual[hu4.ppt.annual$ZoneID == unique(hu4.ppt.annual$ZoneID)[i], ]
  dat.lm = lm((dat$annual/100)~as.numeric(dat$year))
  ppt.change[i] = as.numeric(dat.lm$coefficients[2])
}
ppt.change = data.frame(hu4_zoneid = unique(hu4.ppt.annual$ZoneID),
                        ppt_change = ppt.change)

tmean.change = c()
for (i in 1:length(unique(hu4.tmean.annual$ZoneID))){
  dat = hu4.tmean.annual[hu4.tmean.annual$ZoneID == unique(hu4.tmean.annual$ZoneID)[i], ]
  dat.lm = lm((dat$annual/100)~as.numeric(dat$year))
  tmean.change[i] = as.numeric(dat.lm$coefficients[2])
}
tmean.change = data.frame(hu4_zoneid = unique(hu4.tmean.annual$ZoneID),
                        tmean_change = tmean.change)

# add climate change vars to lake.predictors

lake.predictors = merge(lake.predictors, ppt.change, "hu4_zoneid", all.x = TRUE)
lake.predictors = merge(lake.predictors, tmean.change, "hu4_zoneid", all.x = TRUE)

#add ppt and tmean from 1990 and 2011
names(hu4.tmean.annual)[1] = "hu4_zoneid"
names(hu4.ppt.annual)[1] = "hu4_zoneid"

tmean.1990 = hu4.tmean.annual[hu4.tmean.annual$year == 1990, c("hu4_zoneid", "annual")]
names(tmean.1990)[2] = "tmean.1990"
tmean.2011 = hu4.tmean.annual[hu4.tmean.annual$year == 2011, c("hu4_zoneid", "annual")]
names(tmean.2011)[2] = "tmean.2011"
ppt.1990 = hu4.ppt.annual[hu4.ppt.annual$year == 1990, c("hu4_zoneid", "annual")]
names(ppt.1990)[2] = "ppt.1990"
ppt.2011 = hu4.ppt.annual[hu4.ppt.annual$year == 2011, c("hu4_zoneid", "annual")]
names(ppt.2011)[2] = "ppt.2011"

lake.predictors = merge(lake.predictors, tmean.1990, "hu4_zoneid", all.x = TRUE)
lake.predictors = merge(lake.predictors, tmean.2011, "hu4_zoneid", all.x = TRUE)
lake.predictors = merge(lake.predictors, ppt.1990, "hu4_zoneid", all.x = TRUE)
lake.predictors = merge(lake.predictors, ppt.2011, "hu4_zoneid", all.x = TRUE)

# limit to variables of interest

chag = chag[,c(1,4,60,76,80,84,88,92,96,100,104)]

hu4.lulc$hu4_urban = hu4.lulc$hu4_nlcd2001_pct_22 + hu4.lulc$hu4_nlcd2001_pct_23 + hu4.lulc$hu4_nlcd2001_pct_24
hu4.lulc$hu4_crop = hu4.lulc$hu4_nlcd2001_pct_82
hu4.lulc$hu4_pasture = hu4.lulc$hu4_nlcd2001_pct_81
hu4.lulc$hu4_forest = hu4.lulc$hu4_nlcd2001_pct_41 + hu4.lulc$hu4_nlcd2001_pct_42 + hu4.lulc$hu4_nlcd2001_pct_43

hu4.lulc = hu4.lulc[,c(1,8,158,161,167:170)]#got rid of impervious because too many NAs

hu4.conn = hu4.conn[,c(1,4,7,12,26,79)]

#note - from hu4.conn, got rid of some vars (including average lake size and stream vars) because there
# were too many NAs
hu4.predictors = merge(chag, hu4.lulc, by = "hu4_zoneid", all.x = TRUE)
hu4.predictors = merge(hu4.predictors, hu4.conn, by = "hu4_zoneid")

# add to other lake predictors

# first, add huc4 region id's

lake.predictors = merge(lake.predictors,
                        hu4.predictors,
                        by = "hu4_zoneid",
                        all.x = TRUE)


#####################################
## run random forest model
#####################################

require(rpart)
require(randomForest)

#first get categorical response

lake.predictors$tn_change[!is.na(lake.predictors$tn_coef)] = "no change"
lake.predictors$tn_change[lake.predictors$tn_ymax<0] = "decreasing"
lake.predictors$tn_change[lake.predictors$tn_ymin>0] = "increasing"
lake.predictors$tn_change = as.factor(lake.predictors$tn_change)

lake.predictors$tp_change[!is.na(lake.predictors$tp_coef)] = "no change"
lake.predictors$tp_change[lake.predictors$tp_ymax<0] = "decreasing"
lake.predictors$tp_change[lake.predictors$tp_ymin>0] = "increasing"
lake.predictors$tp_change = as.factor(lake.predictors$tp_change)

lake.predictors$tntp_change[!is.na(lake.predictors$tntp_coef)] = "no change"
lake.predictors$tntp_change[lake.predictors$tntp_ymax<0] = "decreasing"
lake.predictors$tntp_change[lake.predictors$tntp_ymin>0] = "increasing"
lake.predictors$tntp_change = as.factor(lake.predictors$tntp_change)

lake.predictors$chl_change[!is.na(lake.predictors$chl_coef)] = "no change"
lake.predictors$chl_change[lake.predictors$chl_ymax<0] = "decreasing"
lake.predictors$chl_change[lake.predictors$chl_ymin>0] = "increasing"
lake.predictors$chl_change = as.factor(lake.predictors$chl_change)



rf.run.cat <- function(response, dat) {

  #filter data frame to only response and predictors

  vars.keep = grep(paste("^",response, "_change", sep = ""), names(dat))

  dat.keep = dat[,c(17:21,23:26,40:51,53:64,69:75, vars.keep)]

  dat.keep = dat.keep[complete.cases(dat.keep), ]

  pred.vars = names(dat.keep)[names(dat.keep) != paste(response, "_change", sep = "")]
  response.var = paste(response, "_change", sep = "")

  rf.formula <- as.formula(paste(paste(response.var, "~"), paste(pred.vars, collapse= "+")))

  n.min = min(summary(dat.keep[,length(dat.keep)]))
  rf<-randomForest(rf.formula,data=dat.keep,na.action=na.omit,importance=1, ntree=2000,
                   strata = dat.keep[,length(dat.keep)],
                   sampsize = rep(n.min, 3))
  return(rf)
}
rf.run.cat.lake <- function(response, dat) {

  #filter data frame to only response and predictors

  vars.keep = grep(paste("^",response, "_change", sep = ""), names(dat))

  dat.keep = dat[,c(17:21,23:26,40:45, vars.keep)]

  dat.keep = dat.keep[complete.cases(dat.keep), ]

  pred.vars = names(dat.keep)[names(dat.keep) != paste(response, "_change", sep = "")]
  response.var = paste(response, "_change", sep = "")

  rf.formula <- as.formula(paste(paste(response.var, "~"), paste(pred.vars, collapse= "+")))

  n.min = min(summary(dat.keep[,length(dat.keep)]))
  rf<-randomForest(rf.formula,data=dat.keep,na.action=na.omit,importance=1, ntree=2000,
                   strata = dat.keep[,length(dat.keep)],
                   sampsize = rep(n.min, 3))
  return(rf)
}
rf.run.cat.region <- function(response, dat) {

  #filter data frame to only response and predictors

  vars.keep = grep(paste("^",response, "_change", sep = ""), names(dat))

  dat.keep = dat[,c(46:51,53:64,69:75, vars.keep)]

  dat.keep = dat.keep[complete.cases(dat.keep), ]

  pred.vars = names(dat.keep)[names(dat.keep) != paste(response, "_change", sep = "")]
  response.var = paste(response, "_change", sep = "")

  rf.formula <- as.formula(paste(paste(response.var, "~"), paste(pred.vars, collapse= "+")))

  n.min = min(summary(dat.keep[,length(dat.keep)]))
  rf<-randomForest(rf.formula,data=dat.keep,na.action=na.omit,importance=1, ntree=2000,
                   strata = dat.keep[,length(dat.keep)],
                   sampsize = rep(n.min, 3))
  return(rf)
}

tn.rf.cat = rf.run.cat("tn", lake.predictors)
tp.rf.cat = rf.run.cat("tp", lake.predictors)
tntp.rf.cat = rf.run.cat("tntp", lake.predictors)
chl.rf.cat = rf.run.cat("chl", lake.predictors)

tn.rf.cat.lake = rf.run.cat.lake("tn", lake.predictors)
tp.rf.cat.lake = rf.run.cat.lake("tp", lake.predictors)
tntp.rf.cat.lake = rf.run.cat.lake("tntp", lake.predictors)
chl.rf.cat.lake = rf.run.cat.lake("chl", lake.predictors)

tn.rf.cat.region = rf.run.cat.region("tn", lake.predictors)
tp.rf.cat.region = rf.run.cat.region("tp", lake.predictors)
tntp.rf.cat.region = rf.run.cat.region("tntp", lake.predictors)
chl.rf.cat.region = rf.run.cat.region("chl", lake.predictors)

#####################################################
## use varSelRF to do variable selection
## set rf formula and dat.keep inside function run.rf
tn.test = VSURF(rf.formula, data = dat.keep)
tp.test = VSURF(rf.formula, data = dat.keep)
chl.test = VSURF(rf.formula, data = dat.keep)
tntp.test = VSURF(rf.formula, data = dat.keep)

# use VSURF outputs to show variable order
# have to set response var and run code above

tn.vars = pred.vars
tp.vars = pred.vars
tntp.vars = pred.vars
chl.vars = pred.vars

tn.var.keep = tn.vars[tn.test$varselect.interp]
tp.var.keep = tp.vars[tp.test$varselect.interp]
tntp.var.keep = tntp.vars[tntp.test$varselect.interp]
chl.var.keep = chl.vars[chl.test$varselect.interp]



# rerun RF models using variables selected

rf.rerun.cat <- function(response, dat) {

  #filter data frame to only response and predictors

  vars.keep = grep(paste("^",response, "_change", sep = ""), names(dat))
  filter.vars.keep = get(paste(response, ".var.keep", sep = ""))
  filter.vars.keep = which(names(dat) %in% filter.vars.keep)
  dat.keep = dat[,c(filter.vars.keep, vars.keep)]

  dat.keep = dat.keep[complete.cases(dat.keep), ]

  pred.vars = names(dat.keep)[names(dat.keep) != paste(response, "_change", sep = "")]
  response.var = paste(response, "_change", sep = "")

  rf.formula <- as.formula(paste(paste(response.var, "~"), paste(pred.vars, collapse= "+")))

  n.min = min(summary(dat.keep[,length(dat.keep)]))
  rf<-randomForest(rf.formula,data=dat.keep,na.action=na.omit,importance=1, ntree=2000,
                   strata = dat.keep[,length(dat.keep)],
                   sampsize = rep(n.min, 3))
  return(rf)
}

tn.rerun = rf.rerun.cat("tn", lake.predictors)
tp.rerun = rf.rerun.cat("tp", lake.predictors)
tntp.rerun = rf.rerun.cat("tntp", lake.predictors)
chl.rerun = rf.rerun.cat("chl", lake.predictors)


# put together a dataframe with top vars

#tn.imp = data.frame(Response_Variable = "tn",
#                    Predictor_Variable = row.names(varImpPlot(tn.rerun)),
#                    Per_inc_MSE = (as.numeric(varImpPlot(tn.rerun)[,1])/max(as.numeric(varImpPlot(tn.rerun)[,1]))))


tp.imp = data.frame(Response_Variable = "tp",
                    Predictor_Variable = row.names(varImpPlot(tp.rerun)),
                    Per_inc_MSE = (as.numeric(varImpPlot(tp.rerun)[,1])/max(as.numeric(varImpPlot(tp.rerun)[,1]))))
tp.imp = tp.imp[order(tp.imp$Per_inc_MSE, decreasing = TRUE)[1:5], ]

tntp.imp = data.frame(Response_Variable = "tntp",
                      Predictor_Variable = row.names(varImpPlot(tntp.rerun)),
                      Per_inc_MSE = (as.numeric(varImpPlot(tntp.rerun)[,1])/max(as.numeric(varImpPlot(tntp.rerun)[,1]))))
tntp.imp = tntp.imp[order(tntp.imp$Per_inc_MSE, decreasing = TRUE)[1:5], ]


#chl.imp = data.frame(Response_Variable = "chl",
                     Predictor_Variable = row.names(varImpPlot(chl.rerun)),
                     Per_inc_MSE = (as.numeric(varImpPlot(chl.rerun)[,1])/max(as.numeric(varImpPlot(chl.rerun)[,1]))))
#chl.imp = chl.imp[chl.imp$Predictor_Variable %in% chl.var.keep, ]

top.vars = rbind(tp.imp, tntp.imp)

#add tn and chl manually because they only had one
#variable after variable selection, and varImpPlot is
#not produced with only one var

top.vars$Response_Variable = as.character(top.vars$Response_Variable)
top.vars[11,] = c("tn", "hu4_dep_totaln_1990_mean", 1)
top.vars[12,] = c("chl", chl.var.keep, 1)
top.vars$Response_Variable = as.factor(top.vars$Response_Variable)

top.vars$Response_fullname = c(rep("TP Change", 5),rep("TN:TP Change", 5), "TN Change", "Chl Change")
top.vars$Predictor_fullname = c("WS % Crop",
                                "HUC4 Dam Density",
                                "HUC4 TN Deposition 2010",
                                "HUC4 Runoff",
                                "HUC4 % Pasture",
                                "HUC4 Road Density",
                                "WS Slope",
                                "Maximum Lake Depth",
                                "HU4 Mean Temperature",
                                "HUC4 TN Deposition Diff",
                                "HUC4 TN Deposition 1990",
                                "HUC4 Mean Temperature")

# change order of top.vars to reflect how I want them plotted


########## extra code


ggplot(top.vars, aes(x=Predictor_Variable, y=Per_inc_MSE, fill=Response_Variable)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(family="Helvetica")) +
  scale_fill_grey(start = 0.8, end = .1)


save.image("C:/Users/Samantha/Dropbox/Time series/19AUG16.RData")


## rerun chl RF to include TN and TP

rf.run.cat.chl <- function(response, dat) {

  #filter data frame to only response and predictors

  vars.keep = grep(paste("^",response, "_change", sep = ""), names(dat))

  dat.keep = dat[,c(3,6,17:21,23:26,40:51,53:64,69:75, vars.keep)]

  dat.keep = dat.keep[complete.cases(dat.keep), ]

  pred.vars = names(dat.keep)[names(dat.keep) != paste(response, "_change", sep = "")]
  response.var = paste(response, "_change", sep = "")

  rf.formula <- as.formula(paste(paste(response.var, "~"), paste(pred.vars, collapse= "+")))

  n.min = min(summary(dat.keep[,length(dat.keep)]))
  rf<-randomForest(rf.formula,data=dat.keep,na.action=na.omit,importance=1, ntree=2000,
                   strata = dat.keep[,length(dat.keep)],
                   sampsize = rep(n.min, 3))
  return(rf)
}

chl.all.dat = lake.predictors[!is.na(lake.predictors$tn_coef_mean)&!is.na(lake.predictors$tp_coef_mean)&!is.na(lake.predictors$chl_coef_mean), ]
chl.all = rf.run.cat.chl("chl", chl.all.dat)
chl.test = VSURF(rf.formula, data=dat.keep,na.action=na.omit,
                 strata = dat.keep[,length(dat.keep)],
                 sampsize = rep(n.min, 3))
