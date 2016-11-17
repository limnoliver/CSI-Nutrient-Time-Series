require(randomForest)
require(VSURF)
require(plyr)

# ==============================================
# PURPOSE
# This code uses the predictor variables located at: 
# to predict whether a lake is increasing, decreasing, or not changing
# in the four response variables (TN, TP, TN:TP, Chl)
# using random forest analysis

# ==============================================
# gather predictor and response data

# get geo predictor data
# change to LTER data source when posted, for now:

lake.predictors = read.csv("LAGOS_supporting_geophysical.csv", header = TRUE)
head(lake.predictors)

# source data created from running '05_analysis_hlm'
change.db.all = read.table("lmer_change_db.txt")

# merge geo data and responses
lake.predictors = merge(lake.predictors, change.db.all[,c(1,14:17)], all.x = TRUE)

# ==========================================
# Random forest analysis
# Creates 3 models: 1) all predictors 'rf.run.cat', 2) only lake-level
# predictors 'rf.run.cat.lake', 3) only region-level predictors 'rf.run.cat.region'

rf.run.cat <- function(response, dat) {
  
  # dat = the dataframe with predictor and response data
  # response is either tn, tp, tntp, or chl
  
  # filter data frame to only response and predictors
  vars.keep = grep(paste("^",response, "_change", sep = ""), names(dat))

  dat.keep = dat[,c(6:45, vars.keep)]

  dat.keep = dat.keep[complete.cases(dat.keep), ]

  pred.vars = names(dat.keep)[names(dat.keep) != paste(response, "_change", sep = "")]
  response.var = paste(response, "_change", sep = "")

  rf.formula <- as.formula(paste(paste(response.var, "~"), paste(pred.vars, collapse= "+")))

  n.min = min(summary(dat.keep[,length(dat.keep)]))
  rf<-randomForest(rf.formula,data=dat.keep,na.action=na.omit,importance=1, ntree=2000,
                   strata = dat.keep[,length(dat.keep)],
                   sampsize = rep(n.min, 3)) # gives min group size for weighting
  return(rf)
}
rf.run.cat.lake <- function(response, dat) {

  #filter data frame to only response and predictors

  vars.keep = grep(paste("^",response, "_change", sep = ""), names(dat))

  dat.keep = dat[,c(6:20, vars.keep)]

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

  dat.keep = dat[,c(21:45, vars.keep)]

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
## set rf formula and dat.keep inside function rf.run.cat

rf.vsurf  <- function(response, dat) {
  
  # dat = the dataframe with predictor and response data
  # response is either tn, tp, tntp, or chl
  
  # filter data frame to only response and predictors
  vars.keep = grep(paste("^",response, "_change", sep = ""), names(dat))
  
  dat.keep = dat[,c(6:45, vars.keep)]
  
  dat.keep = dat.keep[complete.cases(dat.keep), ]
  
  pred.vars = names(dat.keep)[names(dat.keep) != paste(response, "_change", sep = "")]
  response.var = paste(response, "_change", sep = "")
  
  rf.formula <- as.formula(paste(paste(response.var, "~"), paste(pred.vars, collapse= "+")))
  
  n.min = min(summary(dat.keep[,length(dat.keep)]))
  rf.VSURF<-VSURF(rf.formula,data=dat.keep,na.action=na.omit,
                   strata = dat.keep[,length(dat.keep)],
                   sampsize = rep(n.min, 3)) # gives min group size for weighting
  rf.VSURF$var.names = pred.vars
  return(rf.VSURF)
}
tn.test = rf.vsurf("tn", lake.predictors)
tp.test = rf.vsurf("tp", lake.predictors)
chl.test = rf.vsurf("chl", lake.predictors)
tntp.test = rf.vsurf("tntp", lake.predictors)


# rerun RF models using variables selected

rf.rerun.cat <- function(vsurf.test, dat) {

  # filter data frame to only response 
  # and predictors left after interpretation step of VSURF
  
  vars.keep = vsurf.test$var.names[vsurf.test$varselect.pred]
  response = as.character(vsurf.test$terms[[2]])
  dat.keep = dat[,c(response, vars.keep)]

  dat.keep = dat.keep[complete.cases(dat.keep), ]

  
  rf.formula <- as.formula(paste(paste(response, "~"), paste(vars.keep, collapse= "+")))

  n.min = min(summary(dat.keep[,1]))
  rf<-randomForest(rf.formula,data=dat.keep,na.action=na.omit,importance=1, ntree=5000,
                   strata = dat.keep[,1],
                   sampsize = rep(n.min, 3))
  return(rf)
}

tn.rerun = rf.rerun.cat(tn.test, lake.predictors)
tp.rerun = rf.rerun.cat(tp.test, lake.predictors)
tntp.rerun = rf.rerun.cat(tntp.test, lake.predictors)
chl.rerun = rf.rerun.cat(chl.test, lake.predictors)


# put together a dataframe with top vars

tn.imp = data.frame(Response_Variable = "tn",
                    Predictor_Variable = row.names(varImpPlot(tn.rerun)),
                    Per_inc_MSE = (as.numeric(varImpPlot(tn.rerun)[,1])/max(as.numeric(varImpPlot(tn.rerun)[,1]))))

tn.imp = tn.imp[order(tn.imp$Per_inc_MSE, decreasing = TRUE)[1:5], ]

tp.imp = data.frame(Response_Variable = "tp",
                    Predictor_Variable = row.names(varImpPlot(tp.rerun)),
                    Per_inc_MSE = (as.numeric(varImpPlot(tp.rerun)[,1])/max(as.numeric(varImpPlot(tp.rerun)[,1]))))
tp.imp = tp.imp[order(tp.imp$Per_inc_MSE, decreasing = TRUE)[1:5], ]

tntp.imp = data.frame(Response_Variable = "tntp",
                      Predictor_Variable = row.names(varImpPlot(tntp.rerun)),
                      Per_inc_MSE = (as.numeric(varImpPlot(tntp.rerun)[,1])/max(as.numeric(varImpPlot(tntp.rerun)[,1]))))
tntp.imp = tntp.imp[order(tntp.imp$Per_inc_MSE, decreasing = TRUE)[1:5], ]


chl.imp = data.frame(Response_Variable = "chl",
                     Predictor_Variable = row.names(varImpPlot(chl.rerun)),
                     Per_inc_MSE = (as.numeric(varImpPlot(chl.rerun)[,1])/max(as.numeric(varImpPlot(chl.rerun)[,1]))))
chl.imp = chl.imp[order(chl.imp$Per_inc_MSE, decreasing = TRUE)[1:5], ]

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
