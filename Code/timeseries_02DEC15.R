# load required packages
# 
library(lme4)
library(MuMIn)
library(arm)
library(effects)
require(maps)

## run models with standardized data to make slopes directly comparable
tn.model = lmer(log(tn_umol) ~ sampleyear_cor + (sampleyear_cor|lagoslakeid), data = modern.15, REML=FALSE)
tn.model.int = lmer(log(tn_umol) ~ sampleyear_cor + (1|lagoslakeid), data = modern.15, REML=FALSE)
tp.model = lmer(log(tp_umol) ~ sampleyear_cor + (sampleyear_cor|lagoslakeid), data = modern.15, REML=FALSE)
tntp.model = lmer(log(tn_tp_umol) ~ sampleyear_cor + (sampleyear_cor|lagoslakeid), data = modern.15, REML=FALSE)
secchi.model = lmer(log(secchi) ~ sampleyear_cor + (sampleyear_cor|lagoslakeid), data = modern.15, REML=FALSE)

## create models for expanded dataset
lakes.5 = lakes$lagoslakeid[lakes$keep.5 == TRUE]
modern.e5 = modern[modern$lagoslakeid %in% lakes.5,]
lakes.7 = lakes$lagoslakeid[lakes$keep.7 == TRUE]
modern.e7 = modern[modern$lagoslakeid %in% lakes.7,]
lakes.10 = lakes$lagoslakeid[lakes$keep.10 == TRUE]
modern.e10 = modern[modern$lagoslakeid %in% lakes.10,]

tn.model.5 = lmer(log(tn_umol) ~ sampleyear_cor + (sampleyear_cor|lagoslakeid), data = modern.e5, REML=FALSE)
tp.model.5 = lmer(log(tp_umol) ~ sampleyear_cor + (sampleyear_cor|lagoslakeid), data = modern.e5, REML=FALSE)

## extract random coefficients

blup.tn = coef(tn.model)
blup.tp = coef(tp.model)
blup.tntp = coef(tntp.model)
blup.secchi = coef(secchi.model)

## turn random coefficient list into a dataframe
## also express BLUPs as a percent by multiplying by 100

#calculate starting concnetrations for each lake


blup.tn = data.frame(intercepts = blup.tn$lagoslakeid[[1]], 
                     slopes = blup.tn$lagoslakeid[[2]],
                     lagoslakeid = row.names(blup.tn$lagoslakeid))
blup.tp = data.frame(intercepts = blup.tp$lagoslakeid[[1]], 
                     slopes = blup.tp$lagoslakeid[[2]],
                     lagoslakeid = row.names(blup.tp$lagoslakeid))
blup.tntp = data.frame(intercepts = blup.tntp$lagoslakeid[[1]], 
                     slopes = blup.tntp$lagoslakeid[[2]],
                     lagoslakeid = row.names(blup.tntp$lagoslakeid))
blup.secchi = data.frame(intercepts = blup.secchi$lagoslakeid[[1]], 
                     slopes = blup.secchi$lagoslakeid[[2]],
                     lagoslakeid = row.names(blup.secchi$lagoslakeid))



## create a histogram of slopes for the change in TP and TN
pdf("hist_TN_TP_change.pdf")
hist(blup.tn$slopes*100, breaks = 20, col=rgb(.2,.5,.5,.5), 
     ylim = c(0,35), xlim = c(-5, 4),main = "", xlab = "% Change per year", 
     ylab = "Number of lakes")
hist(blup.tp$slopes*100, breaks = 30, col=rgb(.5,.2,.2,0.5), add = TRUE)
legend(2, 25, c("TN", "TP"), fill= c(rgb(.2,.5,.5,.5), rgb(.5,.2,.2,0.5)))
dev.off()

## identify lakes that have slopes different than zero
## by using 1.96*SE of each BLUP

# set blup equal to which model you want to validate
blup = blup.tp

#extract blup SE using the se.ranef function
#change model name to which model you are evaluating
model = tp.model
blup.se = se.ranef(model)
blup.se = data.frame(intercepts = blup.se$lagoslakeid[,1], 
                     slopes = blup.se$lagoslakeid[,2], 
                     lagoslakeid = row.names(blup.se$lagoslakeid))
blup.low = list()
blup.high = list()
n.diff.mean = list()
n.diff.zero = list()
prop.non.overlap = c()
prop.diff.zero = c()
for (i in 1:2) {
  blup.low[[i]] = as.numeric(blup[,i] - (1.96*blup.se[,i]))
  blup.high[[i]] = as.numeric(blup[,i] + (1.96*blup.se[,i]))
  grand.low = as.numeric(fixef(model)[i])-(1.96*as.numeric(se.fixef(model)[i]))
  grand.high = as.numeric(fixef(model)[i])+(1.96*as.numeric(se.fixef(model)[i]))
  n.diff.mean[[i]] =  (blup.low[[i]]>=grand.low&blup.low[[i]]<=grand.high)|(blup.high[[i]]>=grand.low&blup.high[[i]]<=grand.high)|(blup.low[[i]]<=grand.low&blup.high[[i]]>=grand.high)
  n.diff.zero[[i]]  = blup.low[[i]]>0|blup.high[[i]]<0
  prop.non.overlap[i]=length(which(n.diff.mean[[i]]==FALSE))/length(n.diff.mean[[i]])
  prop.diff.zero[i] = length(which(n.diff.zero[[i]]==TRUE))/length(n.diff.zero[[i]])
}

## save a list identifying which slopes are different from zero
## from both the TN and TP models

tn.diff.zero = n.diff.zero[[2]]
tp.diff.zero = n.diff.zero[[2]]
secchi.diff.zero = n.diff.zero[[2]]

## create a plot of TN ~ TP slopes, where color is dependent on whether 
## TN (green), TP (red), or TN & TP (brown) are different from zero
pdf("TN_TP_change.pdf")
temp.tn = 100*blup.tn$slopes
temp.tp = 100*blup.tp$slopes
plot(temp.tn~temp.tp, 
     xlab = "% Change in TP per year", 
     ylab = "% Change in TN per year", 
     cex = 1.5, cex.lab = 1.2)
abline(0,1)
abline(h=0, col = rgb(.2,.5,.5), lwd = 3)
abline(v=0, col = rgb(.5,.2,.5), lwd = 3)

#first plot points where TN is different from zero
points(temp.tn[tn.diff.zero==TRUE]~temp.tp[tn.diff.zero==TRUE], 
       col = rgb(.2,.5,.5, .5), pch=16, cex = 1.5)
points(temp.tn[tp.diff.zero==TRUE]~temp.tp[tp.diff.zero==TRUE], 
       col = rgb(.5,.2,.5, .5), pch=16, cex=1.5)
points(temp.tn[tn.diff.zero==TRUE & tp.diff.zero==TRUE]~temp.tp[tn.diff.zero==TRUE& tp.diff.zero==TRUE], 
       col = rgb(.2,.2,.2,.5), pch=16, cex = 1.5)
dev.off()

# create a plot of TN slopes ~ Secchi slopes and TP slopes ~ Secchi slopes
# merge data frames

secchi.test.tn = merge(blup.tn, blup.secchi, by = "lagoslakeid", all.x = TRUE )
secchi.test.tp = merge(blup.tp, blup.secchi, by = "lagoslakeid", all.x = TRUE )
secchi.test.tn$slopes.x = 100*secchi.test.tn$slopes.x
secchi.test.tn$slopes.y = 100*secchi.test.tn$slopes.y
secchi.test.tp$slopes.x = 100*secchi.test.tp$slopes.x
secchi.test.tp$slopes.y = 100*secchi.test.tp$slopes.y

png("Change_Secchi_TN.png")
# plot secchi slopes on the y axis and nitrogen slopes on x axis
plot(slopes.y ~ slopes.x, data = secchi.test.tn, 
     xlab = "% Change in TN", ylab = "% Change in Secchi", cex.lab=1.5)
dev.off()

png("Change_Secchi_TP.png")
# plot secchi slopes on the y axis and phosphorus slopes on x axis
plot(slopes.y ~ slopes.x, data = secchi.test.tp, 
     xlab = "% Change in TP", ylab = "% Change in Secchi", cex.lab=1.5)
dev.off()

# Create a map that plots change by location
locations = data.lake.specific[,c(1,3,4)]
blup.tn$diff.zero = tn.diff.zero
blup.tp$diff.zero = tp.diff.zero
tn.blups = merge(blup.tn, locations, by = "lagoslakeid", all.x=TRUE)
tp.blups = merge(blup.tp, locations, by = "lagoslakeid", all.x=TRUE)

pdf("Change_TN_TP_location.pdf")
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE,col = "gray")

points(tn.blups$nhd_long[tn.blups$diff.zero==TRUE], tn.blups$nhd_lat[tn.blups$diff.zero==TRUE],
       col = rgb(.2,.5,.5, .5), pch=16, cex = 1.2)
points(tp.blups$nhd_long[tp.blups$diff.zero==TRUE], tp.blups$nhd_lat[tp.blups$diff.zero==TRUE], 
       col = rgb(.5,.2,.5, .5), pch=16, cex = 1.2)
points(tn.blups$nhd_long[tn.blups$diff.zero==TRUE & tp.blups$diff.zero==TRUE], tn.blups$nhd_lat[tn.blups$diff.zero==TRUE & tp.blups$diff.zero==TRUE], 
       col = rgb(.2,.2,.2,.5), pch=16, cex = 1.2)
points(tn.blups$nhd_long[tp.blups$diff.zero==FALSE & tn.blups$diff.zero==FALSE], tn.blups$nhd_lat[tp.blups$diff.zero==FALSE & tn.blups$diff.zero==FALSE], cex = 1.2)
legend(-83, 49, c("TN", "TP", "TN & TP"), fill= c(rgb(.2,.5,.5,.5), rgb(.5,.2,.2,0.5), rgb(.2,.2,.2,.5)))
dev.off()

## Create a map that shows positive/negative change

pdf("TN_directional_change.pdf")
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE,col = "gray")

#first plot points where TN is different from zero
points(tn.blups$nhd_long[tn.blups$diff.zero==FALSE], tn.blups$nhd_lat[tn.blups$diff.zero==FALSE], cex=1.2)

points(tn.blups$nhd_long[tn.blups$diff.zero==TRUE & tn.blups$slopes>0], tn.blups$nhd_lat[tn.blups$diff.zero==TRUE & tn.blups$slopes>0],
       col = rgb(.1,.5,.1, .5), pch=16, cex=1.2)
points(tn.blups$nhd_long[tn.blups$diff.zero==TRUE & tn.blups$slopes<0], tn.blups$nhd_lat[tn.blups$diff.zero==TRUE & tn.blups$slopes<0],
       col = rgb(0,.1,.5, .5), pch=16,cex=1.2)
legend(-83, 49, c("Positive", "Negative"), fill= c(rgb(.1,.5,.1,.5), rgb(0,.1,.5,0.5)))
dev.off()

pdf("TP_directional_change.pdf")
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE,col = "gray")

#first plot points where tp is different from zero
points(tp.blups$nhd_long[tp.blups$diff.zero==FALSE], 
       tp.blups$nhd_lat[tp.blups$diff.zero==FALSE], cex = 1.2)
points(tp.blups$nhd_long[tp.blups$diff.zero==TRUE & tp.blups$slopes>0], tp.blups$nhd_lat[tp.blups$diff.zero==TRUE & tp.blups$slopes>0],
       col = rgb(.1,.5,.1, .5), pch=16, cex = 1.2)
points(tp.blups$nhd_long[tp.blups$diff.zero==TRUE & tp.blups$slopes<0], tp.blups$nhd_lat[tp.blups$diff.zero==TRUE & tp.blups$slopes<0],
       col = rgb(0,.1,.5, .5), pch=16, cex = 1.2)
legend(-83, 49, c("Positive", "Negative"), fill= c(rgb(.1,.5,.1,.5), rgb(0,.1,.5,0.5)))
dev.off()
#######################
## Model validation
#######################

## create individual linear models to validate mixed model estimates
lakes = unique(modern.15$lagoslakeid)
intercept = c()
slope = c()
p.val = c()
for (i in 1:length(unique(modern.15$lagoslakeid))) {
  model = lm(tn_umol[lagoslakeid==lakes[i]]~sampleyear_cor[lagoslakeid==lakes[i]], data = modern.15)
  intercept[i] = summary(model)$coefficients[1, 1]
  slope[i] = summary(model)$coefficients[2, 1]
  p.val[i] = summary(model)$coefficients[2, 4]
}

## from the simple linear models, 27/90 of TN models have significant change
## this number does not change when you use standardized data 

## check whether the lm outputs are related to the lmer outputs

tn.lm = data.frame(lagoslakeid = lakes, lm.intercepts = intercept, lm.slopes = slope)
lmer.vs.lm = merge(tn.lm, tn.blups, by = "lagoslakeid")

#slopes
plot(slopes~lm.slopes, data = lmer.vs.lm)
hist(lmer.vs.lm$slopes, col = rgb(.5, .2, .2, .5), breaks = 20)
hist(lmer.vs.lm$lm.slopes, col = rgb(.2,.2,.2, .5), breaks = 20, add = TRUE)

#intercepts
plot(intercepts~lm.intercepts, data=lmer.vs.lm)
abline(0,1)

#look at how slopes and intercepts are related in mixed and linear models
plot(lm.slopes~lm.intercepts, data=lmer.vs.lm)
plot(slopes~intercepts, data=lmer.vs.lm)

## ---------------------------------------------------
## Evaluate R2 of the mixed model
## conditional R2 = 
## marginal R2 = 

r.squaredGLMM(tn.model.stand)


## ---------------------------------------------------
## Test H0: is the variance of the random effect == 0?
## uses package RLRsim to test random effects

install.packages("RLRsim")
library(RLRsim)

# mA is the full model
mA = lmer(tp_umol_stand~sampleyear_cor + (1|lagoslakeid) + (0+sampleyear_cor|lagoslakeid), data = modern.15)
# m0 is the full model minus the effect being tested
m0 = update(mA, . ~ . - (0+sampleyear_cor|lagoslakeid))
# m.slope is the model with only the effect being tested
m.slope = update(mA, . ~ . - (1|lagoslakeid))
# note that the exctRLRT function must list models in specific order
test = exactRLRT(m.slope, mA, m0)



###sarah made a change to this file
