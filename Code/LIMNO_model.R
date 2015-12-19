# load required packages
library(lme4)
library(MuMIn)
library(arm)
library(effects)
require(maps)

## run models with standardized data to make slopes directly comparable
tn.model.stand = lmer(tn_umol_stand ~ sampleyear_cor + (sampleyear_cor|lagoslakeid), data = modern.15, REML=FALSE)
tn.model.stand.int = lmer(tn_umol_stand ~ sampleyear_cor + (1|lagoslakeid), data = modern.15, REML=FALSE)
tp.model.stand = lmer(tp_umol_stand ~ sampleyear_cor + (sampleyear_cor|lagoslakeid), data = modern.15, REML=FALSE)
tntp.model.stand = lmer(tn_tp_umol_stand ~ sampleyear_cor + (sampleyear_cor|lagoslakeid), data = modern.15, REML=FALSE)
secchi.model.stand = lmer(secchi_stand ~ sampleyear_cor + (sampleyear_cor|lagoslakeid), data = modern.15, REML=FALSE)

tn.model.stand.uncorr = lmer(tn_umol_stand ~ sampleyear_cor + (1|lagoslakeid) + (0+sampleyear_cor|lagoslakeid), data = modern.15, REML=FALSE)

## extract random coefficients

blup.tn.stand = coef(tn.model.stand)
blup.tp.stand = coef(tp.model.stand)
blup.tntp.stand = coef(tntp.model.stand)
blup.secchi.stand = coef(secchi.model.stand)

## turn random coefficient list into a dataframe
## also express BLUPs as a percent by multiplying by 100

blup.tn.stand = data.frame(intercepts = blup.tn.stand$lagoslakeid[[1]], 
                           slopes = 100*blup.tn.stand$lagoslakeid[[2]],
                           lagoslakeid = row.names(blup.tn.stand$lagoslakeid))
blup.tp.stand = data.frame(intercepts = blup.tp.stand$lagoslakeid[[1]], 
                           slopes = 100*blup.tp.stand$lagoslakeid[[2]],
                           lagoslakeid = row.names(blup.tp.stand$lagoslakeid))
blup.tntp.stand = data.frame(intercepts = blup.tntp.stand$lagoslakeid[[1]], 
                           slopes = 100*blup.tntp.stand$lagoslakeid[[2]],
                           lagoslakeid = row.names(blup.tntp.stand$lagoslakeid))
blup.secchi.stand = data.frame(intercepts = blup.secchi.stand$lagoslakeid[[1]], 
                           slopes = 100*blup.secchi.stand$lagoslakeid[[2]],
                           lagoslakeid = row.names(blup.secchi.stand$lagoslakeid))



## create a histogram of slopes for the change in TP and TN
pdf("hist_TN_TP_change.pdf")
hist(blup.tn.stand$slopes, breaks = 20, col=rgb(.2,.5,.5,.5), 
     ylim = c(0,25), main = "", xlab = "% Change per year", ylab = "Number of lakes")
hist(blup.tp.stand$slopes, breaks = 20, col=rgb(.5,.2,.2,0.5), add = TRUE)
legend(0.03, 20, c("TN", "TP"), fill= c(rgb(.2,.5,.5,.5), rgb(.5,.2,.2,0.5)))
dev.off()

## identify lakes that have slopes different than zero
## by using 1.96*SE of each BLUP

# set blup equal to which model you want to validate
blup = blup.secchi.stand
# extract SE from random effects
test = ranef(secchi.model.stand, condVar=TRUE)

#extract blup SE using the se.ranef function
#change model name to which model you are evaluating

model = secchi.model.stand
blup.se = se.ranef(model)
blup.se = data.frame(intercepts = blup.se$lagoslakeid[,1], 
                     slopes = 100*blup.se$lagoslakeid[,2], 
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
plot(blup.tn.stand$lagoslakeid[,2]~blup.tp.stand$lagoslakeid[,2], 
     xlab = "% Change in TP per year", 
     ylab = "% Change in TN per year", 
     cex = 1.5, cex.lab = 1.2)
abline(0,1)
abline(h=0, col = rgb(.2,.5,.5), lwd = 3)
abline(v=0, col = rgb(.5,.2,.5), lwd = 3)

#first plot points where TN is different from zero
points(blup.tn.stand$lagoslakeid[,2][tn.diff.zero==TRUE]~blup.tp.stand$lagoslakeid[,2][tn.diff.zero==TRUE], 
       col = rgb(.2,.5,.5, .5), pch=16, cex = 1.5)
points(blup.tn.stand$lagoslakeid[,2][tp.diff.zero==TRUE]~blup.tp.stand$lagoslakeid[,2][tp.diff.zero==TRUE], 
       col = rgb(.5,.2,.5, .5), pch=16, cex=1.5)
points(blup.tn.stand$lagoslakeid[,2][tn.diff.zero==TRUE & tp.diff.zero==TRUE]~blup.tp.stand$lagoslakeid[,2][tn.diff.zero==TRUE& tp.diff.zero==TRUE], 
       col = rgb(.2,.2,.2,.5), pch=16, cex = 1.5)


dev.off()

# create a plot of TN slopes ~ Secchi slopes and TP slopes ~ Secchi slopes
# merge data frames

secchi.test.tn = merge(blup.tn.stand, blup.secchi.stand, by = "lagoslakeid", all.x = TRUE )
secchi.test.tp = merge(blup.tp.stand, blup.secchi.stand, by = "lagoslakeid", all.x = TRUE )

pdf("Change_Secchi_TN.pdf")
# plot secchi slopes on the y axis and nitrogen slopes on x axis
plot(slopes.y ~ slopes.x, data = secchi.test.tn, xlab = "% Change in TN", ylab = "% Change in Secchi")
dev.off()

pdf("Change_Secchi_TP.pdf")
# plot secchi slopes on the y axis and phosphorus slopes on x axis
plot(slopes.y ~ slopes.x, data = secchi.test.tp, xlab = "% Change in TP", ylab = "% Change in Secchi")
dev.off()

# Create a map that plots change by location
locations = data.lake.specific[,c(1,3,4)]
tn.blups = merge(blup.tn.stand, locations, by = "lagoslakeid", all.x=TRUE)
tp.blups = merge(blup.tp.stand, locations, by = "lagoslakeid", all.x=TRUE)

pdf("Change_TN_TP_location.pdf")
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE,col = "gray")

#first plot points where TN is different from zero
points(tn.blups$nhd_long[tn.diff.zero==TRUE], tn.blups$nhd_lat[tn.diff.zero==TRUE],
       col = rgb(.2,.5,.5, .5), pch=16)
points(tp.blups$nhd_long[tp.diff.zero==TRUE], tp.blups$nhd_lat[tp.diff.zero==TRUE], 
       col = rgb(.5,.2,.5, .5), pch=16)
points(tn.blups$nhd_long[tn.diff.zero==TRUE & tp.diff.zero==TRUE], tn.blups$nhd_lat[tn.diff.zero==TRUE & tp.diff.zero==TRUE], 
       col = rgb(.2,.2,.2,.5), pch=16)
points(tn.blups$nhd_long[tp.diff.zero==FALSE & tn.diff.zero==FALSE], tn.blups$nhd_lat[tp.diff.zero==FALSE & tn.diff.zero==FALSE])
legend(-83, 49, c("TN", "TP", "TN & TP"), fill= c(rgb(.2,.5,.5,.5), rgb(.5,.2,.2,0.5), rgb(.2,.2,.2,.5)))
dev.off()

## Create a map that shows positive/negative change

pdf("TN_directional_change.pdf")
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE,col = "gray")

#first plot points where TN is different from zero
points(tn.blups$nhd_long[tn.diff.zero==TRUE & tn.blups$slopes>0], tn.blups$nhd_lat[tn.diff.zero==TRUE & tn.blups$slopes>0],
       col = rgb(.1,.5,.1, .5), pch=16)
points(tn.blups$nhd_long[tn.diff.zero==TRUE & tn.blups$slopes<0], tn.blups$nhd_lat[tn.diff.zero==TRUE & tn.blups$slopes<0],
       col = rgb(0,.1,.5, .5), pch=16)
points(tn.blups$nhd_long[tn.diff.zero==FALSE], tn.blups$nhd_lat[tn.diff.zero==FALSE])
legend(-83, 49, c("Positive", "Negative"), fill= c(rgb(.1,.5,.1,.5), rgb(0,.1,.5,0.5)))
dev.off()

pdf("TP_directional_change.pdf")
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE,col = "gray")

#first plot points where tp is different from zero
points(tp.blups$nhd_long[tp.diff.zero==TRUE & tp.blups$slopes>0], tp.blups$nhd_lat[tp.diff.zero==TRUE & tp.blups$slopes>0],
       col = rgb(.1,.5,.1, .5), pch=16)
points(tp.blups$nhd_long[tp.diff.zero==TRUE & tp.blups$slopes<0], tp.blups$nhd_lat[tp.diff.zero==TRUE & tp.blups$slopes<0],
       col = rgb(0,.1,.5, .5), pch=16)
points(tp.blups$nhd_long[tp.diff.zero==FALSE], tp.blups$nhd_lat[tp.diff.zero==FALSE])
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
  model = lm(tn_umol_stand[lagoslakeid==lakes[i]]~sampleyear_cor[lagoslakeid==lakes[i]], data = modern.15)
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




