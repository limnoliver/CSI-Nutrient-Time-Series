# load required packages
# 
library(lme4)
library(MuMIn)
library(arm)
library(effects)
require(maps)
library(merTools)
library(ggplot2)

# set data you want to work with

data = modern.15
data.name = "modern.10"
data.short.name = "15y"

## run models with standardized data to make slopes directly comparable
tn.model = lmer(log(tn_umol) ~ sampleyear_cor + (sampleyear_cor|lagoslakeid), data = data, REML=FALSE)
tp.model = lmer(log(tp_umol) ~ sampleyear_cor + (sampleyear_cor|lagoslakeid), data = data, REML=FALSE)
tntp.model = lmer(log(tn_tp_umol) ~ sampleyear_cor + (sampleyear_cor|lagoslakeid), data = data, REML=FALSE)
secchi.model = lmer(log(secchi) ~ sampleyear_cor + (sampleyear_cor|lagoslakeid), data = data, REML=FALSE)

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

# simulate random effects with posterior distributions
# Uses the Gelman sim technique to built Bayes estimates

tn.randoms = plotREsim(REsim(tn.model, n.sims=500))
tp.randoms = plotREsim(REsim(tp.model, n.sims=500))
tntp.randoms = plotREsim(REsim(tntp.model, n.sims=500))
secchi.randoms = plotREsim(REsim(secchi.model, n.sims=500))



#get TRUE/FALSE out of whether each point is significantly different
#from zero based on above simulations

tn.diff.zero = data.frame(lagoslakeid = tn.randoms$data$groupID[tn.randoms$data$term=="sampleyear_cor"], 
                          sig = tn.randoms$data$sig[tn.randoms$data$term=="sampleyear_cor"])
tp.diff.zero = data.frame(lagoslakeid = tp.randoms$data$groupID[tp.randoms$data$term=="sampleyear_cor"], 
                          sig =  tp.randoms$data$sig[tp.randoms$data$term=="sampleyear_cor"])
tntp.diff.zero = data.frame(lagoslakeid = tntp.randoms$data$groupID[tntp.randoms$data$term=="sampleyear_cor"], 
                          sig = tntp.randoms$data$sig[tntp.randoms$data$term=="sampleyear_cor"])
secchi.diff.zero = data.frame(lagoslakeid = secchi.randoms$data$groupID[secchi.randoms$data$term=="sampleyear_cor"], 
                          sig = secchi.randoms$data$sig[secchi.randoms$data$term=="sampleyear_cor"])

#combine BLUP output with simulated different from zero estimates above
blup.tn = merge(blup.tn, tn.diff.zero, by = "lagoslakeid", all.x = TRUE )
blup.tp = merge(blup.tp, tp.diff.zero, by = "lagoslakeid", all.x = TRUE )
blup.tntp = merge(blup.tntp, tntp.diff.zero, by = "lagoslakeid", all.x = TRUE )
blup.secchi = merge(blup.secchi, secchi.diff.zero, by = "lagoslakeid", all.x = TRUE )

#####################
## Figures ##
#####################

## set wd
setwd("C:/Users/Samantha/Dropbox/CSI_LIMNO_Manuscripts-presentations/CSI_Nitrogen MSs/Time series/Output")
setwd("~/Dropbox/CSI_LIMNO_Manuscripts-presentations/CSI_Nitrogen MSs/Time series/Output")


## create a histogram of slopes for the change in TP and TN
hist.output = hist(blup.tn$slopes*100, breaks = 20)

## some code to fix legend boxes in R
source("http://www.math.mcmaster.ca/bolker/R/misc/legendx.R")

pdf(file=paste(data.short.name, "_TN_TP_change_hist.pdf", sep=""))
par(mar=c(5,5,1,1))
hist(blup.tn$slopes*100, breaks = 20, col=rgb(.2,.5,.5,.5), 
     main = "", xlab = "% Change per year",
     ylim = c(0,max(hist.output$counts)*1.3),
     xlim = c(min(hist.output$breaks)*1.5, max(hist.output$breaks)*1.5),
     ylab = "Number of lakes", 
     cex.lab = 2,
     cex.axis = 1.5)
hist(blup.tp$slopes*100, breaks = 20, col=rgb(.5,.2,.2,0.5), add = TRUE)
legend(min(hist.output$breaks)*1.4, max(hist.output$counts*1.1), 
       c(paste("TN mean\n =",round(as.numeric(fixef(tn.model)[2])*100,3)), 
         paste("TP mean\n = ", round(as.numeric(fixef(tp.model)[2])*100,3))), 
       fill= c(rgb(.2,.5,.5,.5), rgb(.5,.2,.2,0.5)), 
       bty = "n", pt.lwd = 6, box.cex = c(1.2,1.2), y.intersp=2, 
       cex = 1.5)
dev.off()

## create a plot of TN ~ TP slopes, where color is dependent on whether 
## TN (green), TP (red), or TN & TP (brown) are different from zero

pdf(paste(data.short.name,"_TN_TP_change.pdf", sep=""))
par(mar=c(5,5,1,1))
temp.tn = 100*blup.tn$slopes
temp.tp = 100*blup.tp$slopes
plot(temp.tn~temp.tp, 
     xlab = "% Change in TP per year", 
     ylab = "% Change in TN per year", 
     cex = 1.5, cex.lab = 2, cex.axis = 1.5)
abline(0,1)
abline(h=0, col = rgb(.2,.5,.5), lwd = 3)
abline(v=0, col = rgb(.5,.2,.5), lwd = 3)
text(max(temp.tp)*.75, min(temp.tn)*.75, 
     labels = paste(length(temp.tp), "lakes"), cex = 1.5)

#first plot points where TN is different from zero
points(temp.tn[blup.tn$sig==TRUE]~temp.tp[blup.tn$sig==TRUE], 
       col = rgb(.2,.5,.5, .5), pch=16, cex = 1.5)

points(temp.tn[blup.tp$sig==TRUE]~temp.tp[blup.tp$sig==TRUE], 
       col = rgb(.5,.2,.5, .5), pch=16, cex=1.5)
points(temp.tn[blup.tn$sig==TRUE & blup.tp$sig==TRUE]~temp.tp[blup.tn$sig==TRUE& blup.tp$sig==TRUE], 
       col = rgb(.2,.2,.2,.5), pch=16, cex = 1.5)
dev.off()

# plot secchi changes vs nutrient changes
# first have to merge the two slopes

tn.secchi = merge(blup.secchi, blup.tn, by = "lagoslakeid", all.x=TRUE)
tn.secchi[,c(3,6)] = 100*tn.secchi[,c(3,6)]
tn.secchi.model = lm(tn.secchi$slopes.x~tn.secchi$slopes.y)

pdf(paste(data.short.name,"_Change_Secchi_TN.pdf", sep=""))
plot(tn.secchi$slopes.x ~ tn.secchi$slopes.y,
     xlab = "% Change in TN", ylab = "% Change in Secchi", cex.lab=1.5)
dev.off()

tp.secchi = merge(blup.secchi, blup.tp, by = "lagoslakeid", all.x=TRUE)
tp.secchi[,c(3,6)] = 100*tp.secchi[,c(3,6)]
tp.secchi.model= lm(tp.secchi$slopes.x~tp.secchi$slopes.y)

pdf(paste(data.short.name,"_Change_Secchi_TP.pdf", sep=""))
plot(tp.secchi$slopes.x ~ tp.secchi$slopes.y,  
     xlab = "% Change in TP", ylab = "% Change in Secchi", cex.lab=1.5)
dev.off()

# Create a map that plots change by location
locations = data.lake.specific[,c(1,3,4)]

blup.tn = merge(blup.tn, locations, by = "lagoslakeid", all.x=TRUE)
blup.tp = merge(blup.tp, locations, by = "lagoslakeid", all.x=TRUE)

pdf(paste(data.short.name,"_Change_TN_TP_location.pdf", sep=""))
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE,col = "gray")
points(blup.tn$nhd_long[blup.tp$sig==FALSE & blup.tn$sig==FALSE], blup.tn$nhd_lat[blup.tp$sig==FALSE & blup.tn$sig==FALSE], cex = 1.2)

points(blup.tn$nhd_long[blup.tn$sig==TRUE], blup.tn$nhd_lat[blup.tn$sig==TRUE],
       col = rgb(.2,.5,.5, .5), pch=16, cex = 1.2)
points(blup.tp$nhd_long[blup.tp$sig==TRUE], blup.tp$nhd_lat[blup.tp$sig==TRUE], 
       col = rgb(.5,.2,.5, .5), pch=16, cex = 1.2)
points(blup.tn$nhd_long[blup.tn$sig==TRUE & blup.tp$sig==TRUE], blup.tn$nhd_lat[blup.tn$sig==TRUE & blup.tp$sig==TRUE], 
       col = rgb(.2,.2,.2,.5), pch=16, cex = 1.2)
legend(-83, 49, c("TN", "TP", "TN & TP"), fill= c(rgb(.2,.5,.5,.5), rgb(.5,.2,.2,0.5), rgb(.2,.2,.2,.5)))
dev.off()

## Create a map that shows positive/negative change

pdf(paste(data.short.name,"_TN_directional_change.pdf", sep=""))
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE,col = "gray")

#first plot points where TN is different from zero
points(blup.tn$nhd_long[blup.tn$sig==FALSE], blup.tn$nhd_lat[blup.tn$sig==FALSE], cex=1.2)

points(blup.tn$nhd_long[blup.tn$sig==TRUE & blup.tn$slopes>0], blup.tn$nhd_lat[blup.tn$sig==TRUE & blup.tn$slopes>0],
       col = rgb(.1,.5,.1, .5), pch=16, cex=1.2)
points(blup.tn$nhd_long[blup.tn$sig==TRUE & blup.tn$slopes<0], blup.tn$nhd_lat[blup.tn$sig==TRUE & blup.tn$slopes<0],
       col = rgb(0,.1,.5, .5), pch=16,cex=1.2)
legend(-83, 49, c("Positive", "Negative"), fill= c(rgb(.1,.5,.1,.5), rgb(0,.1,.5,0.5)))
dev.off()

pdf(paste(data.short.name, "_TP_directional_change.pdf", sep=""))
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE,col = "gray")

#first plot points where tp is different from zero
points(blup.tp$nhd_long[blup.tp$sig==FALSE], 
       blup.tp$nhd_lat[blup.tp$sig==FALSE], cex = 1.2)
points(blup.tp$nhd_long[blup.tp$sig==TRUE & blup.tp$slopes>0], blup.tp$nhd_lat[blup.tp$sig==TRUE & blup.tp$slopes>0],
       col = rgb(.1,.5,.1, .5), pch=16, cex = 1.2)
points(blup.tp$nhd_long[blup.tp$sig==TRUE & blup.tp$slopes<0], blup.tp$nhd_lat[blup.tp$sig==TRUE & blup.tp$slopes<0],
       col = rgb(0,.1,.5, .5), pch=16, cex = 1.2)
legend(-83, 49, c("Positive", "Negative"), fill= c(rgb(.1,.5,.1,.5), rgb(0,.1,.5,0.5)))
dev.off()

#########################################
## create a histogram that shows
## how many years of observation each 
## lake has that made it into the dataset
#########################################

year.obs = data.frame(lagoslakeid = duration$Group.1, nyears = duration$x[,4])
year.obs = year.obs[year.obs$lagoslakeid %in% unique(data$lagoslakeid), ]
hist.out = hist(year.obs$nyears)

pdf(paste(data.short.name, "_nyears_hist.pdf"))
hist(year.obs$nyears,
     main = "", 
     xlab = "Duration of Record (y)", 
     ylab = "Number of lakes")
text(x = median(hist.out$breaks), 
     y = max(hist.out$counts)*.7, 
     labels = paste("Mean = ", round(mean(year.obs$nyears),1), 
                    "\nMedian = ", median(year.obs$nyears),
                    "\nMin = ", min(year.obs$nyears), sep=""))
dev.off()

################
## create map that puts random subset of the 
## significant and nonsignificant relationships on the map
################

#randomly select 5 lakes with significant TN or TP trends
lakes.sig.tn = sample(1:length(which(blup.tn$sig==TRUE)), 6)
lakes.sig.tn = as.character(blup.tn$lagoslakeid[blup.tn$sig == TRUE][lakes.sig.tn])

lakes.sig.tp = sample(1:length(which(blup.tp$sig==TRUE)), 6)
lakes.sig.tp = as.character(blup.tp$lagoslakeid[blup.tp$sig == TRUE][lakes.sig.tp])

#create a map where the plot is next to the dot marking the location of the lake

map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE,col = "gray")

pdf(paste(data.short.name,"_TN_randomchange.pdf",sep=""), height= 4, width =7)
par(mfrow = c(2,3), oma = c(0,0,0,0), mar = c(2,3,2,1))
for (i in 1:length(lakes.sig.tn)){
  plot(data$tn_umol[data$lagoslakeid == lakes.sig.tn[i]]~data$sampleyear_cor[data$lagoslakeid == lakes.sig.tn[i]],
       main = paste(lakes.sig.tn[i]), 
       #xlab = "Year (post 1990)", 
       #ylab = "TN (umol)", 
       #cex.lab = 2,
       ylab = "",
       xlab = "",
       cex.axis = 1.5, 
       cex.pt = 2, 
       cex = 1.8, 
       pch = 16, 
       col =  rgb(.1,.5,.1, .5))
}
dev.off()

pdf(paste(data.short.name,"_TP_randomchange.pdf",sep=""), height= 4, width =7)
par(mfrow = c(2,3), oma = c(0,0,0,0), mar = c(2,3,2,1))
for (i in 1:length(lakes.sig.tp)){
      plot(data$tp_umol[data$lagoslakeid == lakes.sig.tp[i]]~data$sampleyear_cor[data$lagoslakeid == lakes.sig.tp[i]],
       main = paste(lakes.sig.tp[i]), 
       #xlab = "Year (post 1990)", 
       #ylab = "TP (umol)",
       xlab = "", 
       ylab = "",
       cex.lab = 2,
       cex.axis = 1.5, 
       cex.pt = 2, 
       cex = 1.8, 
       pch = 16, 
       col = rgb(0,.1,.5, .5))
}
dev.off()

#plot only the random slopes
model = list(tn.model, tp.model, tntp.model, secchi.model)
blups = list(blup.tn, blup.tp, blup.tntp, blup.secchi)
model.names = c("TN", "TP", "TNTP", "Secchi")
for (i in 1:4) {
  randoms = REsim(model[[i]], n.sims=500)
  p = plotREsim(randoms[randoms$term == "sampleyear_cor",]) + 
        theme(axis.text=element_text(size=16),
              axis.title=element_text(size=20),
              strip.text.x = element_text(size = 16),
              strip.text.y = element_text(size = 16), 
              title = element_text(size=16))
  p = p + labs(title = paste(model.names[i]," Effect Ranges ","(",data.short.name,")",sep=""), size = 12) 
  p = p + annotate("text", x = .5*length(blups[[i]]$sig), y=-0.05, size = 4,
                   label = paste(length(which(blups[[i]]$sig == TRUE)),"/",length(blups[[i]]$sig), "lakes with slopes \n different from zero"))
  
  pdf(file=paste(data.short.name,"_",model.names[i], "_blups_resim.pdf", sep=""), 
      height=3, width=5)
  print(p)
  dev.off()
}


#####################
## Miscellaneous code ##
#####################

## create a dotplot that examines which BLUPs are different from zero
blup.ext <- ranef(tn.model, condVar=TRUE, whichel = "lagoslakeid")
print(blup.ext)
dotplot(blup.ext)

#different ways to plot blups with errors
#using dotplot
library(lattice)
blup.ext <- ranef(tn.model, condVar=TRUE, whichel = "lagoslakeid")
dotplot(blup.ext)

# using qqmath
qqmath(ranef(tn.model, condVar = TRUE), strip = FALSE)$lagoslakeid

# using ggplot

ggCaterpillar <- function(re, QQ=TRUE, likeDotplot=TRUE) {
  require(ggplot2)
  f <- function(x) {
    pv   <- attr(x, "postVar")
    cols <- 1:(dim(pv)[1])
    se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
    pDf  <- data.frame(y=unlist(x)[ord],
                       ci=1.96*se[ord],
                       nQQ=rep(qnorm(ppoints(nrow(x))), ncol(x)),
                       ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]),
                       ind=gl(ncol(x), nrow(x), labels=names(x)))
    
    if(QQ) {  ## normal QQ-plot
      p <- ggplot(pDf, aes(nQQ, y))
      p <- p + facet_wrap(~ ind, scales="free")
      p <- p + xlab("Standard normal quantiles") + ylab("Random effect quantiles")
    } else {  ## caterpillar dotplot
      p <- ggplot(pDf, aes(ID, y)) + coord_flip()
      if(likeDotplot) {  ## imitate dotplot() -> same scales for random effects
        p <- p + facet_wrap(~ ind)
      } else {           ## different scales for random effects
        p <- p + facet_grid(ind ~ ., scales="free_y")
      }
      p <- p + xlab("Levels") + ylab("Random effects")
    }
    
    p <- p + theme(legend.position="none")
    p <- p + geom_hline(yintercept=0)
    p <- p + geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=0, colour="black")
    p <- p + geom_point(aes(size=1.2), colour="blue") 
    return(p)
  }
  
  lapply(re, f)
}

ggCaterpillar(ranef(tn.model,condVar=TRUE))
ggCaterpillar(ranef(tn.model, condVar=TRUE), QQ=FALSE)
ggCaterpillar(ranef(tn.model, condVar=TRUE), QQ=FALSE, likeDotplot=FALSE)

#using merTools and REsim

randoms = REsim(tn.model, n.sims=500)
head(randoms)
plotREsim(randoms)
plotREsim(tn.model)

#get TRUE/FALSE out of whether each point is significantly different
#from zero or not

#intercepts
summary(test$data$sig[1:212])
#slopes
summary(test$data$sig[213:424])

