# load required packages
# 
library(lme4)
library(MuMIn)
library(arm)
library(effects)
require(maps)

# set data you want to work with

data = modern.e5
data.name = "modern.e5"
data.short.name = "e5"

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

## create a histogram of slopes for the change in TP and TN
pdf(file=paste("TN_TP_change_", data.name, ".pdf", sep=""))
hist(blup.tn$slopes*100, breaks = 20, col=rgb(.2,.5,.5,.5), 
     ylim = c(0,35), xlim = c(-5, 4),main = "", xlab = "% Change per year", 
     ylab = "Number of lakes")
hist(blup.tp$slopes*100, breaks = 20, col=rgb(.5,.2,.2,0.5), add = TRUE)
legend(2, 25, c(paste("TN mean\n =",round(as.numeric(fixef(tn.model)[2])*100,3),"\n"), paste("TP mean\n = ", round(as.numeric(fixef(tp.model)[2])*100,3))), fill= c(rgb(.2,.5,.5,.5), rgb(.5,.2,.2,0.5)))
dev.off()

## create a plot of TN ~ TP slopes, where color is dependent on whether 
## TN (green), TP (red), or TN & TP (brown) are different from zero
pdf(paste(data.short.name,"_TN_TP_change.pdf", sep=""))
temp.tn = 100*blup.tn$slopes
temp.tp = 100*blup.tp$slopes
plot(temp.tn~temp.tp, 
     xlab = "% Change in TP per year", 
     ylab = "% Change in TN per year", 
     cex = 1.5, cex.lab = 1.2)
abline(0,1)
abline(h=0, col = rgb(.2,.5,.5), lwd = 3)
abline(v=0, col = rgb(.5,.2,.5), lwd = 3)
text(max(temp.tp)*.75, min(temp.tn)*.75, labels = paste(length(temp.tp), "lakes"))

#first plot points where TN is different from zero
points(temp.tn[tn.diff.zero==TRUE]~temp.tp[tn.diff.zero==TRUE], 
       col = rgb(.2,.5,.5, .5), pch=16, cex = 1.5)
points(temp.tn[tp.diff.zero==TRUE]~temp.tp[tp.diff.zero==TRUE], 
       col = rgb(.5,.2,.5, .5), pch=16, cex=1.5)
points(temp.tn[tn.diff.zero==TRUE & tp.diff.zero==TRUE]~temp.tp[tn.diff.zero==TRUE& tp.diff.zero==TRUE], 
       col = rgb(.2,.2,.2,.5), pch=16, cex = 1.5)
dev.off()

# create a map with locations that plots where lakes are changing

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

pdf(paste(data.short.name,"_TN_directional_change.pdf", sep=""))
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

pdf(paste(data.short.name, "_TP_directional_change.pdf", sep=""))
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

library(merTools)

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

#plot only the random slopes
model = list(tn.model, tp.model, tntp.model, secchi.model)
model.names = c("tn", "tp", "tntp", "secchi")
for (i in 1:4) {
  randoms = REsim(model[[i]], n.sims=500)
  test = plotREsim(randoms) 
  p = plotREsim(randoms[randoms$term == "sampleyear_cor",])
  p = p + labs(title = paste("Effect Ranges ",data.short.name)) 
  p = p + annotate("text", x = 100, y=-0.05, label = paste(length(which(test$data$sig[213:424] == TRUE)),"/",length(test$data$sig)/2, "lakes with slopes \n different from zero"))
  
  pdf(file=paste(data.short.name,"_",model.names[i], "_blups_resim.pdf", sep=""))
  print(p)
  dev.off()
}


