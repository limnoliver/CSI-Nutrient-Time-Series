require(ggplot2)
library(RColorBrewer)
library(maps)

install.packages("mapproj")
library(mapproj)

# ============================
# Setup

# import data - source from LTER wesbite. For now, use:
source("06_analysis_rf.R")
# will use change.db.all + lake.predictors

setwd("C:/Users/Samantha/Dropbox/CSI_LIMNO_Manuscripts-presentations/CSI_Nitrogen MSs/Time series/Data")
lake.predictors = read.csv("LAGOS_supporting_geophysical.csv", header = TRUE)
change.db.all = merge(change.db.all, lake.predictors[,c(1,4,5)], by = "lagoslakeid", all.x = TRUE)

# set colors for tn and tp
col.tn = rgb(.94,.73,0.035,.7)
col.tp = rgb(.07,.57,.45,0.7)
col.both = rgb(68/255, 36/255,0, 0.7)
col.no.change = "darkgray"

#change to match the number of lakes in each database
n.tn = 833
n.tp = 2096
n.tntp = 742
n.chl = 2239

#####################################################
## create a figure that plots %TN change vs %TP change
#####################################################

temp = change.db.all[!is.na(change.db.all$tn_coef_mean) & !is.na(change.db.all$tp_coef_mean),]
temp.tn = 100*temp$tn_coef_mean
temp.tp = 100*temp$tp_coef_mean

pdf("TN_TP_change.pdf")
par(mar=c(5,5,1,1))
#tn positive, tp no change
plot(temp.tn[temp$tn_ymin > 0 & temp$tp_ymin < 0 & temp$tp_ymax > 0]~temp.tp[temp$tn_ymin > 0 & temp$tp_ymin < 0 & temp$tp_ymax > 0], 
     xlab = "% Change in TP per year", 
     ylab = "% Change in TN per year", 
     cex = 1.5, cex.lab = 2, cex.axis = 1.5,  col = "black", pch=24, bg = col.tn,
     xlim = c(-6,6), ylim = c(-6, 3))
# Tn no change, TP no change
points(temp.tn[temp$tn_ymin < 0 & temp$tn_ymax >0 & temp$tp_ymin < 0 & temp$tp_ymax > 0]~temp.tp[temp$tn_ymin < 0 & temp$tn_ymax >0 & temp$tp_ymin < 0 & temp$tp_ymax > 0],
       pch=1, cex = 1.5)
#TN up, TP up
points(temp.tn[temp$tn_ymin > 0 & temp$tp_ymin > 0]~temp.tp[temp$tn_ymin > 0 & temp$tp_ymin > 0],
       col = "black", bg = col.both, pch=19, cex = 1.5)

#TN down, TP down
points(temp.tn[temp$tn_ymax < 0 & temp$tp_ymax < 0]~temp.tp[temp$tn_ymax < 0 & temp$tp_ymax < 0],
       col = "black", bg = col.both, pch=19, cex = 1.5)

#TN down, TP no change
points(temp.tn[temp$tn_ymax < 0 & temp$tp_ymin < 0 & temp$tp_ymax > 0]~temp.tp[temp$tn_ymax < 0 & temp$tp_ymin < 0 & temp$tp_ymax > 0],
       col = "black", bg = col.tn, pch=25, cex = 1.5)

#TN no change, TP down
points(temp.tn[temp$tn_ymin < 0 & temp$tn_ymax >0 & temp$tp_ymax<0]~temp.tp[temp$tn_ymin < 0 & temp$tn_ymax >0 & temp$tp_ymax<0],
       col = "black", bg = col.tp, pch=25, cex = 1.5)

#TN no change, TP up
points(temp.tn[temp$tn_ymin < 0 & temp$tn_ymax >0 & temp$tp_ymin>0]~temp.tp[temp$tn_ymin < 0 & temp$tn_ymax >0 & temp$tp_ymin>0],
       col = "black", bg = col.tp, pch=24, cex = 1.5)

#TN down, TP up
points(temp.tn[temp$tn_ymax < 0 & temp$tp_ymin > 0]~temp.tp[temp$tn_ymax < 0 & temp$tp_ymin > 0],
       col = "black", bg = col.both,  pch=19, cex = 1.5)


legend("bottomright",
       c("No Change", "Both Changing", "TN Increasing",  "TP Increasing","TN Decreasing", "TP Decreasing"), 
       pch = c(21,21,24,24,25,25), 
       pt.bg = c("white", col.both, col.tn, col.tp, col.tn, col.tp), 
       pt.cex = 1.5)

abline(0,1)
abline(h=0, col = col.tn, lwd = 3)
abline(v=0, col = col.tp, lwd = 3)

dev.off()

# =================================================
# Figure 1
# code that plots 8 maps: for each nutrient or chlorophyll response, 
# plot the regional trend estimates and the lake-specific trend estimates.
require(maps)
require(maptools)
require(rgdal)
require(sp)
require(ggplot2)
library(colorspace)

setwd("C:/Users/Samantha/Dropbox/GEO-Shared2/MGD_Shapefile_Exports_to_map_in_R_May2014")
huc4 = readOGR(dsn ="/Users/Samantha/Dropbox/GEO-Shared2/MGD_Shapefile_Exports_to_map_in_R_May2014", layer = "HU4_simple_wgs1984")
huc4 = readOGR(dsn = "/Users/skoliver/Dropbox/GEO-Shared2/MGD_Shapefile_Exports_to_map_in_R_May2014", layer = "HU4_simple_wgs1984")

#function to extract regional random effects and add significance column for plotting
map.data <- function(region.RE)  {
  dat <- region.RE[[2]][region.RE[[2]]$term == "sampleyear_cor",c("hu4_zoneid", "coef_mean", "ymin", "ymax")]
  names(dat)[1] = "ZoneID"
  # add column that say whether the change value is different from zero
  for (i in 1:nrow(dat)){
    if (dat$ymin[i] > 0|dat$ymax[i]<0) {
      dat$sig[i] = TRUE
    } else {
      dat$sig[i] = FALSE
    }
  }
  return(dat)
}

temp.tn = map.data(change.db.tn)
temp.tp = map.data(change.db.tp)
temp.tntp = map.data(change.db.tntp)
temp.chl = map.data(change.db.chl)

#merge nutrient values with shapefile
huc4 = merge(huc4, temp.tn[,c(1,2,5)], by = "ZoneID", all.x = TRUE)
names(huc4)[c(25,26)] = c("tn_slope", "tn_sig")
huc4 = merge(huc4, temp.tp[,c(1,2,5)], by = "ZoneID", all.x = TRUE)
names(huc4)[c(27,28)] = c("tp_slope", "tp_sig")
huc4 = merge(huc4, temp.tntp[,c(1,2,5)], by = "ZoneID", all.x = TRUE)
names(huc4)[c(29,30)] = c("tntp_slope", "tntp_sig")
huc4 = merge(huc4, temp.chl[,c(1,2,5)], by = "ZoneID", all.x = TRUE)
names(huc4)[c(31,32)] = c("chl_slope", "chl_sig")

colors =  c(rgb(5,48,97,max=255),
            rgb(33,102,172,max=255),
            rgb(67,147,195,max=255),
            rgb(146,197,222,max=255),
            rgb(209,229,240,max=255),
            rgb(253,219,199,max=255),
            rgb(244,165,130,max=255),
            rgb(214,96,77,max=255),
            rgb(178,24,43,max=255),
            rgb(103,0,31,max=255))

get.col.bins <- function(slopes, alpha=255) {
z=100*slopes

ii <- cut(z, breaks = c(-15,-4,-3,-2,-1,0,1,2,3,4,15), 
          include.lowest = TRUE)

levels(ii) <- c(rgb(5,48,97,max=255, alpha=alpha),
                rgb(33,102,172,max=255, alpha=alpha),
                rgb(67,147,195,max=255, alpha=alpha),
                rgb(146,197,222,max=255, alpha=alpha),
                rgb(209,229,240,max=255, alpha=alpha),
                rgb(253,219,199,max=255, alpha=alpha),
                rgb(244,165,130,max=255, alpha=alpha),
                rgb(214,96,77,max=255, alpha=alpha),
                rgb(178,24,43,max=255, alpha=alpha),
                rgb(103,0,31,max=255, alpha=alpha))
ii = as.character(ii)
ii[is.na(ii)==TRUE] <- rgb(255,255,255,max=255)
return(ii)
}

pdf("blup_legend.pdf")
par(mar = c(0,0,0,0))
plot.new()
points(x = seq(from = 0.1, to = 0.9, by = (.8/9)), y= rep(.5,10), pch = 15, cex = 7, col = colors)
text(x = seq(from = .1, to = .9, by = (.8/9)), y = rep(.43,10), labels = c("-5","-4", "-3", "-2","-1","1","2","3","4","5"), cex = 2)
text(x=.5, y = .59, labels = "% Change Per Year", cex = 2.2)
dev.off()

pdf("all_blups.pdf")

# add regional trend estimates, where regions 
# with significant trend are bolded on map
# colors represent decreasing (blue) or increasing (red) nutrients

par(mfcol=c(4,2), cex = 1)
par(mar = c(0,0,0,0))
plot(huc4,col=get.col.bins(huc4$tn_slope), lty = 1, lwd=1,border=TRUE,mar=c(0,0,3,1),oma=c(0,0,0,0))
plot(huc4[which(huc4$tn_sig==TRUE),], lty = 1, lwd=3,border=TRUE, add = TRUE)

plot(huc4,col=get.col.bins(huc4$tp_slope), lty = 1, lwd=1,border=TRUE,mar=c(1,1,1,1),oma=c(0,0,0,0))
plot(huc4[which(huc4$tp_sig==TRUE),], lty = 1, lwd=3,border=TRUE, add = TRUE)

plot(huc4,col=get.col.bins(huc4$tntp_slope), lty = 1, lwd=1,border=TRUE,mar=c(1,1,1,1),oma=c(0,0,0,0))
plot(huc4[which(huc4$tntp_sig==TRUE),], lty = 1, lwd=3,border=TRUE,  add = TRUE)

plot(huc4,col=get.col.bins(huc4$chl_slope), lty = 1, lwd=1,border=TRUE,mar=c(1,1,1,1))
plot(huc4[which(huc4$chl_sig==TRUE),], lty = 1, lwd=3,border=TRUE,  add = TRUE)

# plot lake-specific trends

#function to extract lake BLUPS
lake.map.data <- function(lake.RE)  {
  dat <- lake.RE[[1]][lake.RE[[1]]$term == "sampleyear_cor",c("lagoslakeid", "coef_mean", "ymin", "ymax")]

  # add column that say whether the change value is different from zero
  for (i in 1:nrow(dat)){
    if (dat$ymin[i] > 0|dat$ymax[i]<0) {
      dat$sig[i] = TRUE
    } else {
      dat$sig[i] = FALSE
    }
  }
  dat = merge(dat, lake.predictors[,c("lagoslakeid", "nhd_lat", "nhd_long")], by = "lagoslakeid", all.x = TRUE)
  return(dat)
}

temp.tn = lake.map.data(change.db.tn)
temp.tp = lake.map.data(change.db.tp)
temp.tntp = lake.map.data(change.db.tntp)
temp.chl = lake.map.data(change.db.chl)

#tn plot
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                 "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                 "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                 "Vermont", "New Hampshire","Maine"), fill = FALSE, lwd=1,mar=c(0,0,1,0),oma=c(0,0,0,0))
points(temp.tn$nhd_long[which(temp.tn$sig==FALSE)], temp.tn$nhd_lat[which(temp.tn$sig==FALSE)], col = get.col.bins(temp.tn$coef_mean[which(temp.tn$sig==FALSE)], 200), pch = 16)
points(temp.tn$nhd_long[which(temp.tn$sig==TRUE)], temp.tn$nhd_lat[which(temp.tn$sig==TRUE)], 
       bg = get.col.bins(temp.tn$coef_mean[which(temp.tn$sig==TRUE)], 200), 
       pch = 21, col = "black" ,lwd = 1)
#tp plot
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE, lwd=1,mar=c(0,0,1,1),oma=c(0,0,0,0))
points(temp.tp$nhd_long[which(temp.tp$sig==FALSE)], temp.tp$nhd_lat[which(temp.tp$sig==FALSE)], col = get.col.bins(temp.tp$coef_mean[which(temp.tp$sig==FALSE)], 200), pch = 16)
points(temp.tp$nhd_long[which(temp.tp$sig==TRUE)], temp.tp$nhd_lat[which(temp.tp$sig==TRUE)], 
       bg = get.col.bins(temp.tp$coef_mean[which(temp.tp$sig==TRUE)], 200), 
       pch = 21, col = "black" ,lwd = 1)
#tntp plot
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE, lwd=1,mar=c(0,0,1,1),oma=c(0,0,0,0))
points(temp.tntp$nhd_long[which(temp.tntp$sig==FALSE)], temp.tntp$nhd_lat[which(temp.tntp$sig==FALSE)], col = get.col.bins(temp.tntp$coef_mean[which(temp.tntp$sig==FALSE)], 200), pch = 16)
points(temp.tntp$nhd_long[which(temp.tntp$sig==TRUE)], temp.tntp$nhd_lat[which(temp.tntp$sig==TRUE)], 
       bg = get.col.bins(temp.tntp$coef_mean[which(temp.tntp$sig==TRUE)], 200), 
       pch = 21, col = "black" ,lwd = 1)

#chl plot
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE, lwd=1,mar=c(0,0,1,1),oma=c(0,0,0,0))
points(temp.chl$nhd_long[which(temp.chl$sig==FALSE)], temp.chl$nhd_lat[which(temp.chl$sig==FALSE)], col = get.col.bins(temp.chl$coef_mean[which(temp.chl$sig==FALSE)], 200), pch = 16)
points(temp.chl$nhd_long[which(temp.chl$sig==TRUE)], temp.chl$nhd_lat[which(temp.chl$sig==TRUE)], 
       bg = get.col.bins(temp.chl$coef_mean[which(temp.chl$sig==TRUE)], 200), 
       pch = 21, col = "black" ,lwd = 1)

dev.off()

# ====================================================
# Figure 2
# create a figure that plots %TN change vs %TP change

temp = change.db.all[!is.na(change.db.all$tn_coef_mean) & !is.na(change.db.all$tp_coef_mean),]
temp.tn = 100*temp$tn_coef_mean
temp.tp = 100*temp$tp_coef_mean

pdf("TN_TP_change.pdf")
par(mar=c(5,5,1,1))
#tn positive, tp no change
plot(temp.tn[temp$tn_ymin > 0 & temp$tp_ymin < 0 & temp$tp_ymax > 0]~temp.tp[temp$tn_ymin > 0 & temp$tp_ymin < 0 & temp$tp_ymax > 0], 
     xlab = "% Change in TP per year", 
     ylab = "% Change in TN per year", 
     cex = 1.5, cex.lab = 2, cex.axis = 1.5,  col = "black", pch=24, bg = col.tn,
     xlim = c(-6,6), ylim = c(-6, 3))
# Tn no change, TP no change
points(temp.tn[temp$tn_ymin < 0 & temp$tn_ymax >0 & temp$tp_ymin < 0 & temp$tp_ymax > 0]~temp.tp[temp$tn_ymin < 0 & temp$tn_ymax >0 & temp$tp_ymin < 0 & temp$tp_ymax > 0],
       cex = 1.5, bg = rgb(254,254,254,max=255,alpha=178), pch=21)
#TN up, TP up
points(temp.tn[temp$tn_ymin > 0 & temp$tp_ymin > 0]~temp.tp[temp$tn_ymin > 0 & temp$tp_ymin > 0],
       col = "black", bg = col.both, pch=19, cex = 1.5)

#TN down, TP down
points(temp.tn[temp$tn_ymax < 0 & temp$tp_ymax < 0]~temp.tp[temp$tn_ymax < 0 & temp$tp_ymax < 0],
       col = "black", bg = col.both, pch=19, cex = 1.5)

#TN down, TP no change
points(temp.tn[temp$tn_ymax < 0 & temp$tp_ymin < 0 & temp$tp_ymax > 0]~temp.tp[temp$tn_ymax < 0 & temp$tp_ymin < 0 & temp$tp_ymax > 0],
       col = "black", bg = col.tn, pch=25, cex = 1.5)

#TN no change, TP down
points(temp.tn[temp$tn_ymin < 0 & temp$tn_ymax >0 & temp$tp_ymax<0]~temp.tp[temp$tn_ymin < 0 & temp$tn_ymax >0 & temp$tp_ymax<0],
       col = "black", bg = col.tp, pch=25, cex = 1.5)

#TN no change, TP up
points(temp.tn[temp$tn_ymin < 0 & temp$tn_ymax >0 & temp$tp_ymin>0]~temp.tp[temp$tn_ymin < 0 & temp$tn_ymax >0 & temp$tp_ymin>0],
       col = "black", bg = col.tp, pch=24, cex = 1.5)

#TN down, TP up
points(temp.tn[temp$tn_ymax < 0 & temp$tp_ymin > 0]~temp.tp[temp$tn_ymax < 0 & temp$tp_ymin > 0],
       col = "black", bg = col.both,  pch=19, cex = 1.5)


legend("bottomright",
       c("No Change", "Both Changing", "TN Increasing",  "TP Increasing","TN Decreasing", "TP Decreasing"), 
       pch = c(21,21,24,24,25,25), 
       pt.bg = c("white", col.both, col.tn, col.tp, col.tn, col.tp), 
       pt.cex = 1.5)

abline(0,1)
abline(h=0, col = col.tn, lwd = 3)
abline(v=0, col = col.tp, lwd = 3)

dev.off()


########################
# filling in summary tables
##########################

## extract model coefficients
change.db.tntp[[4]][2,2] - (qnorm(.975)*change.db.tntp[[4]][2,4])
change.db.tntp[[4]][2,2] + (qnorm(.975)*change.db.tntp[[4]][2,4])



# ==========================================
# Figure 4
# create violin plots of important predictors identified by the rf

quant.low = function(x){
  quantile(x,.25)
}
quant.high = function(x){
  quantile(x,.75)
}

pdf("tntp_top_var2.pdf")
#TP vs ppt change
dat.keep = lake.predictors[!is.na(lake.predictors$tp_change), ]
ggplot(dat.keep, aes(x=tp_change, y=hu4_ppt_change, fill=tp_change)) + 
  geom_violin()+
  theme_classic()+
  stat_summary(fun.ymin = "quant.low", 
               fun.y = "median",
               fun.ymax = "quant.high",
               geom="pointrange", shape = 3) +
  scale_fill_manual(values = c(rgb(67,147,195,max=255),rgb(214,96,77,max=255), "gray"))+ 
  labs(x="TP Change", y="Regional Precipitation Change")+
  guides(fill=FALSE)+
  theme(axis.title=element_text(size = rel(2))) +
  theme(axis.text=element_text(size = rel(1.6))) +
  theme(axis.text.y=element_text(margin=margin(2,2,3,6,"pt")))+
  theme(axis.text.x=element_text(margin=margin(2,2,4,3,"pt")))
dev.off()

#TN vs regional TN dep 1990
dat.keep = lake.predictors[!is.na(lake.predictors$tn_change), ]
ggplot(dat.keep, aes(x=tn_change, y=hu4_dep_totaln_1990_mean, fill=tn_change)) + 
  geom_violin()+
  theme_classic()+
  stat_summary(fun.ymin = "quant.low", 
               fun.y = "median",
               fun.ymax = "quant.high",
               geom="pointrange", shape = 3) +
  scale_fill_manual(values = c(rgb(67,147,195,max=255),rgb(214,96,77,max=255), "gray"))+ 
  labs(x="TN Change", y="Regional TN Deposition 1990")+
  guides(fill=FALSE)+
  theme(axis.title=element_text(size = rel(2))) +
  theme(axis.text=element_text(size = rel(1.6))) +
  theme(axis.text.y=element_text(margin=margin(2,2,3,6,"pt")))+
  theme(axis.text.x=element_text(margin=margin(2,2,4,3,"pt")))

#TN:TP vs regional dep change
dat.keep = lake.predictors[!is.na(lake.predictors$tntp_change), ]
ggplot(dat.keep, aes(x=tntp_change, y=hu4_dep_change, fill=tntp_change)) + 
  geom_violin()+
  theme_classic()+
  stat_summary(fun.ymin = "quant.low", 
               fun.y = "median",
               fun.ymax = "quant.high",
               geom="pointrange", shape = 3) +
  scale_fill_manual(values = c(rgb(67,147,195,max=255),rgb(214,96,77,max=255), "gray"))+ 
  labs(x="TN:TP Change", y="Regional Deposition Change")+
  guides(fill=FALSE)+
  theme(axis.title=element_text(size = rel(2))) +
  theme(axis.text=element_text(size = rel(1.6))) +
  theme(axis.text.y=element_text(margin=margin(2,2,3,6,"pt")))+
  theme(axis.text.x=element_text(margin=margin(2,2,4,3,"pt")))

#Chl vs Regional mean temp in 2011
dat.keep = lake.predictors[!is.na(lake.predictors$chl_change), ]
ggplot(dat.keep, aes(x=chl_change, y=hu4_tmean_1990/100, fill=chl_change)) + 
  geom_violin()+
  theme_classic()+
  stat_summary(fun.ymin = "quant.low", 
               fun.y = "median",
               fun.ymax = "quant.high",
               geom="pointrange", shape = 3) +
  scale_fill_manual(values = c(rgb(67,147,195,max=255),rgb(214,96,77,max=255), "gray"))+ 
  labs(x="Chl Change", y="Regional Mean Temp. in 1990")+
  guides(fill=FALSE)+
  theme(axis.title=element_text(size = rel(1.5))) +
  theme(axis.text=element_text(size = rel(1.2))) +
  theme(axis.text.y=element_text(margin=margin(2,2,3,6,"pt")))+
  theme(axis.text.x=element_text(margin=margin(2,2,4,3,"pt")))
dev.off()


###################################
###################################
pdf("response_vs_modelvals.pdf")
par(mfcol = c(4,3), oma = c(4,3,0,0), mar = c(2,1,2,1))
plot(tn_coef_mean~log(tn_meanval), data = lake.predictors, pch = 21, bg = rgb(130,130,130,max=255,alpha=150))
abline(h=0, col = "red")
x = log(lake.predictors$tn_meanval[!is.na(lake.predictors$tn_meanval)])
y = lake.predictors$tn_coef_mean[!is.na(lake.predictors$tn_coef_mean)]
test = lowess(x= x, y=y)
lines(lowess(x,y), col = "yellow")
plot(tp_coef_mean~log(tp_meanval), data = lake.predictors, pch = 21, bg = rgb(130,130,130,max=255,alpha=150))
abline(h=0, col = "red")
plot(tntp_coef_mean~log(tntp_meanval), data = lake.predictors, pch = 21, bg = rgb(130,130,130,max=255,alpha=150))
abline(h=0, col = "red")
plot(chl_coef_mean~log(chl_meanval), data = lake.predictors, pch = 21, bg = rgb(130,130,130,max=255,alpha=150))
abline(h=0, col = "red")


plot(tn_coef_mean~tn_medyear, data = lake.predictors, pch = 21, bg = rgb(130,130,130,max=255,alpha=100), yaxt = "n")
abline(h=0, col = "red")
axis(side=2,labels=F) 
plot(tp_coef_mean~tp_medyear, data = lake.predictors, pch = 21, bg = rgb(130,130,130,max=255,alpha=100), yaxt = "n")
abline(h=0, col = "red")
axis(side=2,labels=F) 
plot(tntp_coef_mean~tntp_medyear, data = lake.predictors, pch = 21, bg = rgb(130,130,130,max=255,alpha=100), yaxt = "n")
abline(h=0, col = "red")
axis(side=2,labels=F) 
plot(chl_coef_mean~chl_medyear, data = lake.predictors, pch = 21, bg = rgb(130,130,130,max=255,alpha=100), yaxt = "n")
abline(h=0, col = "red")
axis(side=2,labels=F) 

plot(tn_coef_mean~tn_nyear, data = lake.predictors, pch = 21, bg = rgb(130,130,130,max=255,alpha=100), yaxt = "n")
abline(h=0, col = "red")
axis(side=2,labels=F) 
plot(tp_coef_mean~tp_nyear, data = lake.predictors, pch = 21, bg = rgb(130,130,130,max=255,alpha=100), yaxt = "n")
abline(h=0, col = "red")
axis(side=2,labels=F) 
plot(tntp_coef_mean~tntp_nyear, data = lake.predictors, pch = 21, bg = rgb(130,130,130,max=255,alpha=100), yaxt = "n")
abline(h=0, col = "red")
axis(side=2,labels=F) 
plot(chl_coef_mean~chl_nyear, data = lake.predictors, pch = 21, bg = rgb(130,130,130,max=255,alpha=100), yaxt = "n")
abline(h=0, col = "red")
axis(side=2,labels=F) 
mtext("TN Change", side=2, line = 1, adj=0.91, outer = TRUE)
mtext("TP Change", side=2, line = 1, adj=0.64, outer = TRUE)
mtext("TN:TP Change", side=2, line = 1, adj=0.35, outer = TRUE)
mtext("Chl Change", side=2, line = 1, adj=0.07, outer = TRUE)

mtext("log Mean Value", side = 1, line = 0, adj=.14, outer = TRUE, padj = 1)
mtext("Median Obs. Year", side = 1, line = 0, adj=.52, outer = TRUE, padj = 1)
mtext("N Years", side = 1, line = 0, adj=.88, outer = TRUE, padj = 1)
dev.off()
