require(ggplot2)
library(RColorBrewer)
library(maps)
require(akima)
require(rgdal)

# ===============================================
# PURPOSE:
# this code recreates the figures from Oliver et al. (submitted)
# relies on results of hierarchical linear models and random forest analysis
# ================================================
# Data import

# change.db.all is output from hlm analysis

change.db.all = read.table("lmer_change_db.txt", header = TRUE)

# import data - source from LTER wesbite where data are published:
# for more info, see https://portal.lternet.edu/nis/mapbrowse?scope=knb-lter-ntl&identifier=333&revision=3

# import nutrient values
infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/333/3/e955b964c44276632edd9f3629022077" 
infile1 <- sub("^https","http",infile1) 
all.nut <- read.csv(infile1,header=F, skip=1, sep=",", 
                          col.names=c("lagoslakeid",     
                                      "sampleyear",     
                                      "value",     
                                      "variable"), check.names=TRUE)

# import geo variables

infile2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/333/3/cc35382bc48fc688750d0c358167f3e1" 
infile2 <- sub("^https","http",infile2) 
lake.predictors <- read.csv(infile2, header=F, skip=1, sep=",", 
                            col.names=c("lagoslakeid",     
                                        "hu4_zoneid",     
                                        "iws_zoneid",     
                                        "nhd_lat",     
                                        "nhd_long",     
                                        "lake_area_ha",     
                                        "maxdepth",     
                                        "iws_areaha",     
                                        "lakeconnectivity",     
                                        "iws_slope_mean",     
                                        "iws_urban",     
                                        "iws_crop",     
                                        "iws_pasture",     
                                        "iws_forest",     
                                        "iws_lakes_lakes4ha_overlapping_area_pct",     
                                        "iws_lakes_lakes4ha_isolated_overlapping_area_pct",     
                                        "iws_lakes_lakes4ha_drlakestream_overlapping_area_pct",     
                                        "iws_streamdensity_headwaters_density_mperha",     
                                        "iws_streamdensity_midreaches_density_mperha",     
                                        "iws_streamdensity_rivers_density_mperha",     
                                        "hu4_baseflowindex_mean",     
                                        "hu4_runoff_mean",     
                                        "hu4_dep_totaln_1990_mean",     
                                        "hu4_dep_totaln_2010_mean",     
                                        "hu4_dep_change",     
                                        "hu4_prism_ppt_30yr_normal_800mm2_annual_mean",     
                                        "hu4_prism_tmean_30yr_normal_800mm2_annual_mean",     
                                        "hu4_tmean_1990",     
                                        "hu4_tmean_2011",     
                                        "hu4_tmean_change",     
                                        "hu4_ppt_1990",     
                                        "hu4_ppt_2011",     
                                        "hu4_ppt_change",     
                                        "hu4_damdensity_pointspersqkm",     
                                        "hu4_roaddensity_density_mperha",     
                                        "hu4_slope_mean",     
                                        "hu4_urban",     
                                        "hu4_crop",     
                                        "hu4_pasture",     
                                        "hu4_forest",     
                                        "hu4_lakes_lakes4ha_avgsize_ha",     
                                        "hu4_lakes_lakes4ha_overlapping_area_pct",     
                                        "hu4_lakes_lakes4ha_isolated_overlapping_area_pct",     
                                        "hu4_lakes_lakes4ha_drlakestream_overlapping_area_pct",     
                                        "hu4_latewisconsinglaciation_glaciation"), check.names=TRUE)

# add categorical (increasing, decreasing, no change) trend data to
# to lake.predictors

temp = change.db.all[,c(1,14:17)]
lake.predictors = merge(lake.predictors, temp, by = "lagoslakeid", all.x = TRUE)

# get huc4 polygons from github, which was used as the regional spatial extent
# may not work on macs - link to data:
# https://github.com/limnoliver/CSI-Nutrient-Time-Series/tree/72c8269902e53c7ec6a2cfbe13a0239d13062dc8/Data

load(url("https://github.com/limnoliver/CSI-Nutrient-Time-Series/blob/72c8269902e53c7ec6a2cfbe13a0239d13062dc8/Data/huc4.RData?raw=true"))

# set colors for tn and tp
col.tn = rgb(.94,.73,0.035,.7)
col.tp = rgb(.07,.57,.45,0.7)
col.both = rgb(48/255, 16/255,0, 0.7)
col.no.change = "darkgray"
col.chl = rgb(163,25,255,max=255,alpha=178)

#change to match the number of lakes in each database
n.tn = length(unique(change.db.all$lagoslakeid[change.db.all$variable == "tn_umol"]))
n.tp = length(unique(change.db.all$lagoslakeid[change.db.all$variable == "tp_umol"]))
n.tntp = length(unique(change.db.all$lagoslakeid[change.db.all$variable == "tn_tp_umol"]))
n.chl = length(unique(change.db.all$lagoslakeid[change.db.all$variable == "chl_ugL"]))


# =================================================
# Figure 1
# code that plots 8 maps: for each nutrient or chlorophyll response, 
# plot the regional trend estimates and the lake-specific trend estimates.


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

plot(huc4, lty = 1, lwd=1,border=TRUE,mar=c(0,0,3,1),oma=c(0,0,0,0))
plot(huc4[which(huc4$tn_sig==FALSE),],col="lightgray", lty = 1, lwd=1,border=TRUE,mar=c(0,0,3,1),oma=c(0,0,0,0), add = TRUE)
plot(huc4[which(huc4$tn_sig==TRUE),],col=get.col.bins(huc4$tn_slope)[which(huc4$tn_sig==TRUE)], lty = 1, lwd=1,border=TRUE, add = TRUE)
plot(huc4[which(is.na(huc4$tn_sig)),], col = "lightgray", density = 25,lty = 1, lwd=1,border="black", add = TRUE)

plot(huc4, lty = 1, lwd=1,border=TRUE,mar=c(0,0,3,1),oma=c(0,0,0,0))
plot(huc4[which(huc4$tp_sig==FALSE),],col="lightgray", lty = 1, lwd=1,border=TRUE,mar=c(0,0,3,1),oma=c(0,0,0,0), add = TRUE)
plot(huc4[which(huc4$tp_sig==TRUE),], col=get.col.bins(huc4$tp_slope)[which(huc4$tp_sig==TRUE)], lty = 1, lwd=1,border=TRUE, add = TRUE)
plot(huc4[which(is.na(huc4$tp_sig)),], col = "lightgray", density = 25,lty = 1, lwd=1,border="black", add = TRUE)

plot(huc4, lty = 1, lwd=1,border=TRUE,mar=c(0,0,3,1),oma=c(0,0,0,0))
plot(huc4[which(huc4$tntp_sig==FALSE),],col="lightgray", lty = 1, lwd=1,border=TRUE,mar=c(0,0,3,1),oma=c(0,0,0,0), add = TRUE)
plot(huc4[which(huc4$tntp_sig==TRUE),], col=get.col.bins(huc4$tntp_slope)[which(huc4$tntp_sig==TRUE)],lty = 1, lwd=1,border=TRUE,  add = TRUE)
plot(huc4[which(is.na(huc4$tntp_sig)),], col = "lightgray", density = 25,lty = 1, lwd=1,border="black", add = TRUE)

plot(huc4, lty = 1, lwd=1,border=TRUE,mar=c(0,0,3,1),oma=c(0,0,0,0))
plot(huc4[which(huc4$chl_sig==FALSE),],col="lightgray", lty = 1, lwd=1,border=TRUE,mar=c(0,0,3,1),oma=c(0,0,0,0), add = TRUE)
plot(huc4[which(huc4$chl_sig==TRUE),], col=get.col.bins(huc4$chl_slope)[which(huc4$chl_sig==TRUE)], lty = 1, lwd=1,border=TRUE,  add = TRUE)
plot(huc4[which(is.na(huc4$chl_sig)),], col = "lightgray", density = 25,lty = 1, lwd=1,border="black", add = TRUE)

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
points(temp.tn$nhd_long[which(temp.tn$sig==FALSE)], temp.tn$nhd_lat[which(temp.tn$sig==FALSE)],bg = rgb(220,220,220,max=255,alpha=178), col = rgb(180,180,180,max=255,alpha=178), pch=21)
points(temp.tn$nhd_long[which(temp.tn$sig==TRUE)], temp.tn$nhd_lat[which(temp.tn$sig==TRUE)], 
       bg = get.col.bins(temp.tn$coef_mean[which(temp.tn$sig==TRUE)], 200), 
       pch = 21, col = "black" ,lwd = 1)
#tp plot
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE, lwd=1,mar=c(0,0,1,1),oma=c(0,0,0,0))
points(temp.tp$nhd_long[which(temp.tp$sig==FALSE)], temp.tp$nhd_lat[which(temp.tp$sig==FALSE)],bg = rgb(220,220,220,max=255,alpha=178), col = rgb(180,180,180,max=255,alpha=178), pch=21)
points(temp.tp$nhd_long[which(temp.tp$sig==TRUE)], temp.tp$nhd_lat[which(temp.tp$sig==TRUE)], 
       bg = get.col.bins(temp.tp$coef_mean[which(temp.tp$sig==TRUE)], 200), 
       pch = 21, col = "black" ,lwd = 1)
#tntp plot
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE, lwd=1,mar=c(0,0,1,1),oma=c(0,0,0,0))
points(temp.tntp$nhd_long[which(temp.tntp$sig==FALSE)], temp.tntp$nhd_lat[which(temp.tntp$sig==FALSE)],bg = rgb(220,220,220,max=255,alpha=178), col = rgb(180,180,180,max=255,alpha=178), pch=21)
points(temp.tntp$nhd_long[which(temp.tntp$sig==TRUE)], temp.tntp$nhd_lat[which(temp.tntp$sig==TRUE)], 
       bg = get.col.bins(temp.tntp$coef_mean[which(temp.tntp$sig==TRUE)], 200), 
       pch = 21, col = "black" ,lwd = 1)

#chl plot
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE, lwd=1,mar=c(0,0,1,1),oma=c(0,0,0,0))
points(temp.chl$nhd_long[which(temp.chl$sig==FALSE)], temp.chl$nhd_lat[which(temp.chl$sig==FALSE)],bg = rgb(220,220,220,max=255,alpha=178), col = rgb(180,180,180,max=255,alpha=178), pch=21)
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
       col = "black", bg = col.both, pch=21, cex = 1.5)

#TN down, TP down
points(temp.tn[temp$tn_ymax < 0 & temp$tp_ymax < 0]~temp.tp[temp$tn_ymax < 0 & temp$tp_ymax < 0],
       col = "black", bg = col.both, pch=21, cex = 1.5)

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
       col = "black", bg = col.both,  pch=21, cex = 1.5)


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
# Figure 3
# creates a figure that plots %Chl change vs TN and TP

# filter for lakes that have N and Chl data
temp = change.db.all[!is.na(change.db.all$tn_coef_mean) & !is.na(change.db.all$chl_coef_mean),]
temp.tn = 100*temp$tn_coef_mean
temp.chl = 100*temp$chl_coef_mean

pdf("Chl_TN_change.pdf")
par(mar=c(5,5,1,1))
#tn positive, chl no change
plot(temp.tn[temp$tn_ymin > 0 & temp$chl_ymin < 0 & temp$chl_ymax > 0],temp.chl[temp$tn_ymin > 0 & temp$chl_ymin < 0 & temp$chl_ymax > 0], 
     ylab = "% Change in Chl per year", 
     xlab = "% Change in TN per year", 
     cex = 1.5, cex.lab = 2, cex.axis = 1.5,  col = "black", pch=24, bg = col.tn,
     ylim = c(-10,13), xlim = c(-6, 6))
# Tn no change, chl no change
points(temp.tn[temp$tn_ymin < 0 & temp$tn_ymax >0 & temp$chl_ymin < 0 & temp$chl_ymax > 0],temp.chl[temp$tn_ymin < 0 & temp$tn_ymax >0 & temp$chl_ymin < 0 & temp$chl_ymax > 0],
       cex = 1.5,bg = rgb(254,254,254,max=255,alpha=178), pch=21)

#TN down, Chl no change
points(temp.tn[temp$tn_ymax < 0 & temp$chl_ymin < 0 & temp$chl_ymax > 0],temp.chl[temp$tn_ymax < 0 & temp$chl_ymin < 0 & temp$chl_ymax > 0],
       col = "black", bg = col.tn, pch=25, cex = 1.5)

#TN no change, Chl down
points(temp.tn[temp$tn_ymin < 0 & temp$tn_ymax >0 & temp$chl_ymax<0],temp.chl[temp$tn_ymin < 0 & temp$tn_ymax >0 & temp$chl_ymax<0],
       col = "black", bg = col.chl, pch=25, cex = 1.5)

#TN no change, Chl up
points(temp.tn[temp$tn_ymin < 0 & temp$tn_ymax >0 & temp$chl_ymin>0],temp.chl[temp$tn_ymin < 0 & temp$tn_ymax >0 & temp$chl_ymin>0],
       col = "black", bg = col.chl, pch=24, cex = 1.5)

#TN down, chl up
points(temp.tn[temp$tn_ymax < 0 & temp$chl_ymin > 0],temp.chl[temp$tn_ymax < 0 & temp$chl_ymin > 0],
       col = "black", bg = col.both,  pch=21, cex = 1.5)
#TN down, chl down
points(temp.tn[temp$tn_ymax < 0 & temp$chl_ymax < 0],temp.chl[temp$tn_ymax < 0 & temp$chl_ymax < 0],
       col = "black", bg = col.both, pch=21, cex = 1.5)
#TN up, chl up
points(temp.tn[temp$tn_ymin > 0 & temp$chl_ymin > 0],temp.chl[temp$tn_ymin > 0 & temp$chl_ymin > 0],
       col = "black", bg = col.both, pch=21, cex = 1.5)
#TN up, chl down
points(temp.tn[temp$tn_ymin > 0 & temp$chl_ymax < 0],temp.chl[temp$tn_ymin > 0 & temp$chl_ymax < 0],
       col = "black", bg = col.both, pch=21, cex = 1.5)

#find how many lakes fall into each category

n.no = length(temp.tn[temp$tn_ymin < 0 & temp$tn_ymax >0 & temp$chl_ymin < 0 & temp$chl_ymax > 0])
n.both = length(temp.tn[temp$tn_ymax < 0 & temp$chl_ymin > 0]) +
  length(temp.tn[temp$tn_ymax < 0 & temp$chl_ymax < 0]) +
  length(temp.tn[temp$tn_ymin > 0 & temp$chl_ymin > 0]) + 
  length(temp.tn[temp$tn_ymin > 0 & temp$chl_ymax < 0])
n.tn.inc = length(temp.tn[temp$tn_ymin > 0 & temp$chl_ymin < 0 & temp$chl_ymax > 0])
n.tn.dec = length(temp.tn[temp$tn_ymax < 0 & temp$chl_ymin < 0 & temp$chl_ymax > 0])
n.chl.inc = length(temp.tn[temp$tn_ymin < 0 & temp$tn_ymax >0 & temp$chl_ymin>0])
n.chl.dec = length(temp.tn[temp$tn_ymin < 0 & temp$tn_ymax >0 & temp$chl_ymax<0])



legend("bottomright",
       c("No Change (64%)", "Both Changing (2%)", "TN Increasing (1%)",  "Chl Increasing (21%)","TN Decreasing (9%)", "Chl Decreasing (2%)"), 
       pch = c(21,21,24,24,25,25), 
       pt.bg = c("white", col.both, col.tn, col.chl, col.tn, col.chl), 
       pt.cex = 1.5)

abline(0,1)
abline(h=0, col = col.chl, lwd = 3)
abline(v=0, col = col.tn, lwd = 3)

dev.off()

# now plot chl vs TP

# filter for lakes that have TP and Chl data
pdf("Chl_TP_change.pdf")
par(mar=c(5,5,1,1))
temp = change.db.all[!is.na(change.db.all$tp_coef_mean) & !is.na(change.db.all$chl_coef_mean),]
temp.tp = 100*temp$tp_coef_mean
temp.chl = 100*temp$chl_coef_mean

#tp positive, chl no change
plot(temp.tp[temp$tp_ymin > 0 & temp$chl_ymin < 0 & temp$chl_ymax > 0],temp.chl[temp$tp_ymin > 0 & temp$chl_ymin < 0 & temp$chl_ymax > 0], 
     xlab = "% Change in TP per year", 
     ylab = "% Change in Chl per year", 
     cex = 1.5, cex.lab = 2, cex.axis = 1.5,  col = "black", pch=24, bg = col.tp,
     ylim = c(-10,13), xlim = c(-7, 6))
# tp no change, chl no change
points(temp.tp[temp$tp_ymin < 0 & temp$tp_ymax >0 & temp$chl_ymin < 0 & temp$chl_ymax > 0],temp.chl[temp$tp_ymin < 0 & temp$tp_ymax >0 & temp$chl_ymin < 0 & temp$chl_ymax > 0],
       cex = 1.5,bg = rgb(254,254,254,max=255,alpha=178), pch=21)


#tp down, Chl no change
points(temp.tp[temp$tp_ymax < 0 & temp$chl_ymin < 0 & temp$chl_ymax > 0],temp.chl[temp$tp_ymax < 0 & temp$chl_ymin < 0 & temp$chl_ymax > 0],
       col = "black", bg = col.tp, pch=25, cex = 1.5)

#tp no change, Chl down
points(temp.tp[temp$tp_ymin < 0 & temp$tp_ymax >0 & temp$chl_ymax<0],temp.chl[temp$tp_ymin < 0 & temp$tp_ymax >0 & temp$chl_ymax<0],
       col = "black", bg = col.chl, pch=25, cex = 1.5)

#tp no change, Chl up
points(temp.tp[temp$tp_ymin < 0 & temp$tp_ymax >0 & temp$chl_ymin>0],temp.chl[temp$tp_ymin < 0 & temp$tp_ymax >0 & temp$chl_ymin>0],
       col = "black", bg = col.chl, pch=24, cex = 1.5)

#tp down, chl up
points(temp.tp[temp$tp_ymax < 0 & temp$chl_ymin > 0],temp.chl[temp$tp_ymax < 0 & temp$chl_ymin > 0],
       col = "black", bg = col.both,  pch=21, cex = 1.5)
#tp down, chl down
points(temp.tp[temp$tp_ymax < 0 & temp$chl_ymax < 0],temp.chl[temp$tp_ymax < 0 & temp$chl_ymax < 0],
       col = "black", bg = col.both, pch=21, cex = 1.5)
#tp up, chl up
points(temp.tp[temp$tp_ymin > 0 & temp$chl_ymin > 0],temp.chl[temp$tp_ymin > 0 & temp$chl_ymin > 0],
       col = "black", bg = col.both, pch=21, cex = 1.5)
#tp up, chl down
points(temp.tp[temp$tp_ymin > 0 & temp$chl_ymax < 0],temp.chl[temp$tp_ymin > 0 & temp$chl_ymax < 0],
       col = "black", bg = col.both, pch=21, cex = 1.5)

#find number of lakes in each category
n.no = length(temp.tp[temp$tp_ymin < 0 & temp$tp_ymax >0 & temp$chl_ymin < 0 & temp$chl_ymax > 0])
n.both = length(temp.tp[temp$tp_ymax < 0 & temp$chl_ymin > 0]) +
  length(temp.tp[temp$tp_ymax < 0 & temp$chl_ymax < 0]) +
  length(temp.tp[temp$tp_ymin > 0 & temp$chl_ymin > 0]) + 
  length(temp.tp[temp$tp_ymin > 0 & temp$chl_ymax < 0])
n.tp.inc = length(temp.tp[temp$tp_ymin > 0 & temp$chl_ymin < 0 & temp$chl_ymax > 0])
n.tp.dec = length(temp.tp[temp$tp_ymax < 0 & temp$chl_ymin < 0 & temp$chl_ymax > 0])
n.chl.inc = length(temp.tp[temp$tp_ymin < 0 & temp$tp_ymax >0 & temp$chl_ymin>0])
n.chl.dec = length(temp.tp[temp$tp_ymin < 0 & temp$tp_ymax >0 & temp$chl_ymax<0])

legend("bottomright",
       c("No Change (67%)", "Both Changing (3%)", "TP Increasing (4%)",  "Chl Increasing (12%)","TP Decreasing (10%)", "Chl Decreasing (4%)"), 
       pch = c(21,21,24,24,25,25), 
       pt.bg = c("white", col.both, col.tp, col.chl, col.tp, col.chl), 
       pt.cex = 1.5)

abline(0,1)
abline(h=0, col = col.chl, lwd = 3)
abline(v=0, col = col.tp, lwd = 3)

dev.off()


# Create contour plot
# have to create separately and stitch together later
# could not get contour plot to print with Tn and Tp plot

# filter data for lakes that have TN, TP and chl records
temp = change.db.all[!is.na(change.db.all$tn_coef_mean) & !is.na(change.db.all$tp_coef_mean)& !is.na(change.db.all$chl_coef_mean),]
temp = data.frame(x = temp$tp_coef_mean*100, y = temp$tn_coef_mean*100, z = temp$chl_coef_mean*100)

# interpolate for contour plot
temp.m = interp(x = temp$x, y = temp$y, z = temp$z, linear = TRUE, 
               extrap = FALSE, duplicate = "mean")

png("Nutrients_Chl_contour.png", height = 1200, width = 1200, pointsize = 20)
par(mar=c(5,5,3,0),cex = 1.2, cex.lab = 2.5, cex.axis = 2,oma=c(0,1,0,0))
filled.contour(x = temp.m$x,
               y = temp.m$y, 
               z = temp.m$z,
               levels = c(-10,-7,-4,-3,-2,-1,0,1,2,3,4,7,10),
               col = c("#031932",rev(brewer.pal(10, name = "RdBu")),"#400114"),
               #color.palette = colorRampPalette(c(rgb(5,48,97,max=255), rgb(255,255,255,max=255),rgb(103,0,31,max=255))), 
               xlab = "% Change in TP Per Year",
               ylab = "% Change in TN Per Year", 
               key.title = title(main = "% Change in \nChl Per Year", cex.main = 1.1))
# lines are a bit weird on this graph and don't necessarily correspond
# to axis values, may have to tweak by hand
lines(x = c(-6.45, 3.2), y = c(0,0),  lwd = 4, lty = 2)
lines(x = c(-1.24,-1.24), y = c(-4.09,2.485),  lwd = 4, lty = 2)
text(labels = "n = 51", x = -6.1, y = 2.3, cex = 2, adj = 0)
text(labels = "n = 93", x = -1, y = 2.3, cex = 2, adj = 0)
text(labels = "n = 201", x = 1.1, y = -3.8, cex = 2, adj = 0)
text(labels = "n = 316", x = -6.1, y = -3.8, cex = 2, adj = 0)

dev.off()

# ==========================================
# Figure 4
# create violin plots of top trend predictors identified by the rf
# this was "hu4_ppt_change for TP
# "hu4_dep_totaln_1990_mean" for TN
# "hu4_dep_change" for TN:TP
# "hu4_tmean_1990" for chl

quant.low = function(x){
  quantile(x,.25)
}
quant.high = function(x){
  quantile(x,.75)
}


pdf("tp_top_var.pdf")
#TP vs ppt change
par(mfrow=c(2,2))
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

pdf("tn_top_var.pdf")

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
dev.off()

#TN:TP vs regional dep change
pdf("tntp_top_var.pdf")
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
dev.off()

#Chl vs Regional mean temp in 1990
pdf("chl_top_var.pdf")
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


# ==========================================
# Supplmental figures
# ==========================================
# Figure S1
# histograms of LULC and size of census vs sample
# note I converted counts to proportions in each bin

pdf("sample_vs_population_hist.pdf")

par(mfrow=c(2,2), cex = 1,mar=c(3,3,0.5,0.5))
# sum row crop and pasture to create "ag" category
iws.ag = dt$iws.lulc$iws_nlcd2001_pct_81 + dt$iws.lulc$iws_nlcd2001_pct_82

h = hist(iws.ag, plot = FALSE)
h$density = h$counts/sum(h$counts)
iws.ag.sample = lake.predictors$iws_crop + lake.predictors$iws_pasture
h2 = hist(iws.ag.sample, breaks = h$breaks, plot = FALSE)
h2$density = h2$counts/sum(h2$counts)

plot(h, freq = FALSE, ylab = "Proportion of Lakes", 
     xlab = "% Agriculture", col = "gray", main = "",
     mar=c(2,2,0,0),oma=c(0,0,0,0), mgp=c(2,.5,0))
plot(h2, freq = FALSE, add = TRUE, col = rgb(106,90,205,alpha=178,max=255))
legend("topright",
       c("Census", "Sample"),
       fill = c("gray", rgb(106,90,205,alpha=178,max=255)))
# next, urban, which I used as low, medium and high developed categories
iws.urban = dt$iws.lulc$iws_nlcd2001_pct_21 + dt$iws.lulc$iws_nlcd2001_pct_22+ dt$iws.lulc$iws_nlcd2001_pct_23
h = hist(iws.urban, breaks = 20, plot = FALSE)
h$density = h$counts/sum(h$counts)
iws.urban.sample = lake.predictors$iws_urban
h2 = hist(iws.urban.sample, breaks = h$breaks, plot = FALSE)
h2$density = h2$counts/sum(h2$counts)

plot(h, freq = FALSE, ylab = "", 
     xlab = "% Urban", col = "gray", ylim = c(0, 0.8), main = "",
     mar=c(2,2,0,0), mgp=c(2,.5,0),oma=c(0,0,0,0))
plot(h2, freq = FALSE, add = TRUE, col = rgb(106,90,205,alpha=178,max=255))

# next, forest, which I calculate as mixed + hardwood + deciduous
iws.forest = dt$iws.lulc$iws_nlcd2001_pct_41 + dt$iws.lulc$iws_nlcd2001_pct_42 + dt$iws.lulc$iws_nlcd2001_pct_43
h = hist(iws.forest, breaks = 20, plot = FALSE)
h$density = h$counts/sum(h$counts)
iws.forest.sample = lake.predictors$iws_forest
h2 = hist(iws.urban.sample, breaks = h$breaks, plot = FALSE)
h2$density = h2$counts/sum(h2$counts)

plot(h, freq = FALSE, ylab = "Proportion of Lakes", 
     xlab = "% Forest", col = "gray", ylim = c(0, 0.8),
     main = "",
     mar=c(2,2,0,0),oma=c(0,0,0,0), mgp=c(2,.5,0))
plot(h2, freq = FALSE, add = TRUE, col = rgb(106,90,205,alpha=178,max=255))

# lake size comparisons
lakearea = dt$lagoslakes$lake_area_ha[dt$lagoslakes$lake_area_ha>1]
h = hist(log10(lakearea), breaks = 25, plot = FALSE)
h$density = h$counts/sum(h$counts)
lakearea.sample = lake.predictors$lake_area_ha
h2 = hist(log10(lakearea.sample), breaks = h$breaks, plot = FALSE)
h2$density = h2$counts/sum(h2$counts)

plot(h, freq = FALSE, ylab = "", 
     xlab = "Lake Area (ha)", col = "gray", ylim = c(0, 0.3),
     main = "",
     mar=c(2,2,0,0),oma=c(0,0,0,0), mgp=c(2,.5,0), xaxt = "n")
axis(1, 
     labels = c("1", "10", expression(10^{2}), expression(10^{3}), expression(10^{4}), expression(10^{5}), expression(10^{6})), at = c(0,1,2,3,4,5,6),
     mgp=c(2,.5,0))
plot(h2, freq = FALSE, add = TRUE, col = rgb(106,90,205,alpha=178,max=255))

#
dev.off()


# ===================================================================
# Figure S2
# show lake-specific categorical trends vs various data characteristics

# first calculate variables of interest from raw data
# this includes number of years of observation per lake (N Years), 
# median year of observation in each lake (Median Obs Year)
# mean nutrient value per lake (log Mean Value)

remove(temp)
temp = count(all.nut[all.nut$variable=="tn_umol", ], vars = "lagoslakeid")
names(temp)[2] = "tn_nyear"
dat.char = merge(lake.predictors[,c(1,46:49)], temp, by = "lagoslakeid", all.x = TRUE)

temp = count(all.nut[all.nut$variable=="tp_umol", ], vars = "lagoslakeid")
names(temp)[2] = "tp_nyear"
dat.char = merge(dat.char, temp, by = "lagoslakeid", all.x = TRUE)

temp = count(all.nut[all.nut$variable=="tn_tp_umol", ], vars = "lagoslakeid")
names(temp)[2] = "tn_tp_nyear"
dat.char = merge(dat.char, temp, by = "lagoslakeid", all.x = TRUE)

temp = count(all.nut[all.nut$variable=="chl_ugL", ], vars = "lagoslakeid")
names(temp)[2] = "chl_nyear"
dat.char = merge(dat.char, temp, by = "lagoslakeid", all.x = TRUE)

med = by(all.nut[,c(2)], INDICES = all.nut[,c(4,1)], FUN = median)
med = data.frame(lagoslakeid = attributes(med)$dimnames$lagoslakeid, 
                 tn_medyear = med[3,], 
                 tp_medyear = med[4,],
                 tntp_medyear = med[2,], 
                 chl_medyear = med[1,])

dat.char = merge(dat.char, med, by = "lagoslakeid", all.x = TRUE)

meanval = by(all.nut[,c(3)], INDICES = all.nut[,c(4,1)], FUN = mean)
meanval = data.frame(lagoslakeid = attributes(meanval)$dimnames$lagoslakeid, 
                 tn_meanval = meanval[3,], 
                 tp_meanval = meanval[4,],
                 tntp_meanval = meanval[2,], 
                 chl_meanval = meanval[1,])

dat.char = merge(dat.char, meanval, by = "lagoslakeid", all.x = TRUE)

# create a 4x3 panel that shows all responses ~ mean value/med year/nobs
pdf("response_vs_modelvals_boxplot.pdf", height = 7, width = 10)
par(mfrow = c(3,4), oma = c(4,3,0,0), mar = c(2,2,1,1))
boxplot(log(tn_meanval)~tn_change, data = dat.char, xaxt = "n")
boxplot(log(tp_meanval)~tp_change, data = dat.char, xaxt = "n")
boxplot(log(tntp_meanval)~tntp_change, data = dat.char, xaxt = "n")
boxplot(log(chl_meanval)~chl_change, data = dat.char, xaxt = "n")

boxplot(tn_medyear~tn_change, data = dat.char, xaxt = "n")
boxplot(tp_medyear~tp_change, data = dat.char, xaxt = "n")
boxplot(tntp_medyear~tntp_change, data = dat.char, xaxt = "n")
boxplot(chl_medyear~chl_change, data = dat.char, xaxt = "n")

boxplot(tn_nyear~tn_change, data = dat.char, xlab = "TN Change")
boxplot(tp_nyear~tp_change, data = dat.char, xlab = "TP Change")
boxplot(tn_tp_nyear~tntp_change, data = dat.char, xlab = "TN:TP Change")
boxplot(chl_nyear~chl_change, data = dat.char, "Chl Change")

mtext("TN Change", side=1, line = 2, adj=0.11, outer = TRUE)
mtext("TP Change", side=1, line = 2, adj=0.38, outer = TRUE)
mtext("TN:TP Change", side=1, line = 2, adj=0.65, outer = TRUE)
mtext("Chl Change", side=1, line = 2, adj=0.92, outer = TRUE)

mtext("log Mean Value", side = 2, line = 1, adj=.9, outer = TRUE, padj = 1)
mtext("Median Obs. Year", side = 2, line = 1, adj=.5, outer = TRUE, padj = 1)
mtext("N Years", side = 2, line = 1, adj=.14, outer = TRUE, padj = 1)

dev.off()

# =============================
# Figure S3

# add chlorophyll trend vals to dat.char
dat.char = merge(dat.char, change.db.all[,c("lagoslakeid", "chl_coef_mean")], by = "lagoslakeid", all.x = TRUE)
dat = dat.char[!is.na(dat.char$tn_meanval) & !is.na(dat.char$tp_meanval)& !is.na(dat.char$chl_coef_mean),]
dat = data.frame(x = log10(dat$tp_meanval), y = log10(dat$tn_meanval), z = dat$chl_coef_mean*100)


dat.m = interp(x = dat$x, y = dat$y, z = dat$z, linear = TRUE, 
               extrap = FALSE, duplicate = "mean")

# Create contour plot

png("Nutrients_cont_Chl_contour.png", height = 1200, width = 1200, pointsize = 20)
par(mar=c(5,5,3,0),cex = 1.2, cex.lab = 2.5, cex.axis = 2,oma=c(0,1,0,0))
filled.contour(x = dat.m$x,
               y = dat.m$y, 
               z = dat.m$z,
               levels = c(-10,-7,-4,-3,-2,-1,0,1,2,3,4,7,10),
               col = c("#031932",rev(brewer.pal(10, name = "RdBu")),"#400114"),
               #color.palette = colorRampPalette(c(rgb(5,48,97,max=255), rgb(255,255,255,max=255),rgb(103,0,31,max=255))), 
               xlab = "log TP (uM)",
               ylab = "log TN (uM)", 
               key.title = title(main = "% Change in \nChl Per Year", cex.main = 1.1))
#key.axes = axis(5, at = c(-1,0,1), labels = c(0.1, 1, 10)))
lines(x = c(-6.45, 3.2), y = c(0,0),  lwd = 4, lty = 2)
lines(x = c(-1.24,-1.24), y = c(-4.09,2.485),  lwd = 4, lty = 2)
text(labels = "n = 51", x = -6.1, y = 2.3, cex = 2, adj = 0)
text(labels = "n = 93", x = -1, y = 2.3, cex = 2, adj = 0)
text(labels = "n = 201", x = 1.1, y = -3.8, cex = 2, adj = 0)
text(labels = "n = 316", x = -6.1, y = -3.8, cex = 2, adj = 0)

dev.off()

