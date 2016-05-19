## use output from 00_Model_conditional
## create dataframe with TN and TP change through time (and sd) estimates for each lake

require(maps)
install.packages("mapproj")
library(mapproj)

# set colors for tn and tp

col.tn = rgb(.94,.73,0.035,.7)
col.tp = rgb(.07,.57,.45,0.7)
col.both = rgb(68/255, 36/255,0, 0.7)
col.no.change = "darkgray"

#change to match the number of lakes in each database
n.tn = 833
n.tp = 2096

# create data frame of % change values

tp.ints = as.numeric(output_TP_01[c(1:n.tp),1])
tp.ints.sd = as.numeric(output_TP_01[c(1:n.tp),2])
tp.slopes = as.numeric(output_TP_01[c((n.tp+1):(2*n.tp)),1])
tp.slopes.sd = as.numeric(output_TP_01[c((n.tp+1):(2*n.tp)),2])
tp.slope.pop = as.numeric(output_TP_01[rownames(output_TP_01)=="mu.beta", 1])
tp.slope.sd = as.numeric(output_TP_01[rownames(output_TP_01)=="mu.beta", 2])


tn.ints = as.numeric(output_TN_01[c(1:n.tn),1])
tn.ints.sd = as.numeric(output_TN_01[c(1:n.tn),2])
tn.slopes = as.numeric(output_TN_01[c((n.tn+1):(n.tn*2)),1])
tn.slopes.sd = as.numeric(output_TN_01[c((n.tn+1):(n.tn*2)),2])
tn.slope.pop = as.numeric(output_TN_01[rownames(output_TN_01)=="mu.beta", 1])
tn.slope.sd = as.numeric(output_TN_01[rownames(output_TN_01)=="mu.beta", 2])

change.db.tp = data.frame(tp.slopes = tp.slopes, 
                       tp.ints = tp.ints,
                       tp.slopes.sd = tp.slopes.sd, 
                       tp.ints.sd = tp.ints.sd)
change.db.tn = data.frame(tn.slopes = tn.slopes, 
                          tn.ints = tn.ints,
                          tn.slopes.sd = tn.slopes.sd, 
                          tn.ints.sd = tn.ints.sd)
## create dataframe that matches lake number to lagoslakeid

change.db.tp$lake_num = c(1:n.tp)
lake.conv = dat[,c(1,8)]
lake.conv = unique(lake.conv)
change.db.tp = merge(change.db.tp, lake.conv, by = "lake_num", all.x = TRUE)

change.db.tn$lake_num = c(1:n.tn)
lake.conv = dat[,c(1,8)]
lake.conv = unique(lake.conv)
change.db.tn = merge(change.db.tn, lake.conv, by = "lake_num", all.x = TRUE)

lake.info = data.lake.specific[,c("lagoslakeid", "nhd_lat", "nhd_long")]


change.db.tp = merge(change.db.tp, lake.info, by = "lagoslakeid", all.x = TRUE)
change.db.tn = merge(change.db.tn, lake.info, by = "lagoslakeid", all.x = TRUE)

for (i in c(1:n.tp)) {
  if (change.db.tp$tp.slopes[i]-(1.645*change.db.tp$tp.slopes.sd[i]) > 0) {
    change.db.tp$tp.change[i] = "positive"
  } else {
    if (change.db.tp$tp.slopes[i]+(1.645*change.db.tp$tp.slopes.sd[i]) < 0) {
      change.db.tp$tp.change[i] = "negative"
    } else {
      change.db.tp$tp.change[i] = "no change"
    }
  }
}

for (i in c(1:n.tn)) {
  if (change.db.tn$tn.slopes[i]-(1.645*change.db.tn$tn.slopes.sd[i]) > 0) {
    change.db.tn$tn.change[i] = "positive"
  } else {
    if (change.db.tn$tn.slopes[i]+(1.645*change.db.tn$tn.slopes.sd[i]) < 0) {
      change.db.tn$tn.change[i] = "negative"
    } else {
      change.db.tn$tn.change[i] = "no change"
    }
  }
}


## calculate probability of change > or < 0 for each lake
## create histograms of probability of change
change.db.tp$tp.slopes.z = change.db.tp$tp.slopes/change.db.tp$tp.slopes.sd
change.db.tp$tp.slopes.p = (1-2*pnorm(-abs(change.db.tp$tp.slopes.z)))*(change.db.tp$tp.slopes/(abs(change.db.tp$tp.slopes)))

change.db.tn$tn.slopes.z = change.db.tn$tn.slopes/change.db.tn$tn.slopes.sd
change.db.tn$tn.slopes.p = (1-2*pnorm(-abs(change.db.tn$tn.slopes.z)))*(change.db.tn$tn.slopes/(abs(change.db.tn$tn.slopes)))

## create histograms of probabilities
pdf("TN_TP_change_prob.pdf")
par(mar = c(5,5,4,2))
hist(change.db.tn$tn.slopes.p, breaks = 20, 
     main="", xlab = "Probability of Change", ylab = "Density", bty="l", freq = FALSE, 
     cex.lab = 1.8, cex.axis = 1.3, xaxt = "n", col = col.tn)
hist(change.db.tp$tp.slopes.p, breaks = 20, add = TRUE, col = col.tp, freq = FALSE)
axis(side = 1, labels = c(1,0.5,0,0.5,1), at = c(-1,-0.5,0,.5,1), cex.axis = 1.3)

# add red line at zero
abline(v = 0, col = "red", lwd = 2, lty = 2)

# add text
text(-0.7, .1, "Decreasing", col = "white", cex = 1.3)
text(0.7, .1, "Increasing", col = "white", cex = 1.3)
text(0, .1, "No Trend", col = "white", cex = 1.3)

# add legend
legend(0.5, .7, c("TN", "TP"), fill= c(col.tn, col.tp))

dev.off()

 


## create histograms of estimates of % change
pdf("TN_TP_change_hist.pdf")

par(mar = c(5,5,4,2))
hist(change.db.tn$tn.slopes, col=col.tn, breaks=20,
     main="", xlab = "% Change per year", ylab = "Density", bty="l", freq = FALSE, 
     cex.lab = 1.8, cex.axis = 1.3, xaxt = "n", xlim = c(-0.08, 0.07))
axis(side = 1, labels = c(-8,-6, -4, -2, 0, 2, 4, 6), at = c(-0.08,-.06, -0.04, -0.02,0,.02,.04,0.06))
hist(change.db.tp$tp.slopes, add=TRUE, breaks=30, col=col.tp, freq = FALSE)
legend(-0.06, 30, c("TN", "TP"), fill= c(col.tn, col.tp))


# add red line at zero
abline(v = 0, col = "red", lwd = 2, lty = 2)

dev.off()


# create a map with pos/neg change in TN and TP

library(rworldmap)
newmap <- getMap(resolution = "high")
plot(newmap, xlim = c(-98, -66), ylim = c(34, 50), asp = 1)

pdf("TN_directional_change.pdf")
map(database = "state", regions=
                                  projection="albers", par=c(lat0=48,lat1=37))

map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE, col = "lightgray", add = TRUE)

#first plot points where TN is different from zero
points(change.db.tn$nhd_long[change.db.tn$tn.change=="no change"], change.db.tn$nhd_lat[change.db.tn$tn.change=="no change"], 
       cex=0.7, col = col.no.change, pch = 16)

points(change.db.tn$nhd_long[change.db.tn$tn.change=="positive"], change.db.tn$nhd_lat[change.db.tn$tn.change=="positive"],
       col = col.both, pch=16, cex=1.2)
points(change.db.tn$nhd_long[change.db.tn$tn.change=="negative"], change.db.tn$nhd_lat[change.db.tn$tn.change=="negative"],
       col = col.tn, pch=16,cex=1.2)
legend(-83, 49.5, c("Increasing", "Decreasing", "No Change"), fill= c(col.both, col.tn, col.no.change))
dev.off()

pdf("TP_directional_change.pdf")
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE, col = "lightgray")

#first plot points where tp is different from zero
points(change.db.tp$nhd_long[change.db.tp$tp.change=="no change"], change.db.tp$nhd_lat[change.db.tp$tp.change=="no change"], 
       cex=0.7, col = col.no.change, pch = 16)

points(change.db.tp$nhd_long[change.db.tp$tp.change=="positive"], change.db.tp$nhd_lat[change.db.tp$tp.change=="positive"],
       col = col.both, pch=16, cex=1.2)
points(change.db.tp$nhd_long[change.db.tp$tp.change=="negative"], change.db.tp$nhd_lat[change.db.tp$tp.change=="negative"],
       col = col.tp, pch=16,cex=1.2)
legend(-83, 49.5, c("Increasing", "Decreasing", "No Change"), fill= c(col.both, col.tp, col.no.change))
dev.off()

## create a map that plots regional mean TN & TP
## overlaps regions with color coded lakes

require(maps)
require(maptools)
require(rgdal)
require(sp)
require(ggplot2)

setwd("C:/Users/Samantha/Dropbox/GEO-Shared2/MGD_Shapefile_Exports_to_map_in_R_May2014")
huc4 = readOGR(dsn ="/Users/Samantha/Dropbox/GEO-Shared2/MGD_Shapefile_Exports_to_map_in_R_May2014", layer = "HU4_simple_wgs1984")

huc4_tn_mean = as.data.frame(tapply(change.db$tn.slopes, INDEX = change.db$hu4_zoneid, FUN = mean))
huc4_tn_sd = as.data.frame(tapply(change.db$tn.slopes, INDEX = change.db$hu4_zoneid, FUN = sd))
huc4_tn = data.frame(hu4_zoneid = rownames(huc4_tn_mean), 
                     mean = as.numeric(huc4_tn_mean[,1]), 
                     sd = as.numeric(huc4_tn_sd[,1]))

# remove NAs to get read for plotting
huc4_tn = huc4_tn[!is.na(huc4_tn$mean), ]

huc4_tp_mean = as.data.frame(tapply(change.db$tp.slopes, INDEX = change.db$hu4_zoneid, FUN = mean))
huc4_tp_sd = as.data.frame(tapply(change.db$tp.slopes, INDEX = change.db$hu4_zoneid, FUN = sd))
huc4_tp = data.frame(hu4_zoneid = rownames(huc4_tp_mean), 
                     mean = as.numeric(huc4_tp_mean[,1]), 
                     sd = as.numeric(huc4_tp_sd[,1]))

# remove NAs to get read for plotting
huc4_tp = huc4_tp[!is.na(huc4_tp$mean), ]


library(colorspace)
names(huc4_tp)[1] = "ZoneID"
names(huc4_tn)[1] = "ZoneID"

huc4 = merge(huc4, huc4_tp, by="ZoneID", all.x = TRUE)
huc4 = merge(huc4, huc4_tn, by="ZoneID", all.x = TRUE)
names(huc4)[25:28] = c("mean_tp", "sd_tp", "mean_tn", "sd_tn")

huc4@data$id = rownames(huc4@data)
huc4=fortify(huc4, region="i")

z=huc4$mean_tp*100
# remove the lowest value because it's throwing off colors
z[4] = NA
col_ramp_tn = c(col.tn,col.both)
col_ramp_tp = c(col.tp,col.both)

zcol <- colorRampPalette(col_ramp_tp)(65)[as.numeric(cut(z, breaks = 65))]


pdf("HUC4_mean_tp.pdf")
plot(huc4,col=zcol, border="gray50", lty = 1, lwd= 1)
plot(states[which(states$State_Name %in% LAGOS.states), ], lwd=1.1, add=TRUE)

#add legend

## set the location and the colorbar gradation
xleft <- -81.5
xright <- -80
ybot <- 44
yint <- (48-44) / 65
ytop <- ybot + yint


## create the bar by stacking a bunch of colored rectangles
for(c in colorRampPalette(col_ramp_tp)(65)){
  ybot = ybot + yint
  ytop = ytop + yint
  rect(xleft, ybot, xright, ytop, border = NA, col = c)
  print(c(xleft, xright, ybot, ytop, c))
}

## generate labels
labels <- round(seq(min(z, na.rm = TRUE), max(z, na.rm = TRUE), length.out = 5),2)

## add the labels to the plot
text(c(xright + 0.0005),
     seq(44, 48, length.out = 5),
     labels = as.character(labels),
     cex = 0.8,
     pos = 4)
text(-79.6, 48.9, "TN % Change", offset=0, cex=.9)
rect(xleft, ytop - 65*yint, xright, ytop)
rect(-82.5, 43.7, -76.6, 49.4)

dev.off()

## function to create colors around zero

diverge.color <- function(start.color,end.color,min.value,max.value,mid.value=0,mid.color="ivory") 
  
{ 
  # based on ideas from Maureen Kennedy, Nick Povak, and Alina Cansler 
  
  # creates a palette for the current session for a divergent-color 
  # graphic with a non-symmetric range 
  # "cuts" = the number of slices to be made in the range above and below "mid.value" 
  
  ramp1 <- colorRampPalette(c(start.color,mid.color)) 
  ramp2 <- colorRampPalette(c(mid.color,end.color)) 
  
  # now specify the number of values on either side of "mid.value" 
  
  max.breaks <- length(which(z > 0))
  min.breaks <- length(which(z < 0))+1
  
  num.breaks <- max.breaks + min.breaks
  
  low.ramp <- ramp1(min.breaks) 
  high.ramp <- ramp2(max.breaks) 
  
  # now create a combined ramp from the higher values of "low.ramp" and 
  # the lower values of "high.ramp", with the longer one using all values 
  # high.ramp starts at 2 to avoid duplicating zero 
  
  myColors <- c(low.ramp,high.ramp[2:max.breaks]) 
  
  myColors 
} 

test = diverge.color(start.color = col_ramp_tn[1], "black", min(z, na.rm = TRUE), max(z, na.rm = TRUE), mid.value = 0, mid.color = "lightgray")
test = test[as.numeric(cut(z, breaks = 41, na.rm = TRUE))]

test2 = diverge.color(start.color = col_ramp_tn[1], "black", min(z, na.rm = TRUE), max(z, na.rm = TRUE), mid.value = 0, mid.color = "lightgray")


plot(huc4,col=test, border="gray50", lty = 1, lwd= 1)

## create a figure that plots TN change vs TP change
temp.tn = 100*change.db.tn$tn.slopes[change.db.tn$lagoslakeid %in% change.db.tp$lagoslakeid]
temp.tp = 100*change.db.tp$tp.slopes[change.db.tp$lagoslakeid %in% change.db.tn$lagoslakeid]
temp.tn.change = change.db.tn$tn.change[change.db.tn$lagoslakeid %in% change.db.tp$lagoslakeid]
temp.tp.change = change.db.tp$tp.change[change.db.tp$lagoslakeid %in% change.db.tn$lagoslakeid]

pdf("TN_TP_change.pdf")
par(mar=c(5,5,1,1))
plot(temp.tn[temp.tn.change == "positive" & temp.tp.change == "no change"]~temp.tp[temp.tn.change == "positive" & temp.tp.change == "no change"], 
     xlab = "% Change in TP per year", 
     ylab = "% Change in TN per year", 
     cex = 1.5, cex.lab = 2, cex.axis = 1.5,  col = "black", pch=24, bg = col.tn,
     xlim = c(-8,6), ylim = c(-6, 3))
# Tn no change, TP no change
points(temp.tn[temp.tn.change == "no change" & temp.tp.change == "no change"]~temp.tp[temp.tn.change == "no change" & temp.tp.change == "no change"],
       pch=1, cex = 1.5)
#TN up, TP up
points(temp.tn[temp.tn.change == "positive" & temp.tp.change == "positive"]~temp.tp[temp.tn.change == "positive" & temp.tp.change == "positive"],
       col = "black", bg = col.both, pch=19, cex = 1.5)
#TN down, TP down
points(temp.tn[temp.tn.change == "negative" & temp.tp.change == "negative"]~temp.tp[temp.tn.change == "negative" & temp.tp.change == "negative"],
       col = "black", bg = col.both, pch=19, cex = 1.5)
#TN down, TP no change
points(temp.tn[temp.tn.change == "negative" & temp.tp.change == "no change"]~temp.tp[temp.tn.change == "negative" & temp.tp.change == "no change"],
       col = "black", bg = col.tn, pch=25, cex = 1.5)
#TN no change, TP down
points(temp.tn[temp.tn.change == "no change" & temp.tp.change == "negative"]~temp.tp[temp.tn.change == "no change" & temp.tp.change == "negative"],
       col = "black", bg = col.tp, pch=25, cex = 1.5)
#TN no change, TP up
points(temp.tn[temp.tn.change == "no change" & temp.tp.change == "positive"]~temp.tp[temp.tn.change == "no change" & temp.tp.change == "positive"],
       col = "black", bg = col.tp, pch=24, cex = 1.5)
legend("bottomright",
       c("No Change", "Both Changing", "TN Increasing",  "TP Increasing","TN Decreasing", "TP Decreasing"), 
       pch = c(21,21,24,24,25,25), 
       pt.bg = c("white", col.both, col.tn, col.tp, col.tn, col.tp), 
       pt.cex = 1.5)



abline(0,1)
abline(h=0, col = col.tn, lwd = 3)
abline(v=0, col = col.tp, lwd = 3)

dev.off()

## create fig of lake-specific slopes and intercepts and predictors

dat.unique = dat[,c(1,2,10:13,24:31)]
dat.unique = unique(dat.unique)
change.db = merge(change.db, dat.unique, "lagoslakeid", all.x = TRUE)

pdf("TN_ints_crops.pdf")
par(mar=c(5,5,1,1))

plot(change.db$tn.ints~change.db$iws_crop, 
     xlab = "% Crops in Watershed", 
     ylab = "Lake-Specific Intercepts", 
     cex = 1.5, cex.lab = 2, cex.axis = 1.5)
points(change.db$tn.ints[change.db$tn.change == "negative"]~change.db$iws_crop[change.db$tn.change == "negative"], 
       col = col.tn, pch=16, cex = 1.5)
points(change.db$tn.ints[change.db$tn.change == "positive"]~change.db$iws_crop[change.db$tn.change == "positive"], 
       col = col.both, pch=16, cex = 1.5)
legend(1.4, 3.5, c("Increasing", "Decreasing", "No Change"), fill= c(col.both, col.tn, "white"), cex = .8)

dev.off()

pdf("TN_slopes_dep.pdf")
par(mar=c(5,5,1,1))

plot(change.db$tn.slopes~change.db$tn_dep_mean, 
     xlab = "Mean TN Deposition", 
     ylab = "Lake-Specific Intercepts", 
     cex = 1.5, cex.lab = 2, cex.axis = 1.5)
points(change.db$tn.slopes[change.db$tn.change == "negative"]~change.db$tn_dep_mean[change.db$tn.change == "negative"], 
       col = col.tn, pch=16, cex = 1.5)
points(change.db$tn.slopes[change.db$tn.change == "positive"]~change.db$tn_dep_mean[change.db$tn.change == "positive"], 
       col = col.both, pch=16, cex = 1.5)
legend(-2.5, -0.04, c("Increasing", "Decreasing", "No Change"), fill= c(col.both, col.tn, "white"), cex = 1)

dev.off()


## plot a mock up of hierarchical methods
pdf("TP_all_1.pdf")
par(mar=c(5,5,1,1))

plot(log(dat$tp_umol) ~ dat$sampleyear_cor,
     xlab = "Years since 1990", 
     ylab = "log TP (umol)", 
     cex = 1.5, cex.lab = 2, cex.axis = 1.5)
abline(0.16, -0.002, col = "red", lwd = 3)


abline(-0.72, -0.012, col = "blue", lwd = 2)
abline(-1.26, 0.004, col = "blue", lwd = 2)
abline(0.79, 0.019, col = "blue", lwd = 2)
abline(1.14, .007, col = "blue", lwd = 2)
abline(.23, .013, col = "blue", lwd = 2)

dev.off()

pdf("TP_1lake.pdf")
par(mar=c(5,5,1,1))
plot(log(dat$tp_umol[dat$lagoslakeid == 65]) ~ dat$sampleyear_cor[dat$lagoslakeid == 65],
     xlab = "Years since 1990", 
     ylab = "log TP (umol)", 
     cex = 1.5, cex.lab = 2, cex.axis = 1.5)
abline(lm(log(dat$tp_umol[dat$lagoslakeid == 65]) ~ dat$sampleyear_cor[dat$lagoslakeid == 65]), col = "blue", lwd = 3)
dev.off()

## plot all lakes in LAGOS with TP or TN measurement

data.tp = subset(data, !is.na(data[,22]))
data.tn = subset(data, !is.na(data[,96]))
data.tp = merge(data.tp, data.lake.specific, "lagoslakeid", all.x = TRUE)
data.tn = merge(data.tn, data.lake.specific, "lagoslakeid", all.x = TRUE)
data.tp.loc = data.tp[,c(1,98,99)]
data.tp.loc = unique(data.tp.loc)

data.tn.loc = data.tn[,c(1,98,99)]
data.tn.loc = unique(data.tn.loc)

pdf("TP_all_obs.pdf")
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE, col = "lightgray")

#first plot points where tp is different from zero
points(data.tp.loc$nhd_long,data.tp.loc$nhd_lat, 
       cex=0.5, col = col.tp, pch = 16)

text(-82, 48.5, "9372 lakes - 149,997 TP obs")
dev.off()


pdf("TN_all_obs.pdf")
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE, col = "lightgray")

#first plot points where tp is different from zero
points(data.tp.loc$nhd_long,data.tp.loc$nhd_lat, 
       cex=0.5, col = col.tn, pch = 16)

text(-82, 48.5, "6210 lakes - 58,016 TN obs")
dev.off()

## create a map that shows how many year records the lake has
library(plyr)
counts = count(data.tn, vars = "lagoslakeid")
counts = merge(counts, data.lake.specific[,c(1,3,4)], "lagoslakeid", all.x = TRUE)


pdf("TN_Num_yrs_location.pdf")
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE, col = "lightgray")
points(counts$nhd_long,counts$nhd_lat, 
       cex=counts$freq/10, col  = col.tn, pch = 16)

legend(-83, 49, 
       c("2 years", "5 years", "10 years", "20 years"), 
       pt.cex = c(2/10, 5/10, 10/10, 20/10), 
       pch = 16, 
       col = col.tn)
dev.off()

## create a histogram of mean tn & tp observations
pdf("hist_TN_TP_vals.pdf")
par(mar = c(5,5,4,2))

hist(log10(dat$tn_umol*14), breaks = 20, 
     main="", xlab = "TN or TP (ug/L)", ylab = "Number of lakes", bty="l", freq = TRUE, 
     cex.lab = 1.8, cex.axis = 1.3, xaxt = "n", col = col.tn, xlim = c(0,5))
hist(log10(dat$tp_umol*30.9), breaks = 20, add = TRUE, col = col.tp)
axis(side = 1, labels = c(1,10,100,1000,10000), at = c(0,1,2,3,4), cex.axis = 1.3)


# add legend
legend(.3, 1100, c("TN", "TP"), fill= c(col.tn, col.tp), cex = 1.2)

text(3,200,"1118", cex = 2, col = "white")
text(1.4,200,"53", cex = 2, col = "white")
dev.off()


## create fig of precip data
pdf("TN_precip_all.pdf")
par(mar=c(5,5,1,1))

plot(log(dat$tn_umol) ~ dat$precip_spring,
     xlab = "Standardized Winter Precip", 
     ylab = "log TN (umol)", 
     cex = 1.5, cex.lab = 2, cex.axis = 1.5)
abline(4.16, .012, col = "red", lwd = 3)


abline(3.9, .01, col = "blue", lwd = 2)
abline(2.3, -0.02, col = "blue", lwd = 2)
abline(5, 0.03, col = "blue", lwd = 2)
abline(4.8, .04, col = "blue", lwd = 2)
abline(3.2, .001, col = "blue", lwd = 2)

dev.off()

## Tn vs TP model changes

pdf("TN_TP_change_mod.pdf")
par(mar=c(5,5,1,1))
temp.tn = 100*change.db$tn.slopes
temp.tp = 100*change.db$tp.slopes
#TN up, TP no change
plot(temp.tn[change.db$tn.change == "positive" & change.db$tp.change == "no change"]~temp.tp[change.db$tn.change == "positive" & change.db$tp.change == "no change"], 
     xlab = "% Change in TP per year", 
     ylab = "% Change in TN per year", 
     cex = 1.5, cex.lab = 2, cex.axis = 1.5,  col = "black", pch=24, bg = col.tn,
     xlim = c(-8,6), ylim = c(-6, 3))
# Tn no change, TP no change
points(temp.tn[change.db$tn.change == "no change" & change.db$tp.change == "no change"]~temp.tp[change.db$tn.change == "no change" & change.db$tp.change == "no change"],
      pch=1, cex = 1.5)
#TN up, TP up
points(temp.tn[change.db$tn.change == "positive" & change.db$tp.change == "positive"]~temp.tp[change.db$tn.change == "positive" & change.db$tp.change == "positive"],
       col = "black", bg = col.both, pch=24, cex = 1.5)
#TN down, TP down
points(temp.tn[change.db$tn.change == "negative" & change.db$tp.change == "negative"]~temp.tp[change.db$tn.change == "negative" & change.db$tp.change == "negative"],
       col = "black", bg = col.both, pch=25, cex = 1.5)
#TN down, TP no change
points(temp.tn[change.db$tn.change == "negative" & change.db$tp.change == "no change"]~temp.tp[change.db$tn.change == "negative" & change.db$tp.change == "no change"],
       col = "black", bg = col.tn, pch=25, cex = 1.5)
#TN no change, TP down
points(temp.tn[change.db$tn.change == "no change" & change.db$tp.change == "negative"]~temp.tp[change.db$tn.change == "no change" & change.db$tp.change == "negative"],
       col = "black", bg = col.tp, pch=25, cex = 1.5)
#TN no change, TP up
points(temp.tn[change.db$tn.change == "no change" & change.db$tp.change == "positive"]~temp.tp[change.db$tn.change == "no change" & change.db$tp.change == "positive"],
       col = "black", bg = col.tp, pch=24, cex = 1.5)

abline(0,1, lwd = 3, lty = 3)
abline(h=0, col = col.tn, lwd = 3)
abline(v=0, col = col.tp, lwd = 3)


dev.off()

## create histograms of estimates of % change
jpeg("TN_TP_change_hist.jpg", height=200, width=800)

par(mar = c(5,5,4,2))
hist(change.db$tp.slopes, col=col.tp, breaks=20,
     main="",xlab = "", ylab = "", bty="l", freq = TRUE, 
     cex.lab = 1.8, cex.axis = 1.3, yaxt = "n", xlim = c(-0.08, 0.06))


dev.off()
