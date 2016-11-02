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
n.tntp = 742
n.chl = 2143

setwd("C:/Users/Samantha/Dropbox/CSI-LIMNO_DATA/LAGOSData/Version1.054.1")
data.lake.specific = read.table("lagos_lakes_10541.txt", 
                                header = TRUE, 
                                sep = "\t", 
                                quote = "", 
                                dec = ".", 
                                strip.white = TRUE,
                                comment.char = "")

lake.info = data.lake.specific[,c("lagoslakeid", "nhd_lat", "nhd_long")]



## calculate probability of change > or < 0 for each lake
## create histograms of probability of change
change.db.tp$tp.slopes.z = change.db.tp$tp.slopes/change.db.tp$tp.slopes.sd
change.db.tp$tp.slopes.p = (1-2*pnorm(-abs(change.db.tp$tp.slopes.z)))*(change.db.tp$tp.slopes/(abs(change.db.tp$tp.slopes)))

change.db.tn$tn.slopes.z = change.db.tn$tn.slopes/change.db.tn$tn.slopes.sd
change.db.tn$tn.slopes.p = (1-2*pnorm(-abs(change.db.tn$tn.slopes.z)))*(change.db.tn$tn.slopes/(abs(change.db.tn$tn.slopes)))


## merge change dbs with slopes calculated post 2006

change.db.tp = merge(change.db.tp, tp.slopes.07[,c(2,3)], by = "lagoslakeid", all.x = TRUE)
change.db.tn = merge(change.db.tn, tn.slopes.07[,c(2,3)], by = "lagoslakeid", all.x = TRUE)


## create dataframe with all lake slopes (TP, TN, CHL, TNTP)

temp.tn = change.db.tn[[1]][,c(2,3,11,13,14)]
names(temp.tn)[3:5] = c("tn_coef_mean", "tn_ymin", "tn_ymax")

temp.tp = change.db.tp[[1]][,c(2,3,11,13,14)]
names(temp.tp)[3:5] = c("tp_coef_mean", "tp_ymin", "tp_ymax")

temp.tntp = change.db.tntp[[1]][,c(2,3,11,13,14)]
names(temp.tntp)[3:5] = c("tntp_coef_mean", "tntp_ymin", "tntp_ymax")

temp.chl = change.db.chl[[1]][,c(2,3,11,13,14)]
names(temp.chl)[3:5] = c("chl_coef_mean", "chl_ymin", "chl_ymax")

change.db.all = merge(temp.tn[temp.tn$term=="sampleyear_cor",c(2:5)], temp.tp[temp.tp$term=="sampleyear_cor",c(2:5)], by = "lagoslakeid", all = TRUE)
change.db.all = merge(change.db.all, temp.chl[temp.chl$term == "sampleyear_cor",c(2:5)], by = "lagoslakeid", all = TRUE)
change.db.all = merge(change.db.all, temp.tntp[temp.tntp$term == "sampleyear_cor",c(2:5)], by = "lagoslakeid", all = TRUE)

setwd("C:/Users/Samantha/Dropbox/CSI_LIMNO_Manuscripts-presentations/CSI_Nitrogen MSs/Time series/Data")

write.table(change.db.all, "lmer_change_db.txt")

## create dataframe with all region slopes (TP, TN, CHL, TNTP)

temp.tn = change.db.tn[[2]][,c(1,2,6,8,9)]
names(temp.tn)[3:5] = c("tn_coef_mean", "tn_ymin", "tn_ymax")

temp.tp = change.db.tp[[2]][,c(1,2,6,8,9)]
names(temp.tp)[3:5] = c("tp_coef_mean", "tp_ymin", "tp_ymax")

temp.chl = change.db.chl[[2]][,c(1,2,6,8,9)]
names(temp.chl)[3:5] = c("chl_coef_mean", "chl_ymin", "chl_ymax")

temp.tntp = change.db.tntp[[2]][,c(1,2,6,8,9)]
names(temp.tntp)[3:5] = c("tntp_coef_mean", "tntp_ymin", "tntp_ymax")

change.db.region = merge(temp.tn[temp.tn$term=="sampleyear_cor",c(1,3,4,5)], temp.tp[temp.tp$term=="sampleyear_cor",c(1,3,4,5)], by = "hu4_zoneid", all = TRUE)
change.db.region = merge(change.db.region, temp.chl[temp.chl$term == "sampleyear_cor",c(1,3,4,5)], by = "hu4_zoneid", all = TRUE)

change.db.region = merge(change.db.region, temp.tntp[temp.tntp$term == "sampleyear_cor",c(1,3,4,5)], by = "hu4_zoneid", all = TRUE)


setwd("C:/Users/Samantha/Dropbox/CSI_LIMNO_Manuscripts-presentations/CSI_Nitrogen MSs/Time series/Data")

write.table(change.db.region, "lmer_change_db_region.txt")

# create change.db but this time with BLUPs (not combined coefficients)
# so that lake and region BLUPs can be used in the random forest analysis

temp.tn = change.db.tn[[1]][,c(2,3,5)]
names(temp.tn)[3] = "tn_blup"

temp.tp = change.db.tp[[1]][,c(2,3,5)]
names(temp.tp)[3] = "tp_blup"

temp.tntp = change.db.tntp[[1]][,c(2,3,5)]
names(temp.tntp)[3] = "tntp_blup"

temp.chl = change.db.chl[[1]][,c(2,3,5)]
names(temp.chl)[3] = "chl_blup"

lake.blups = merge(temp.tn[temp.tn$term=="sampleyear_cor",c(2:3)], temp.tp[temp.tp$term=="sampleyear_cor",c(2:3)], by = "lagoslakeid", all = TRUE)
lake.blups = merge(lake.blups, temp.chl[temp.chl$term == "sampleyear_cor",c(2:3)], by = "lagoslakeid", all = TRUE)
lake.blups = merge(lake.blups, temp.tntp[temp.tntp$term == "sampleyear_cor",c(2:3)], by = "lagoslakeid", all = TRUE)

# create the same dataframe for regions

temp.tn = change.db.tn[[2]][,c(1:3)]
names(temp.tn)[3] = "tn_blup"

temp.tp = change.db.tp[[2]][,c(1:3)]
names(temp.tp)[3] = "tp_blup"

temp.tntp = change.db.tntp[[2]][,c(1:3)]
names(temp.tntp)[3] = "tntp_blup"

temp.chl = change.db.chl[[2]][,c(1:3)]
names(temp.chl)[3] = "chl_blup"

region.blups = merge(temp.tn[temp.tn$term=="sampleyear_cor",c(1,3)], temp.tp[temp.tp$term=="sampleyear_cor",c(1,3)], by = "hu4_zoneid", all = TRUE)
region.blups = merge(region.blups, temp.chl[temp.chl$term == "sampleyear_cor",c(1,3)], by = "hu4_zoneid", all = TRUE)
region.blups = merge(region.blups, temp.tntp[temp.tntp$term == "sampleyear_cor",c(1,3)], by = "hu4_zoneid", all = TRUE)


#################################################
# create a map with pos/neg change in TN and TP
#################################################
library(rworldmap)
newmap <- getMap(resolution = "high")
plot(newmap, xlim = c(-98, -66), ylim = c(34, 50), asp = 1)

pdf("TN_directional_change.pdf")

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

# create a map of lakes with longterm records in both N and P, 
# plot change on map following symbols of TN vs TP plot

pdf("map_TNTP_directional_change.pdf")
par(mar=c(0,0,0,0), oma = c(0,0,0,0))
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE, col = "lightgray")

temp.tn = change.db.tn[change.db.tn$lagoslakeid %in% change.db.tp$lagoslakeid,]
temp = merge(temp.tn, change.db.tp[,c(1,9)], by = "lagoslakeid", all.x = TRUE)

#first plot points where there is no change
points(temp$nhd_long[temp$tn.change=="no change" & temp$tp.change=="no change"], temp$nhd_lat[temp$tn.change=="no change" & temp$tp.change=="no change"], 
       cex=1.2, pch = 1, col = "darkgray")

#plot lakes where tn is not changing, tp is going up
points(temp$nhd_long[temp$tn.change=="no change" & temp$tp.change=="positive"], temp$nhd_lat[temp$tn.change=="no change" & temp$tp.change=="positive"], 
       cex=1.2, col = "black", pch=24, bg = col.tp)

#plot lakes where tn is not changing, tp is going down
points(temp$nhd_long[temp$tn.change=="no change" & temp$tp.change=="negative"], temp$nhd_lat[temp$tn.change=="no change" & temp$tp.change=="negative"], 
       cex=1.2, col = "black", pch=25, bg = col.tp)

#plot lakes where tn is going up, no change in tp
points(temp$nhd_long[temp$tn.change=="positive" & temp$tp.change=="no change"], temp$nhd_lat[temp$tn.change=="positive" & temp$tp.change=="no change"], 
       cex=1.2, col = "black", pch=24, bg = col.tn)

#plot lakes where tn is going down, no change in tp
points(temp$nhd_long[temp$tn.change=="negative" & temp$tp.change=="no change"], temp$nhd_lat[temp$tn.change=="negative" & temp$tp.change=="no change"], 
       cex=1.2, col = "black", pch=25, bg = col.tn)


#plot lakes where both nutrients are going up
points(temp$nhd_long[temp$tn.change=="positive" & temp$tp.change=="positive"], temp$nhd_lat[temp$tn.change=="positive" & temp$tp.change=="positive"], 
       cex=1.2, col = "black", pch=24, bg = col.both)

#plot lakes where both nutrients are going down
points(temp$nhd_long[temp$tn.change=="negative" & temp$tp.change=="negative"], temp$nhd_lat[temp$tn.change=="negative" & temp$tp.change=="negative"], 
       cex=1.2, col = "black", pch=24, bg = col.both)

dev.off()

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


############################################################
## create dotplot that shows bias in type of lakes that we sample
############################################################

names(data.tn)
lake.info.short = lake.info[,c(1,5,12,26,32,38)]

lake.list <- function(dat){
  lakes = data.frame(lagoslakeid = unique(dat[,"lagoslakeid"]))
  lakes = merge(lakes, lake.info.short, by = "lagoslakeid", all.x = TRUE)
  return(lakes)
}
lakes.tn = lake.list(data.tn)
lakes.tp = lake.list(data.tp)
lakes.tntp = lake.list(data.tntp)
lakes.chl = lake.list(data.chl)


## calculate population and sample statistics
# area
area = aggregate(data.lake.specific$lake_area_ha, by = list(data.lake.specific$state_name), mean)
names(area) = c("state", "area")
area.sample.tn = aggregate(tn$lake_area_ha, by = list(tn$state_name), mean)
area.sample.tp = aggregate(tp$lake_area_ha, by = list(tp$state_name), mean)
names(area.sample.tn) = c("state", "area")
names(area.sample.tp) = c("state", "area")

# max depth
depth = aggregate(data.lake.specific$maxdepth, by = list(data.lake.specific$state_name), mean, na.rm = TRUE)
names(depth) = c("state", "depth")

depth.sample.tn = aggregate(tn$maxdepth, by = list(tn$state_name), mean, na.rm = TRUE)
depth.sample.tp = aggregate(tp$maxdepth, by = list(tp$state_name), mean, na.rm = TRUE)
names(depth.sample.tn) = c("state", "depth")
names(depth.sample.tp) = c("state", "depth")


# watershed area to lake area ratio
data.lake.specific$wsala = data.lake.specific$iws_areaha/data.lake.specific$lake_area_ha
tn$wsala = tn$iws_areaha/tn$lake_area_ha
tp$wsala = tp$iws_areaha/tp$lake_area_ha

wsala = aggregate(data.lake.specific$wsala, by = list(data.lake.specific$state_name), mean, na.rm = TRUE)
wsala.sample.tn = aggregate(tn$wsala, by = list(tn$state_name), mean, na.rm = TRUE)
wsala.sample.tp = aggregate(tp$wsala, by = list(tp$state_name), mean, na.rm = TRUE)


names(wsala) = c("state", "wsala")
names(wsala.sample.tn) = c("state", "wsala")
names(wsala.sample.tp) = c("state", "wsala")

# lake connectivity as proportion isolated (seepage lakes)

prop.iso <- function(x) {
  n.lakes = length(x)
  n.iso = as.numeric(summary(x)[4])
  return(n.iso/n.lakes)
}

connectivity = aggregate(data.lake.specific$lakeconnectivity, by = list(data.lake.specific$state_name), prop.iso)
names(connectivity) = c("state", "connectivity")

connectivity.sample.tn = aggregate(tn$lakeconnectivity, by = list(tn$state_name), prop.iso)
connectivity.sample.tp = aggregate(tp$lakeconnectivity, by = list(tp$state_name), prop.iso)
names(connectivity.sample.tn) = c("state", "connectivity")
names(connectivity.sample.tp) = c("state", "connectivity")

# proportion of lakes in population

abundance = aggregate(data.lake.specific$lagoslakeid, by = list(data.lake.specific$state_name), length)
names(abundance) = c("state", "abundance")
abundance$abundance = abundance$abundance/length(data.lake.specific$lagoslakeid)

abundance.sample.tn = aggregate(tn$lagoslakeid, by = list(tn$state_name), length)
names(abundance.sample.tn) = c("state", "abundance.sample.tn")
abundance.sample.tn$abundance.sample.tn = abundance.sample.tn$abundance.sample.tn/length(tn$lagoslakeid)

abundance.sample.tp = aggregate(tp$lagoslakeid, by = list(tp$state_name), length)
names(abundance.sample.tp) = c("state", "abundance.sample.tp")
abundance.sample.tp$abundance.sample.tp = abundance.sample.tp$abundance.sample.tp/length(tp$lagoslakeid)

## create dataframes for population, tn and tp

population = as.data.frame(cbind(area$area, depth$depth,  wsala$wsala, connectivity$connectivity, abundance$abundance))
names(population) = c("area", "depth", "wsala", "connectivity", "abundance")
population$state = area$state
population = population[c(1:13,15:18),]
population = droplevels(population)
order = order(-population$abundance)
population = population[order(-population$abundance),]

sample.tn = as.data.frame(cbind(area.sample.tn$area, depth.sample.tn$depth,  wsala.sample.tn$wsala, connectivity.sample.tn$connectivity, abundance.sample.tn$abundance))
names(sample.tn) = c("area", "depth", "wsala", "connectivity", "abundance")
sample.tn$state = area.sample.tn$state
sample.tn[18,5:6] = c(0, "New Jersey")
sample.tn$abundance = as.numeric(sample.tn$abundance)
sample.tn = sample.tn[c(1:12,14:18),]
sample.tn = droplevels(sample.tn)
sample.tn = sample.tn[order(sample.tn$state),]
sample.tn = sample.tn[order, ]

sample.tp = as.data.frame(cbind(area.sample.tp$area, depth.sample.tp$depth,  wsala.sample.tp$wsala, connectivity.sample.tp$connectivity, abundance.sample.tp$abundance))
names(sample.tp) = c("area", "depth", "wsala", "connectivity", "abundance")
sample.tp$state = area.sample.tp$state
sample.tp[17,5:6] = c(0, "New Jersey")
sample.tp[18,5:6] = c(0, "Ohio")
sample.tp$abundance = as.numeric(sample.tp$abundance)
sample.tp = sample.tp[c(1:11,13:18),]
sample.tp = droplevels(sample.tp)
sample.tp = sample.tp[order(sample.tp$state),]


## now create a dotplot that shows comparison of these stats
library(Hmisc)
plot.new()

pdf("Prop_state_dotplot.pdf")
par(mfrow=c(1,2), cex = 1, oma = c(1,5,2,1))
dotchart2(population$abundance, labels = population$state,dotsize = 2, xlim = c(-.07,.4), xlab = "Proportion of Lakes", bty= "L",width.factor = .2)
dotchart2(sample.tn$abundance,  dotsize = 2, add = TRUE, col = col.tn)
dotchart2(sample.tp$abundance,  dotsize = 2, add = TRUE, col = col.tp)


dotchart2(population$connectivity, labels = "", xlim = c(-.07,.6), dotsize = 2, xlab = "Proportion Seepage Lakes", mar = c(5,3,4,2))
dotchart2(sample.tn$connectivity, add = TRUE, col = col.tn, dotsize = 2)
dotchart2(sample.tp$connectivity, add = TRUE, col = col.tp, dotsize = 2)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend(-.5,.85, legend = c("Population", "TN Sample", "TP Sample"), 
       pch = 16, pt.cex = 2, bty = "n", col = c("black", col.tn, col.tp), cex =1, horiz = TRUE, text.width = c(0,.3,.3))
dev.off()

pdf("Morphology_state_dotplot.pdf")
par(mfrow=c(1,3), cex = 1, oma = c(0,5,3,3))
par(mar=c(5,0,0,0))


dotchart2(log10(population$area), labels = population$state,dotsize = 2, xlim = c(0,6), xlab = "log Area (ha)", bty= "L", width.factor = .2)
dotchart2(log10(sample.tn$area),  dotsize = 2, add = TRUE, col = col.tn)
dotchart2(log10(sample.tp$area),  dotsize = 2, add = TRUE, col = col.tp)

dotchart2(log10(population$depth), labels = "", xlim = c(0,2.1), dotsize = 2, xlab = "log Max Depth (m)")
dotchart2(log10(sample.tn$depth), add = TRUE, col = col.tn, dotsize = 2)
dotchart2(log10(sample.tp$depth), add = TRUE, col = col.tp, dotsize = 2)

dotchart2(log10(sample.tp$wsala), col = col.tp, xlim = c(0,3.5), labels = "",  dotsize = 2, xlab = "log WS:Lake Area", mar = c(0,0,0,4))
dotchart2(log10(population$wsala), add = TRUE, dotsize = 2)
dotchart2(log10(sample.tn$wsala), add = TRUE, col = col.tn, dotsize = 2)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend(-.5,1, legend = c("Population", "TN Sample", "TP Sample"), 
       pch = 16, pt.cex = 2, bty = "n", col = c("black", col.tn, col.tp), cex =1, horiz = TRUE, text.width = c(0,.3,.3))
dev.off()

## Make random plots of raw data from lakes with significant change

#randomly select 6 lakes with significant TN or TP trends
lakes.sig.tn = sample(1:length(which(change.db.tn$tn.change != "no change")), 6)
lakes.sig.tn = as.character(change.db.tn$lagoslakeid[change.db.tn$tn.change != "no change"][lakes.sig.tn])

lakes.sig.tp = sample(1:length(which(change.db.tp$tp.change != "no change")), 6)
lakes.sig.tp = as.character(change.db.tp$lagoslakeid[change.db.tp$tp.change != "no change"][lakes.sig.tp])

#no plot raw data from each of these lakes

pdf("TN_random_change.pdf", height = 5, width = 12)
par(mfrow = c(2,3), oma = c(0,0,0,0), mar = c(2,3,2,1))
for (i in 1:length(lakes.sig.tn)){
  plot(data.tn$tn_umol[data.tn$lagoslakeid == lakes.sig.tn[i]]~data.tn$sampleyear_cor[data.tn$lagoslakeid == lakes.sig.tn[i]],
       main = paste(lakes.sig.tn[i]), 
       #xlab = "Year (post 1990)", 
       #ylab = "TN (umol)", 
       #cex.lab = 2,
       ylab = "",
       xlab = "",
       cex.axis = 1.5, 
       cex = 1.8, 
       pch = 16)
}
dev.off()

pdf("TP_random_change.pdf", height= 5, width = 12)
par(mfrow = c(2,3), oma = c(0,0,0,0), mar = c(2,3,2,1))
for (i in 1:length(lakes.sig.tp)){
  plot(data.tp$tp_umol[data.tp$lagoslakeid == lakes.sig.tp[i]]~data.tp$sampleyear_cor[data.tp$lagoslakeid == lakes.sig.tp[i]],
       main = paste(lakes.sig.tp[i]), 
       #xlab = "Year (post 1990)", 
       #ylab = "TP (umol)",
       xlab = "", 
       ylab = "",
       cex.lab = 2,
       cex.axis = 1.5, 
       cex = 1.8, 
       pch = 16)
}
dev.off()

#find lakes on map
par(mar=c(0,0,0,0))
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE, col = "lightgray")
points(change.db.tp$nhd_long,change.db.tp$nhd_lat, 
       pch = 16, cex = .8, col = "darkgray")
points(change.db.tp$nhd_long[change.db.tp$lagoslakeid == 8294],change.db.tp$nhd_lat[change.db.tp$lagoslakeid == 8294], 
       pch = 16, cex = .8, col = "red") 

###############################################
## create figures showing change in TN and TP from after 1990
## to change in TN and TP after 2000
pdf("slopes_2007_comparison.pdf", height = 8, width = 12)
par(mfrow = c(1,2), mar=c(5,5,0,0), oma = c(0,4,1,1))
plot(change.db.tp$tp.slopes.07 ~ change.db.tp$tp.slopes, 
     pch = 1,  cex = 1.5, col = "gray47", xlab = "% Change per year > 1990", 
     ylab = "% Change per year > 2007", cex.lab = 1.5, cex.axis = 1.2)
points(change.db.tp$tp.slopes.07[change.db.tp$tp.change == "negative"] ~ change.db.tp$tp.slopes[change.db.tp$tp.change == "negative"], 
     pch = 21, bg = col.tp, cex = 1.5)
points(change.db.tp$tp.slopes.07[change.db.tp$tp.change == "positive"] ~ change.db.tp$tp.slopes[change.db.tp$tp.change == "positive"], 
       pch = 21, bg = col.both, cex = 1.5)
text(x = -.1, y = .2, "1990: -0.02% per year \n2007: -0.16% per year", pos = 4, cex = 1.5)
legend("bottomright", legend = c("TN Not Changing", "TN Increasing", "TN Decreasing"), pch = c(1,21,21), pt.bg = c(NA, col.both, col.tp), pt.cex = 1.5, cex = 1.2)
abline(0,1)

plot(change.db.tn$tn.slopes.07 ~ change.db.tn$tn.slopes, 
     pch = 1,  cex = 1.5, col = "gray47", xlab = "% Change per year > 1990", 
     ylab = "", cex.lab = 1.5, cex.axis = 1.2)
points(change.db.tn$tn.slopes.07[change.db.tn$tn.change == "negative"] ~ change.db.tn$tn.slopes[change.db.tn$tn.change == "negative"], 
       pch = 21, bg = col.tn, cex = 1.5)
points(change.db.tn$tn.slopes.07[change.db.tn$tn.change == "positive"] ~ change.db.tn$tn.slopes[change.db.tn$tn.change == "positive"], 
       pch = 21, bg = col.both, cex = 1.5)
text(x = -.06, y = .17, "1990: -0.67% per year \n2007: 1.4% per year", pos = 4, cex = 1.5)
legend("bottomright", legend = c("TN Not Changing", "TN Increasing", "TN Decreasing"), pch = c(1,21,21), pt.bg = c(NA, col.both, col.tn), pt.cex = 1.5, cex = 1.2)
abline(0,1)
dev.off()

######################################################
## create a map that plots regional random effects
######################################################
require(maps)
require(maptools)
require(rgdal)
require(sp)
require(ggplot2)
library(colorspace)

setwd("C:/Users/Samantha/Dropbox/GEO-Shared2/MGD_Shapefile_Exports_to_map_in_R_May2014")
huc4 = readOGR(dsn ="/Users/Samantha/Dropbox/GEO-Shared2/MGD_Shapefile_Exports_to_map_in_R_May2014", layer = "HU4_simple_wgs1984")

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
par(mfcol=c(4,2), cex = 1)
par(mar = c(0,0,0,0))
plot(huc4,col=get.col.bins(huc4$tn_slope), lty = 1, lwd=1,border=TRUE,mar=c(0,0,3,1),oma=c(0,0,0,0))
plot(huc4[which(huc4$tn_sig==TRUE),], lty = 1, lwd=3,border=TRUE, add = TRUE)


#map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
#                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
#                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
#                                  "Vermont", "New Hampshire","Maine"), fill = FALSE, lwd=2,lty=3, add = TRUE)
plot(huc4,col=get.col.bins(huc4$tp_slope), lty = 1, lwd=1,border=TRUE,mar=c(1,1,1,1),oma=c(0,0,0,0))
plot(huc4[which(huc4$tp_sig==TRUE),], lty = 1, lwd=3,border=TRUE, add = TRUE)

plot(huc4,col=get.col.bins(huc4$tntp_slope), lty = 1, lwd=1,border=TRUE,mar=c(1,1,1,1),oma=c(0,0,0,0))
plot(huc4[which(huc4$tntp_sig==TRUE),], lty = 1, lwd=3,border=TRUE,  add = TRUE)

plot(huc4,col=get.col.bins(huc4$chl_slope), lty = 1, lwd=1,border=TRUE,mar=c(1,1,1,1))
plot(huc4[which(huc4$chl_sig==TRUE),], lty = 1, lwd=3,border=TRUE,  add = TRUE)


######################################################
## create a map that plots lake random effects
######################################################

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
  dat = merge(dat, lake.info[,c("lagoslakeid", "nhd_lat", "nhd_long")], by = "lagoslakeid", all.x = TRUE)
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

# make a contour plot of x = tp, y = tn and z = chl. Basically, show response of 
# chlorophyll to changes in tp and tn

dat = change.db.all[complete.cases(change.db.all), ]
dat = data.frame(x = dat$tp_coef_mean*100, y = dat$tn_coef_mean*100, z = dat$chl_coef_mean*100)

require(akima)

dat.m = interp(x = dat$x, y = dat$y, z = dat$z, linear = TRUE, 
               extrap = FALSE, duplicate = "mean")

# Create contour plot

png("Nutrients_Chl_contour.png", height = 1200, width = 1200, pointsize = 20)
par(mar=c(5,5,3,1),cex = 1.2, cex.lab = 1.5, cex.axis = 1.2)
filled.contour(x = dat.m$x,
               y = dat.m$y, 
               z = dat.m$z,
               levels = c(-10,-4,-3,-2,-1,0,1,2,3,4,10),
               col = rev(brewer.pal(10, name = "RdBu")),
               #color.palette = colorRampPalette(c(rgb(5,48,97,max=255), rgb(255,255,255,max=255),rgb(103,0,31,max=255))), 
               xlab = "% TP Change Per Year", 
               ylab = "% TN Change Per Year", 
               key.title = title(main = paste("% Chl Change per Year"), cex.main = 1))
lines(x = c(-6.45, 3.2), y = c(0,0),  lwd = 4, lty = 2)
lines(x = c(-1.24,-1.24), y = c(-4.09,2.485),  lwd = 4, lty = 2)
text(labels = "n = 50", x = -6, y = 2.2, cex = 2, adj = 0)
text(labels = "n = 91", x = -.9, y = 2.2, cex = 2, adj = 0)
text(labels = "n = 195", x = 1.6, y = -3.8, cex = 2, adj = 0)
text(labels = "n = 307", x = -6, y = -3.8, cex = 2, adj = 0)

dev.off()

########################
# filling in summary tables
##########################

## extract model coefficients
change.db.tntp[[4]][2,2] - (qnorm(.975)*change.db.tntp[[4]][2,4])
change.db.tntp[[4]][2,2] + (qnorm(.975)*change.db.tntp[[4]][2,4])

##########################
# create cumulative distributions of start and end concentrations
##########################

new.tn = unique(data.tn[,c("lagoslakeid", "hu4_zoneid")])
new.tn = rbind(new.tn, new.tn)
new.tn$sampleyear_cor[1:833] = 0
new.tn$sampleyear_cor[834:1666] = 21
new.tn$sampleyear_cor = as.integer(new.tn$sampleyear_cor)


new.predict = predict(tn.3fr23, newdata = new.tn)
new.tn = unique(data.tn[,c("lagoslakeid", "hu4_zoneid")])
new.tn$pred_1990 = new.predict[c(1:833)]
new.tn$pred_2011 = new.predict[c(834:1666)]


# simulate starting and ending concentrations in 1990 and 2011, respectively
mySumm <- function(.) {
  predict(., newdata = new.tn, re.form = NULL)
}



sumBoot <- function(merBoot) {
  return(
    data.frame(fit = apply(merBoot$t, 2, function(x) mean(x, na.rm=TRUE)),
               lwr = apply(merBoot$t, 2, function(x) mean(x,na.rm=TRUE) - (qnorm(.95)*sd(x))),
               upr = apply(merBoot$t, 2, function(x) mean(x,na.rm=TRUE) + (qnorm(.95)*sd(x)))
  )
  )
}

PI.boot1.time <- system.time(
  boot1 <- lme4::bootMer(tn.3fr23, mySumm, nsim=100, use.u=FALSE, type="parametric")
)

PI.boot1 <- sumBoot(boot1)

mySumm <- function(.) { s <- sigma(.)
c(beta =getME(., "beta"), sigma = s, sig01 = unname(s * getME(., "theta"))) 
}

system.time(boo01 <- bootMer(tn.3fr23, mySumm, nsim = 100))

tn.compare = data.frame(fit.2011 = PI.boot1$fit[1:833], lwr.2011 = PI.boot1$lwr[1:833], upr.2011 = PI.boot1$upr[1:833],
                        fit.1990 = PI.boot1$fit[834:1666], lwr.1990 = PI.boot1$lwr[834:1666], upr.1990 = PI.boot1$upr[834:1666])
tn.2011 = PI.boot1

tn.2011$lagoslakeid = new.tn$lagoslakeid
tn.2011$hu4_zoneid = new.tn$hu4_zoneid
test = change.db.tn[[1]][change.db.tn[[1]]$term == "(Intercept)", ]
test = test[, c(3,11,13,14)]
tn.2011 = merge(tn.2011, test, by = "lagoslakeid")
tn.2011$diff = tn.2011$fit - tn.2011$coef_mean 

test1 =  change.db.tn[[1]][change.db.tn[[1]]$term == "(Intercept)", ]
test1 = test1[,c("lagoslakeid", "coef_mean")]
test2 =  change.db.tn[[1]][change.db.tn[[1]]$term == "sampleyear_cor", ]
test2 = test2[,c("lagoslakeid", "coef_mean")]

test3 = merge(test1, test2, by = "lagoslakeid")
# show cumulative distributions of concentration in those time points

test = lmer(log(tn_umol) ~ sampleyear_cor + (sampleyear_cor||lagoslakeid) + (sampleyear_cor||hu4_zoneid), data = data.tn, REML=TRUE)

PI.time <- system.time(
  PI <- predictInterval(merMod = test, newdata = data.tn, 
                        level = 0.90, n.sims = 1000,
                        stat = "median", type="linear.prediction",
                        include.resid.var = TRUE)
)


plot(ecdf(tn.compare$fit.1990), 
     ylab = "Cumulative Proportion", 
     xlab = "TN (umol/L)", 
     col = "blue", 
     cex = .5, 
     cex.axis = 1.3, 
     cex.main = 1.7)

plot(ecdf(tn.compare$fit.2011), add = TRUE, cex = 0.5, col = "deeppink")

## visualize raw data (nutrient ~ year) by region and by lake
pdf("tn_lakebyregion.pdf")
temp = data.tn[with(data.tn, order(lagoslakeid, sampleyear_cor)), ]
xyplot(log(tn_umol)~sampleyear_cor | factor(hu4_zoneid), data=temp, pch=19,
       xlab = "Year since 1990", ylab = "log(TN umol)", layout=c(10,5),type="l", groups = lagoslakeid)
dev.off()

pdf("tp_lakebyregion.pdf")
temp = data.tp[with(data.tp, order(lagoslakeid, sampleyear_cor)), ]

xyplot(log(tp_umol)~sampleyear_cor | factor(hu4_zoneid), data=temp, pch=19,
       xlab = "Year since 1990", ylab = "log(TP umol)", layout=c(10,6),type="l", groups = lagoslakeid)
dev.off()

pdf("tntp_bylakeregion.pdf")
temp = data.tntp[with(data.tntp, order(lagoslakeid, sampleyear_cor)), ]

xyplot(log(tn_tp_umol)~sampleyear_cor | factor(hu4_zoneid), data=temp, pch=19,
       xlab = "Year since 1990", ylab = "log(TN:TP molar)", layout=c(9,5),type="l", groups = lagoslakeid)
dev.off()

pdf("chl_lakebyregion.pdf")
temp = data.chl[with(data.chl, order(lagoslakeid, sampleyear_cor)), ]
temp = temp[temp$hu4_zoneid != "OUT_OF_HU4",]

xyplot(log(chl)~sampleyear_cor | factor(hu4_zoneid), data=temp, pch=19,
       xlab = "Year since 1990", ylab = "log(Chl umol)", layout=c(9,7),type="l", groups = lagoslakeid)
dev.off()

# create plot of nutrients vs model parameters (n obs/lake, median year, etc)

#########################
## random forest results
#########################

# create violin plots of important predictors identified by the rf

library(ggplot2)

pdf("tntp_top_var.pdf")
#TP vs ppt change
ggplot(dat.keep, aes(x=tp_change, y=ppt_change, fill=tp_change)) + 
  geom_violin()+
  theme_classic()+
  stat_summary(fun.ymin = "tenth", 
               fun.y = "median",
               fun.ymax = "ninetieth",
               geom="pointrange", shape = 3) +
  scale_fill_manual(values = c(rgb(67,147,195,max=255),rgb(214,96,77,max=255), "gray"))+ 
  labs(x="TP Change", y="Regional Precipitation Change")+
  guides(fill=FALSE)+
  theme(axis.title=element_text(size = rel(2))) +
  theme(axis.text=element_text(size = rel(1.6))) +
  theme(axis.text.y=element_text(margin=margin(2,2,3,6,"pt")))+
  theme(axis.text.x=element_text(margin=margin(2,2,4,3,"pt")))

#TN vs regional TN dep 1990
ggplot(dat.keep, aes(x=tn_change, y=hu4_dep_totaln_1990_mean, fill=tn_change)) + 
  geom_violin()+
  theme_classic()+
  stat_summary(fun.ymin = "tenth", 
               fun.y = "median",
               fun.ymax = "ninetieth",
               geom="pointrange", shape = 3) +
  scale_fill_manual(values = c(rgb(67,147,195,max=255),rgb(214,96,77,max=255), "gray"))+ 
  labs(x="TN Change", y="Regional TN Deposition 1990")+
  guides(fill=FALSE)+
  theme(axis.title=element_text(size = rel(2))) +
  theme(axis.text=element_text(size = rel(1.6))) +
  theme(axis.text.y=element_text(margin=margin(2,2,3,6,"pt")))+
  theme(axis.text.x=element_text(margin=margin(2,2,4,3,"pt")))

#TN:TP vs regional road density
ggplot(dat.keep, aes(x=tntp_change, y=hu4_roaddensity_density_mperha, fill=tntp_change)) + 
  geom_violin()+
  theme_classic()+
  stat_summary(fun.ymin = "tenth", 
               fun.y = "median",
               fun.ymax = "ninetieth",
               geom="pointrange", shape = 3) +
  scale_fill_manual(values = c(rgb(67,147,195,max=255),rgb(214,96,77,max=255), "gray"))+ 
  labs(x="TN:TP Change", y="Regional Road Density")+
  guides(fill=FALSE)+
  theme(axis.title=element_text(size = rel(2))) +
  theme(axis.text=element_text(size = rel(1.6))) +
  theme(axis.text.y=element_text(margin=margin(2,2,3,6,"pt")))+
  theme(axis.text.x=element_text(margin=margin(2,2,4,3,"pt")))

#Chl vs Regional mean temp in 2011
ggplot(dat.keep, aes(x=chl_change, y=tmean.2011, fill=chl_change)) + 
  geom_violin()+
  theme_classic()+
  stat_summary(fun.ymin = "tenth", 
               fun.y = "median",
               fun.ymax = "ninetieth",
               geom="pointrange", shape = 3) +
  scale_fill_manual(values = c(rgb(67,147,195,max=255),rgb(214,96,77,max=255), "gray"))+ 
  labs(x="Chl Change", y="Regional Mean Temperature in 2011")+
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