# GIF for press release
# create stacked histogram by nutrient and year
library(ggplot2)
library(ggjoy)
library(animation)
library(hrbrthemes)
library(ggthemes)
library(zoo)
library(dplyr)
library(maps)
library(mapproj)
library(ggplot2)
library(gganimate)

# get lake info
infile2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/333/3/cc35382bc48fc688750d0c358167f3e1" 
infile2 <- sub("^https","http",infile2) 
lake.info <-read.csv(infile2,header=F, skip=1, sep=",", 
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

# get lake nutrient info
infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/333/3/e955b964c44276632edd9f3629022077" 
infile1 <- sub("^https","http",infile1) 
all.nut.data <-read.csv(infile1, header=F, skip=1, sep=",",  
                        col.names=c("lagoslakeid",     
                                    "sampleyear",     
                                    "value",     
                                    "variable"), check.names=TRUE)
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(all.nut.data$lagoslakeid)!="factor") all.nut.data$lagoslakeid<- as.factor(all.nut.data$lagoslakeid)
if (class(all.nut.data$value)=="factor") all.nut.data$value <-as.numeric(levels(all.nut.data$value))[as.integer(all.nut.data$value) ]
if (class(all.nut.data$variable)!="factor") all.nut.data$variable<- as.factor(all.nut.data$variable)

all.nut.data$sampleyear_cor = all.nut.data$sampleyear - 1990

# create individual dataframes that represent each nutrient

data.tn = subset(all.nut.data, all.nut.data$variable == "tn_umol")
data.tp = subset(all.nut.data, all.nut.data$variable == "tp_umol")
data.tntp = subset(all.nut.data, all.nut.data$variable == "tn_tp_umol")
data.chl = subset(all.nut.data, all.nut.data$variable == "chl_ugL")

tp.chl = merge(data.chl, data.tp, by = 'lagoslakeid', all.x = TRUE)
tp.chl = tp.chl[,c(1,2,3,7)]
names(tp.chl) = c('lagoslakeid', 'sampleyear', 'Chl', 'TP')
tp.chl = tp.chl[!is.na(tp.chl$Chl)&!is.na(tp.chl$TP), ]

names(data.chl)

# create a dataframe of all chl lakes w/every year - carry last observation forward

n.lakes <- length(unique(data.chl$lagoslakeid))
lakes <- as.character(unique(data.chl$lagoslakeid))
years <- 1990:2011
chl.all.y <- data.frame(sampleyear = rep(years, n.lakes), 
                       lagoslakeid = rep(lakes, each = length(years)))

chl.all.fill <- merge(chl.all.y, data.chl[, c('lagoslakeid', 'sampleyear', 'value')], all.x = TRUE)
chl.all.fill <- droplevels.data.frame(chl.all.fill)
chl.all.fill <- as_tibble(chl.all.fill)

chl.all.fill <- chl.all.fill %>%
  arrange(lagoslakeid, sampleyear) 


chl.filled <- do.call(rbind, by(chl.all.fill, chl.all.fill$lagoslakeid, na.locf))

# make map and animate through time

states <- map_data('state')
study.site <- subset(states, region %in% c('minnesota', 'michigan', 'wisconsin', 'iowa', 'missouri', 'illinois', 
                                           'indiana', 'ohio', 'pennsylvania', 'new york', 'new jersey', 'maine', 'new hampshire', 'connecticut',
                                           'delaware', 'rhode island', 'massachusetts'))

chl.filled <- merge(chl.filled, lake.info[,c('lagoslakeid', 'nhd_lat', 'nhd_long')], by = 'lagoslakeid', all.x = TRUE)
chl.filled.c <- chl.filled[complete.cases(chl.filled),]
chl.filled.c$logvalue <- log10(as.numeric(chl.filled.c$value))


plot.maps.list <- list()
for (i in 1990:2011) {
  temp.dat <- chl.filled.c %>%
    filter(sampleyear == i)
  
  plot.maps.list[[i-1989]] <- plot.map(temp.dat)
}
plot.map <- function(temp.dat) {
ggplot() +
  geom_polygon(data = study.site, aes(long, lat, group = group),fill = 'white', color = 'darkgray') +
  geom_point(data = temp.dat, aes_string('nhd_long', 'nhd_lat', size = 'logvalue'), color = rgb(100,100,100,100, max = 255)) +
  scale_size_continuous(range = c(4, 10)) +
  theme_map()

}

p <- ggplot() +
  geom_polygon(data = study.site, aes(long, lat, group = group),fill = 'white', color = 'darkgray') +
  geom_point(data = chl.filled.c, size = 2, aes_string('nhd_long', 'nhd_lat', color = 'logvalue', frame = 'sampleyear')) +
  scale_color_gradient2(low = 'blue', mid = 'white', high = 'green', midpoint = 0.7) +
  coord_map('albers', lat0 = 41, lat1 = 50) +
  theme_map()

gganimate(p)

ani.options(interval = 0.5)
saveGIF(
  for (i in 1:length(plot.maps.list)) {
    print(plot.maps.list[[i]])
  }, movie.name = 'Chl_map_animation.gif'  
)
  scale_fill_brewer(type = 'div', palette = 'Spectral', direction = -1, name = var.legend) +
  scale_color_brewer(type = 'div', palette = 'Spectral', direction = -1, guide = F) +
  coord_map('albers', lat0 = 41, lat1 = 50) +
  #facet_wrap(~timescale, scales = 'fixed', nrow = 2, ncol = 2)
  
  theme(legend.position = c(0, 0), 
        legend.justification = c(0, 0),
        legend.direction = 'horizontal',
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  guides(fill = guide_legend(title.position = 'top', title.hjust = 0.5, label.position = 'bottom', nrow = 1)) +
  labs(title = season)

# try nitrogen first as only one that showed trend.
setwd("~/ts_gif")

plot.list <- list()
for (i in 1:length(1990:2011)){
  set.year <- 1989 + i
  set.missing.year <- 2011-set.year
  tn <- subset(data.tn, sampleyear < 2012)
  # if (i < 20) {
  # tn <- subset(data.tn, sampleyear <= set.year|sampleyear > (set.year+2))
  # nrows <- nrow(tn)
  # tn[(nrows+1):(nrows+2), 'sampleyear'] <- c((set.year+1):(set.year+2))
  # tn[(nrows+1):(nrows+2), 'value'] <- rep(50, 2)
  # }
  # if (i == 20|i == 21) {
  # tn <- subset(data.tn, sampleyear <= set.year)
  # nrows <- nrow(tn)
  # tn[(nrows+1):(nrows+(22-i)), 'sampleyear'] <- c((set.year+1):2011)
  # tn[(nrows+1):(nrows+(22-i)), 'value'] <- rep(50, 22-i)
  # }
  
  tn$sampleyear <- as.factor(tn$sampleyear)
  tn$tn_umol <- log10(tn$value)
  plot.list[[i]] <- ggplot(tn, aes(x = tn_umol, y = sampleyear, fill = sampleyear, color = sampleyear)) + 
    geom_joy2(scale = 3, rel_min_height = 0.01, panel_scaling = FALSE) +
    scale_y_discrete(name="", limits = rev(levels(tn$sampleyear)), expand = c(0.01, 0)) +
    scale_x_continuous(limits = c(0.5,3.3), expand = c(0.01, 0), breaks = c(1,2,3), labels = c('10', '100', '1000')) +
    scale_fill_cyclical(values = c(rep('gray', i), rep(NA, 22-i))) +
    scale_color_cyclical(values = c(rep('black', i), rep(NA, 22-i))) +
    theme_ipsum(grid = T) +
    theme(axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          strip.text.y = element_text(angle = 180, hjust = 1),
          panel.grid.major.y = element_blank()) +
    labs(title='Lake nitrogen concentrations',
         subtitle='Measurements from 833 lakes in the Midwest and Northeast United States',
         x = 'Total Nitrogen (micromolar)')
}

plot.list <- list()
for (i in 1:length(1990:2011)){
  set.year <- 1989 + i
  set.missing.year <- 2011-set.year
  chl <- subset(data.chl, sampleyear < 2012)
  # if (i < 20) {
  # chl <- subset(data.chl, sampleyear <= set.year|sampleyear > (set.year+2))
  # nrows <- nrow(chl)
  # chl[(nrows+1):(nrows+2), 'sampleyear'] <- c((set.year+1):(set.year+2))
  # chl[(nrows+1):(nrows+2), 'value'] <- rep(50, 2)
  # }
  # if (i == 20|i == 21) {
  # chl <- subset(data.chl, sampleyear <= set.year)
  # nrows <- nrow(chl)
  # chl[(nrows+1):(nrows+(22-i)), 'sampleyear'] <- c((set.year+1):2011)
  # chl[(nrows+1):(nrows+(22-i)), 'value'] <- rep(50, 22-i)
  # }
  
  chl$sampleyear <- as.factor(chl$sampleyear)
  chl$chl_mgL <- log10(chl$value)
  plot.list[[i]] <- ggplot(chl, aes(x = chl_mgL, y = sampleyear, fill = sampleyear, color = sampleyear)) + 
    geom_joy2(scale = 3, rel_min_height = 0.01, panel_scaling = FALSE) +
    scale_y_discrete(name="", limits = rev(levels(chl$sampleyear)), expand = c(0.01, 0)) +
    scale_x_continuous(limits = c(-1,3), expand = c(0.01, 0), breaks = c(-1,0,1,2,3), labels = c('0.1','1','10', '100', '1000')) +
    scale_fill_cyclical(values = c(rep('gray', i), rep(NA, 22-i))) +
    scale_color_cyclical(values = c(rep('black', i), rep(NA, 22-i))) +
    theme_ipsum(grid = T) +
    theme(axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          strip.text.y = element_text(angle = 180, hjust = 1),
          panel.grid.major.y = element_blank()) +
    labs(title='Lake Chlorophyll Concentrations',
         subtitle='Measurements from 2239 lakes in the Midwest and Northeast United States',
         x = 'Chlorophyll (milligrams per liter)')
}


ani.options(interval = 0.5)
saveGIF(
  for (i in 1:length(plot.list)) {
    print(plot.list[[i]])
  }, movie.name = 'Chl_animation.gif'  
)

# create a data subset of lakes with the highest and lowest chlorophyll changes

lows <- c(6494, 6560,6480)
highs <- c(1919, 3875, 52808)

# get all data with those lake ids

extremes <- subset(all.nut.data, lagoslakeid %in% c(lows, highs))
extremes <- subset(extremes, variable == 'chl_ugL'|variable == 'tp_umol')
extreme.chl <- subset(extremes, variable == 'chl_ugL')
names(extreme.chl)[3] <- 'Chl'
extreme.P <- subset(extremes, variable == 'tp_umol')
names(extreme.P)[3] <- 'TP'
extremes.m <- merge(extreme.chl, extreme.P, c('lagoslakeid', 'sampleyear'), all.x = TRUE)
extremes.m$Chl <- log10(extremes.m$Chl)
extremes.m$TP <- log10(extremes.m$TP)
extremes.m$change <- NA
extremes.m$change[extremes.m$lagoslakeid %in% lows] <- 'decreasing'
extremes.m$change[extremes.m$lagoslakeid %in% highs] <- 'increasing'

plot.extremes <- function(set.year){
  g <- ggplot(extremes.m, aes(x = sampleyear, y = Chl, colour = factor(change))) +
  geom_line(size = 1.3, aes(group = factor(lagoslakeid))) +
  geom_point(size = 2.5, aes(shape = factor(lagoslakeid))) +
  geom_rect(aes(xmin = set.year, xmax = 2013.5, ymin = -1.5, ymax = 2.3), fill = 'white', col = 'white') +
  theme_ipsum(grid = F, axis_title_size = 16, axis_title_just = "c") +
  scale_color_manual(values = c('blue', 'red')) +
  scale_shape_manual(values = c(15:17, 15:17)) + 
  scale_y_continuous(limits = c(-1.5,2.5), breaks = c(-1,0,1,2), labels = c('0.1','1','10','100')) +
  theme(axis.ticks.y=element_blank(),
        strip.text.y = element_text(angle = 180, hjust = 1),
        panel.grid.major.y = element_blank(),
        legend.position = 'none') +
  labs(title='Extreme changes in chlorophyll 1990-2011',
       subtitle='Though on average there was no trend across the population of lakes, \nsome individual lakes had large increases (>9% increase per year; red \nlines) or decreases (>7% decrease per year; blue lines) in chlorophyll.',
       x = '',
       y = 'Chlorophyll (ug/L)')
  return(g)
}

saveGIF(
  for (i in 1:length(1990:2011)){
    set.year <- 1989 + (i + 0.11)
    print(plot.extremes(set.year))
  }, movie.name = 'extremes.gif'
)

# Did algae respond to changes in nutrients
# Again, we look at the lakes with extreme increases and descreases
# in algae. The plants of some lakes appeared to be tracking changes in
# plant nutrients. In some lakes, (e.g., red lines) - this did not seem to 
# be true. Our study found that lakes experiencing high air temperatures
# in 1990 were more likely to be increasing in algae.

# first, ID lakes that have a lot of chl observations, 
# then cross reference lakes that have a lot of P data
n.chl <- summary(as.factor(data.chl$lagoslakeid))
n.chl <- n.chl[n.chl>20]
n.chl <- names(n.chl)

n.tp <- summary(as.factor(data.tp$lagoslakeid))
n.tp <- n.tp[n.tp>20]
n.tp <- names(n.tp)

n.chl.tp <- n.chl[which(n.chl %in% n.tp)]
n.chl.tp <- n.chl.tp[-32]

lakes.high.n <- subset(change.db.all, lagoslakeid %in% n.chl.tp)
# keep lakes showing a variety of patterns in Chl,
# including increasing (2), decreasing (2), not changing (2)
lakes.in <- c(113702, 99307, 133500, 6218, 6404, 6494)
lakes.bio <- subset(all.nut.data, lagoslakeid %in% lakes.in)
lakes.bio.chl <- subset(lakes.bio, variable == 'chl_ugL')
lakes.bio.tp <- subset(lakes.bio, variable == 'tp_umol')
lakes.bio <- merge(lakes.bio.chl, lakes.bio.tp, by = c('lagoslakeid', 'sampleyear'), all.x = TRUE)
names(lakes.bio)[c(3,6)] <- c('Chl', 'TP')

plot.bio <- function(dat) {
  ggplot(dat, aes(x = TP, y = Chl)) +
  geom_path(size = 1.3, aes(group = factor(lagoslakeid), color = sampleyear)) +
  #geom_point(size = 2.5, aes(shape = factor(lagoslakeid))) +
  theme_ipsum(grid = F, axis_title_size = 16, axis_title_just = "c") +
  facet_wrap(~factor(lagoslakeid), nrow = 2, scales = 'free', dir="v") +
  scale_colour_gradient2(low = "yellow", mid = "orange" , high = "red", 
                           midpoint=2000) +
  #scale_shape_manual(values = c(15:17, 15:17)) + 
  scale_y_continuous(breaks = c(0,1,2), labels = c('1','10','100')) +
  scale_x_continuous(breaks = c(-1,0,1), labels = c('0.1','1','10')) +
  theme(axis.ticks.y=element_blank(),
        strip.text.y = element_text(angle = 180, hjust = 1),
        panel.grid.major.y = element_blank(),
        legend.position = 'none') +
  labs(title='Are extreme changes in algae related to nutrients?',
       subtitle='It appeared that for some lakes, extreme changes in algae may have been 
caused by increases or decreases in nutrients (e.g. lakes with clear 
positive or negative correlation). However, in many cases, changes in 
chlorophyll were unrelated to nutrients. Our study found that lakes 
that experienced warmer temperatures in 1990 were more likely to be 
increasing in chlorophyll.',
       x = 'Phosphorus (uM)',
       y = 'Chlorophyll (ug/L)')
  
}
lakes.bio$Chl <- log10(lakes.bio$Chl)
lakes.bio$TP <- log10(lakes.bio$TP)
lakes.bio <- droplevels(lakes.bio)
lakes.bio$trend <- lakes.bio$lagoslakeid
change.db.all$chl_change[change.db.all$lagoslakeid %in% levels(lakes.bio$trend)]
levels(lakes.bio$trend) <- c('No Trend (-0.2% per year)', 'No Trend (-0.3% per year)', 'Increasing (4.3% per year)', 'Increasing (4.4% per year)', 'Decreasing (5.0% per year)', 'Decreasing (7.3% per year)')
saveGIF(
  for (i in 1:length(1990:2011)){
    dat <- subset(lakes.bio, sampleyear <= 1989 + i)
    print(plot.bio(dat))
  }, movie.name = 'lakes_chl_TP.gif'
)
order(c(extremes.m$lagoslakeid, extremes.m$sampleyear))
tn <- data.tn
tn <- subset(tn, sampleyear < 2012)
tn$sampleyear <- as.factor(tn$sampleyear)
tn$tn_umol <- log10(tn$tn_umol)
ggplot(tn, aes(x = tn_umol, y = sampleyear)) + 
  geom_joy(scale = 1)

# predict chl values through time and map them. 
predict.chl <- data.chl
predict.chl <- unique(data.chl[,'lagoslakeid'])
