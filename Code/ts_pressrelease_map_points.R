n.lakes <- length(unique(data.chl$lagoslakeid))
lakes <- unique(data.chl[,c('lagoslakeid', 'hu4_zoneid')])
years <- 1990:2011
chl.all.y <- data.frame(sampleyear = rep(years, n.lakes), 
                        lagoslakeid = rep(lakes$lagoslakeid, each = length(years)), 
                        hu4_zoneid = rep(lakes$hu4_zoneid, each = length(years)), 
                        chl_ugL = NA)
chl.all.y$sampleyear_cor = chl.all.y$sampleyear - 1990

chl.all.y$predicted <- predict(chl.3fr23, chl.all.y)

chl.all <- merge(chl.all.y, lake.info[,c('lagoslakeid', 'nhd_lat', 'nhd_long')], by = 'lagoslakeid', all.x = TRUE)
chl.all <- merge(chl.all, temp.chl, by = 'lagoslakeid', all.x = TRUE)

chl.all.summ <- as_tibble(chl.all) %>%
  group_by(lagoslakeid) %>%
  summarise(med.pred = median(predicted))

chl.all.summ <- merge(chl.all.summ, chl.all[,c('lagoslakeid', 'nhd_lat', 'nhd_long', 'chl_change')], by = 'lagoslakeid', all.x = TRUE)
chl.all.summ <- unique(chl.all.summ)
chl.all.change <- subset(chl.all, chl_change != 'no change')
chl.all.chage <- droplevels.data.frame(chl.all.change)
# instead of color by value, color by whether it is oligotrophic, mesotrophic, eutrophic
# or keep all lake gray, color those that are increasing(red), decreasing(blue)
par(mfrow = c(1,1))
p <- ggplot() +
  geom_polygon(data = study.site, aes(long, lat, group = group),fill = 'white', color = 'darkgray') +
  geom_point(data = subset(chl.all.summ, chl_change == 'no change'), show.legend = F,  color = rgb(200,200,200,51, max = 255), aes_string('nhd_long', 'nhd_lat', size = 'med.pred')) +
  geom_point(data = chl.all.change, alpha = 0.2, shape = 16, aes_string('nhd_long', 'nhd_lat', size = 'predicted', color = factor(chl.all.change$chl_change), frame = 'sampleyear')) +
  scale_color_manual(values = c('blue', 'red'), name = 'Chl Trend') +
  scale_size_continuous(range = c(1,7), breaks = c(-4.61,-2.303,0,2.303, 4.61), labels = c('0.01', '0.1', '1', '10', '100'), name = 'Chl Concentration (ug/L)') +
  coord_map('albers', lat0 = 41, lat1 = 50) +
  theme_map() +
  theme_ipsum() +
  guides(colour = guide_legend(override.aes = list(size = 7, alpha = 0.5))) +
  labs(subtitle = 'Modeled chlorophyll concentrations and\ntrends in the Midwest and Northeast U.S.') +
  theme(plot.title = element_text(size = 24, hjust = 1, vjust = -0.2),
        legend.position = c(0, -0.8), 
        legend.justification = c(0, -0.8), 
        legend.box = 'horizontal', 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 16), 
        plot.subtitle = element_text(size = 18, hjust = -0.5), 
        plot.margin = unit(c(1,4,1,1), 'lines'))
  

cols <- brewer.pal(9, 'BuGn')
ani.options(interval = 0.5)
gganimate(p, 'chl_predicted.gif')
gganimate(p)
chl.all.fill <- merge(chl.all.y, data.chl[, c('lagoslakeid', 'sampleyear', 'value')], all.x = TRUE)
chl.all.fill <- droplevels.data.frame(chl.all.fill)