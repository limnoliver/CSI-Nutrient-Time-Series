# save just TN and TP data

tp = year.means

# save tp data where we have 2007 and 2012 data

tp.sub = tp[tp$sampleyear == 2007 | tp$sampleyear == 2012, ]

tp.2006 = tp$tp_umol[tp$sampleyear == 2006]
id.2006 = tp$lagoslakeid[tp$sampleyear == 2006]
tp.06 = data.frame(tp_06 = tp.2006, lagoslakeid = id.2006)


tp.2007 = tp$tp_umol[tp$sampleyear == 2007]
id.2007 = tp$lagoslakeid[tp$sampleyear == 2007]
tp.07 = data.frame(tp_07 = tp.2007, lagoslakeid = id.2007)


tp.2011 = tp$tp_umol[tp$sampleyear == 2011]
id.2011 = tp$lagoslakeid[tp$sampleyear == 2011]
tp.11 = data.frame(tp_11 = tp.2011, lagoslakeid = id.2011)


tp.2012 = tp$tp_umol[tp$sampleyear == 2012]
id.2012 = tp$lagoslakeid[tp$sampleyear == 2012]
tp.12 = data.frame(tp_12 = tp.2012, lagoslakeid = id.2012)


# create data frame of 2007 and 2012 data

tp.07.12 = merge(tp.07, tp.12, by = "lagoslakeid", all.x = TRUE)
tp.07.12 = tp.07.12[complete.cases(tp.07.12),]
tp.07.11 = merge(tp.07, tp.11, by = "lagoslakeid", all.x = TRUE)
tp.07.11 = tp.07.11[complete.cases(tp.07.11),]
tp.06.11 = merge(tp.06, tp.11, by = "lagoslakeid", all.x = TRUE)
tp.06.11 = tp.06.11[complete.cases(tp.06.11),]


plot(log10(tp_12*31) ~ log10(tp_07*31), data = tp.07.12, 
     xaxt = "n", 
     yaxt = "n",
     xlab = "TP in 2007 (ug/L)", 
     ylab = "TP in 2012 (ug/L)", 
     xlim = c(0,2.5), 
     ylim = c(0,2.5))
axis(1, at = c(0,1,2), labels = c("1", "10", "100"))
axis(2, at = c(0,1,2), labels = c("1", "10", "100"))
abline(0,1, col = "red")
abline(v=1)

plot(tp_11 ~ tp_07, data = tp.07.11)
abline(0,1, col = "red")
abline(v=0.32)

plot(tp_11 ~ tp_06, data = tp.06.11)
abline(0,1, col = "red")
abline(v=0.32)

tp.07.11.low = subset(tp.07.11, tp.07.11$tp_07 < 0.33 | tp.07.11$tp_11 < 0.33)
plot(tp_11 ~ tp_07, data = tp.07.11.low)
abline(0,1, col = "red")

obs = summary(as.factor(tp.sub$lagoslakeid))
obs = data.frame(lagoslakeid = names(obs), 
                 obs = as.numeric(obs))
test = merge(test, obs, by = "lagoslakeid", all.x = TRUE)

test = subset(test, test$obs == 2)

#create a plot that shows year comparisons
# 07 to 12
# 07 to 11
# 06 to 11
# for each year comparison, show all data, and paired data

pdf("Cumulative_tp.pdf")
par(mfrow=c(3,2))
par(mar=c(1,1,2,2), oma = c(4,4,0,0))
plot(ecdf(log10(tp.07.12$tp_07*31)), 
     xaxt = "n", 
     xlab = "", 
     ylab = "", 
     col = "blue", 
     cex = .5, 
     main = "Paired Data", 
     cex.axis = 1.3, 
     cex.main = 1.7)
plot(ecdf(log10(tp.07.12$tp_12*31)), add = TRUE, cex = 0.5, col = "deeppink")
text(1, 0.8,"2007", col = "blue", cex = 2)   
text(1, 0.6,"2012", col = "deeppink", cex = 2)  
length(tp.07.12$tp_07)
text(2,.2,"n = 51", cex = 1.7)
axis(1, at = c(0,1,2,3), labels = c("1", "10", "100","1000"), cex.axis = 1.3)

plot(ecdf(log10(tp.2007*31)), 
     xaxt = "n", 
     xlab = "", 
     ylab = "", 
     col = "blue", 
     cex = .5, 
     main = "All Data", 
     cex.axis = 1.3, 
     cex.main = 1.7)
plot(ecdf(log10(tp.2012*31)),add = TRUE, cex = 0.5, col = "deeppink") 
axis(1, at = c(0,1,2,3), labels = c("1", "10", "100","1000"), 
     cex.axis = 1.3)
length(tp.2007) 
length(tp.2012)
text(2.5, .4, "n = 986", col = "blue", cex = 1.7)
text(2.5, .2, "n = 95", col = "deeppink", cex = 1.7)

plot(ecdf(log10(tp.07.11$tp_07*31)), 
     xaxt = "n", 
     xlab = "", 
     ylab = "Cumulative Proportion of Lakes", 
     col = "blue", 
     cex = .5, 
     main = "", 
     cex.lab = 2.5, 
     cex.axis = 1.3, 
     mar = c(2,6,2,2))
plot(ecdf(log10(tp.07.11$tp_11*31)), add = TRUE, cex = 0.5, col = "deeppink")

axis(1, at = c(0,1,2,3), labels = c("1", "10", "100","1000"), 
     cex.axis = 1.3)
length(tp.07.11$tp_07)
text(1.9, 0.2, "n = 146", cex = 1.7)

text(0.4, 0.8,"2007", col = "blue", cex = 2)   
text(0.4, 0.6,"2011", col = "deeppink", cex = 2) 

plot(ecdf(log10(tp.2007*31)), 
     xaxt = "n", 
     xlab = "", 
     ylab = "", 
     col = "blue", 
     cex = .5, 
     main = "", 
     cex.axis = 1.3)
plot(ecdf(log10(tp.2011*31)),add = TRUE, cex = 0.5, col = "deeppink") 
axis(1, at = c(0,1,2,3), labels = c("1", "10", "100","1000"), 
     cex.axis = 1.3)

length(tp.2007)
length(tp.2011)
text(2.5, .4, "n = 986", col = "blue", cex = 1.7)
text(2.5, .2, "n = 315", col = "deeppink", cex = 1.7)

plot(ecdf(log10(tp.06.11$tp_06*31)), 
     xaxt = "n", 
     xlab = "Total Phosphorus (ug/L)", 
     ylab = "", 
     col = "blue", 
     cex = .5, 
     main = "", 
     cex.lab = 2.5, 
     cex.axis = 1.3)
plot(ecdf(log10(tp.06.11$tp_11*31)), add = TRUE, cex = 0.5, col = "deeppink")

axis(1, at = c(0,1,2,3), labels = c("1", "10", "100","1000"), 
     cex.axis = 1.3)
text(0.8, 0.8,"2006", col = "blue", cex = 2)   
text(0.8, 0.6,"2011", col = "deeppink", cex = 2) 
length(tp.06.11$tp_06)
text(2, .2, "n = 147", cex = 1.7)

plot(ecdf(log10(tp.2006*31)), 
     xaxt = "n", 
     xlab = "Total Phosphorus (ug/L)", 
     ylab = "", 
     col = "blue", 
     cex = .5, 
     main = "", 
     cex.lab = 2.5, 
     cex.axis = 1.3)
plot(ecdf(log10(tp.2011*31)),add = TRUE, cex = 0.5, col = "deeppink") 
axis(1, at = c(0,1,2,3), labels = c("1", "10", "100","1000"), 
     cex.axis = 1.3)
length(tp.2006)
text(2.5, .4, "n = 749", col = "blue", cex = 1.7)
text(2.5, .2, "n = 315", col = "deeppink", cex = 1.7)

mtext("Total Phosphorus (ug/L)", side = 1, cex = 1.7, outer = TRUE, line = 2)
mtext("Cumulative Distribution", side = 2, cex = 1.7, outer = TRUE, line = 2)


dev.off()



axis(1, at = c(0,1,2,3), labels = c("1", "10", "100","1000"))






plot(ecdf(log10(tp.07.12$tp_12*31)), col = "red", add = TRUE, cex = 0.5)
legend(0, .8, col = c("green", "red"))

test <- ecdf(log10(tp.07.12$tp_12*31))
plot(test, verticals = TRUE, do.p = FALSE)
lines(as.list(environment(test)), col = "blue")


