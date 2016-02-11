## this code expands our lake selection criteria
## lakes must have an observation in each 5 year period
## to be included.

keep.5 = c()
keep.7 = c()
keep.10 = c()

for (i in 1:length(unique(year.means$lagoslakeid))) {
  lake = unique(year.means$lagoslakeid)[i]
  years = year.means$sampleyear[year.means$lagoslakeid == lake]
  test.5 = c(length(which(years %in% c(1990:1994)))>0,
           length(which(years %in% c(1995:1999)))>0,
           length(which(years %in% c(2000:2004)))>0,
           length(which(years %in% c(2005:2009)))>0)
  test.7 = c(length(which(years %in% c(1990:1996)))>0,
             length(which(years %in% c(1997:2003)))>0,
             length(which(years %in% c(2004:2010)))>0)  
  test.10 = c(length(which(years %in% c(1990:1999)))>0, 
              length(which(years %in% c(2000:2010)))>0)
  
  if (length(which(test.5 == FALSE)) > 0) {
    keep.5[i] = FALSE
  } else {
    keep.5[i] = TRUE
  }
  if (length(which(test.7 == FALSE)) > 0) {
    keep.7[i] = FALSE
  } else {
    keep.7[i] = TRUE
  }
  if (length(which(test.10 == FALSE)) > 0) {
    keep.10[i] = FALSE
  } else {
    keep.10[i] = TRUE
  }
}
  
## create maps of locations based on the different cutoffs
lakes = data.frame(lagoslakeid = unique(year.means$lagoslakeid), 
                   keep.5 = keep.5,
                   keep.7 = keep.7,
                   keep.10 = keep.10)
lakes = merge(lakes, locations, "lagoslakeid", all.x = TRUE)
pdf("lakes_by_cutoff.pdf")
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE,col = "gray")

points(lakes$nhd_long[lakes$keep.10 == TRUE], lakes$nhd_lat[lakes$keep.10 == TRUE], 
       pch = 16, cex=0.8, col = "slategray")
points(lakes$nhd_long[lakes$keep.7 == TRUE], lakes$nhd_lat[lakes$keep.7 == TRUE], 
       pch = 16, cex=0.8, col = "steelblue1")
points(lakes$nhd_long[lakes$keep.5 == TRUE], lakes$nhd_lat[lakes$keep.5 == TRUE], 
       pch = 16, cex=0.8, col = "springgreen2")
dev.off()

## create models for expanded dataset
lakes.5 = lakes$lagoslakeid[lakes$keep.5 == TRUE]
modern.e5 = modern[modern$lagoslakeid %in% lakes.5,]
lakes.7 = lakes$lagoslakeid[lakes$keep.7 == TRUE]
modern.e7 = modern[modern$lagoslakeid %in% lakes.7,]
lakes.10 = lakes$lagoslakeid[lakes$keep.10 == TRUE]
modern.e10 = modern[modern$lagoslakeid %in% lakes.10,]

## create a random sample of N and P values from each decade. So, for example,
## find th 
## I created several groups based on lake characteristics I was interested in weighting for
## From Ty's paper, we know we are biased based on state, land use and lake size. 
## test 

