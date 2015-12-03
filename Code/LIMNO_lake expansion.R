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

