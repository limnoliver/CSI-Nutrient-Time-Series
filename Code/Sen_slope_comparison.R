# test if Sen Slope gives a different answer in regards to nutrient trends
library(trend)
library(dplyr)
library(openair)

test = data.tn
test = test[,c(1:3)]
test$lagoslakeid = as.factor(test$lagoslakeid)
names(test)[2] = "date"
test$date <- as.Date(as.character(test$date), "%Y")

test.lim = test[c(1:23), ]
head(test.lim)
test.lim$date <- format(test.lim$date, "%Y")
data.tn.lim = data.tn[c(1:23), ]
sen.tn <- test.lim %>%
  group_by(lagoslakeid) %>%
  TheilSen(pollutant = "tn_umol")
  

sen.test = TheilSen(mydata = test, 
                    pollutant = "tn_umol", 
                    type = "lagoslakeid")

## code from Winslow

sens_site = function(times, data, sites_i){
  
  all_df = data.frame(times, data, sites_i)
  
  
  output = ddply(all_df, 'sites_i', function(df){
    data = df$data
    times = df$times
    
    if(length(data)==2){
      perm.i = matrix(c(1,2))
      #return((data[1]-data[2])/(times[1]-times[2]))
    }else{
      perm.i = combn(1:length(data),2)
    }
    
    
    #perm1 = data[perm.i[1,],]
    #perm2 = data[perm.i[2,],]
    starts = apply(matrix(times[c(perm.i[1,], perm.i[2,])], ncol=2), 1, min)
    ends   = apply(matrix(times[c(perm.i[1,], perm.i[2,])], ncol=2), 1, max)
    dts    = abs(times[perm.i[1,]] - times[perm.i[2,]])
    
    slopes = (data[perm.i[1,]]-data[perm.i[2,]])/(times[perm.i[1,]] - times[perm.i[2,]])
    
    return(data.frame(slopes, start=starts, end=ends, dt=diff(range(times)), n.obs=length(times)))
    
  })
  
  return(output)
}

test.sen = sens_site(data.tn$date, log(data.tn$tn_umol), data.tn$lagoslakeid)
