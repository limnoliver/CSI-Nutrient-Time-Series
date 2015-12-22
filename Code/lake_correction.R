## I created several groups based on lake characteristics I was interested in weighting for
## From Ty's paper, we know we are biased based on state, land use and lake size. 

# the chance of being sampled by state

lakes.4ha = data.lake.specific[data.lake.specific$lake_area_ha >= 4, ]
lakes.4ha = lakes.4ha[lakes.4ha$state_name != "OUT_OF_COUNTY_STATE", ]
lakes.4ha = droplevels(lakes.4ha)
states = levels(lakes.4ha$state_name)

# What are the chances of being in a certain state?
# from the population, calculate proportion of lakes in each state

state.props = as.numeric(summary(lakes.4ha$state_name))/nrow(lakes.4ha)

# what are the true sampled proportions?

data.props = data[,c(2)]
data.props = unique(data.props)
data.props = merge(data.props, data.lake.specific, all.x=TRUE)

# What are the changes of being  in a certain landuse category, given a certain state?
# 