## calculate sampling weights for each state

## overall idea:
## weight separately in each state based on 1) size and 2) land use, then overall
## based on location (state)

# first, create bins for area
pop = data.lake.specific[data.lake.specific$state_name != "OUT_OF_COUNTY_STATE", ]
pop$area_groups = as.numeric(cut2(pop$lake_area_ha, g=5))
area.groups = cut2(pop$lake_area_ha, g=5, onlycuts = TRUE)

  prop.area <- function(x, group) {
  n.lakes = length(x)
  n.group = as.numeric(summary(as.factor(x)))
  return(n.group/n.lakes)
}

    
state.area = aggregate(pop$area_groups, by = list(pop$state_name), prop.area)
state.area =  data.frame(state = state.area$Group.1, area_1 = state.area$x[,1], 
                         area_2 = state.area$x[,2], area_3 = state.area$x[,3], 
                         area_4 = state.area$x[,4], area_5 = state.area$x[,5])

 data.lake.specific$lakeconnectivity_group = data.lake.specific$lakeconnectivity
levels(data.lake.specific$lakeconnectivity_group) = c(4,3,2,1)
