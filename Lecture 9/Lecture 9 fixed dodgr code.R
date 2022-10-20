# Code for Spatial Econometrics: Lecture "Market Access"
# Created: 11/6/19
# Last Updated: 10/19/22
# Author: Noel Johnson

library(tidyverse)
library(sf)
library(dodgr)
library(stargazer)
library(units)
library(osmdata)

# starbucks across from merten hall
xy <- rbind (c( -77.30724741044384, 38.83382348516032),
# carow hall
             c( -77.30124991245012, 38.83144057919348)) 
xy <- data.frame(lon = xy[, 1], lat = xy[, 2])

# retrieves the OSM network
haj <- dodgr_streetnet(pts = xy,
                       expand = 0.1, quiet = F) %>%
  select(highway, osm_id)

# turns it into a graph
haj.graph <- weight_streetnet(haj,
                              wt_profile = "foot")

# computes the weighted distance on the graph
test <- dodgr_dists(haj.graph, from = xy[1,], to = xy[2,])
plot(haj[3], reset=T)
box <- st_bbox(haj)
f <- dodgr_flows_aggregate(haj.graph, from = xy[1,], to = xy[2,], flows=1, contract= T)
# the call "merge_directed_flows()" seems to no longer work. (12/14/2020)
# dodgr_flowmap(merge_directed_flows(f), linescale=5, bbox=box)
# plot(haj[3], add=T)
# these two lines do seem to work...
dodgr_flowmap(f, linescale=5, bbox=box)
plot(haj[3], add=T)





#








# End Code