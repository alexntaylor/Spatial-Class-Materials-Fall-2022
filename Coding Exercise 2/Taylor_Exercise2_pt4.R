# Homework 2, Part 4
# Created: 9/21/22
# Last Edited: 9/21/22
# Author: Alexander Taylor


# Clear Workspace ---------------------------------------------------------

rm(list = ls())

# Libraries & Working Directory -------------------------------------------

pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
  return("OK")
}

global.libraries <- c("tidyverse", "sf", "haven", "raster", "gstat", "viridis", "tmap", "units", "pacman")

results <- sapply(as.list(global.libraries), pkgTest)

library(pacman)
p_load(tidyverse, sf, raster, gstat, tmap, units)

dir <- "/Users/atay508/Documents/George Mason/2022-23 Classes/Spatial/Spatial-Class-Materials-Fall-2022/Coding Exercise 2"

setwd(dir)
getwd()

# Data
africa <- st_read("./data/africa_scale.shp")
roads <- st_read("./data/africa_roads.shp")


# Problem 1 ---------------------------------------------------------------

# Grid
afrca_grid <- africa %>% 
  st_make_grid(cellsize = 1, offset = c(-18,-35), what = "polygons")

# IDs and sf
afrca_grid <- st_sf(id = 1:length(afrca_grid), geometry = afrca_grid)


# Problem 2 ---------------------------------------------------------------

# Union
africa <- africa %>% 
  st_transform("+proj=moll")
africa <- africa %>% 
  st_snap(africa, tolerance = 100) %>% 
  st_transform(4326)
africa <- africa %>% 
  st_union()

# Clip
africa_int <- st_intersection(afrca_grid, africa)
plot(africa_int, col=NA)


# Problem 3 ---------------------------------------------------------------

# Renumber
africa_int <- africa_int %>% 
  mutate(id = 1:length(africa_int$id))

# Area
africa_int <- africa_int %>% 
  mutate(area = st_area(.)) %>% 
  mutate(area = set_units(area, km^2))


# Problem 4 ---------------------------------------------------------------

# I tried to run the intersection necessary here, but R worked for several hours and couldn't do it.
# So here's the code I would have run for problems 4 and 5.

#rds_africa <- roads %>% 
#  st_intersection(africa_int)
#rds_africa <- rds_africa %>% 
#  mutate(rd_lgnth = st_length(.)) %>% 
#  mutate(rd_lgnth = set_units(rd_lgnth, km)) %>% 
#  group_by(id) %>% 
#  summarize(ttl_lgnth = sum(rd_lgnth)) %>% 
#  ungroup()


# Problem 5 ---------------------------------------------------------------

#africa_int <- africa_int %>% 
#  left_join(rds_africa, by = "id") %>% 
#  mutate(rd_density = ttl_lgnth/area)
#
#plot(africa_int["rd_density"], breaks="kmeans")














#










# End Code


