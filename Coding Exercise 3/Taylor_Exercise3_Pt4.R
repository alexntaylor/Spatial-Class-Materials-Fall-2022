# Homework 3, Part 4
# Created: 9/23/22
# Last Edited: 9/23/22
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

global.libraries <- c("tidyverse", "sf", "haven", "raster", "gstat", "tmap", "units", "pacman", "rgdal")

results <- sapply(as.list(global.libraries), pkgTest)

library(pacman)
p_load(tidyverse, raster, sf, rgdal, haven, gstat, tmap, units)

dir <- "/Users/atay508/Documents/George Mason/2022-23 Classes/Spatial/Spatial-Class-Materials-Fall-2022/Coding Exercise 3"

setwd(dir)
getwd()


# Problem 1 ---------------------------------------------------------------

kenya_pop <- raster("./data/pop_2000.tif")
kenya_county <- st_read("./data/kenya_counties.shp")

# Projection & Spatial
kenya_county_sp <- kenya_county %>% 
  st_transform("+proj=moll") %>% 
  as("Spatial")

# Crop and Mask
kenya_pop <- crop(kenya_pop, kenya_county_sp)
kenya_pop <- mask(kenya_pop, kenya_county_sp)


# Problem 2 ---------------------------------------------------------------

pop_class <- reclassify(kenya_pop, c(-Inf, 1499, NA))
pop_vec <- clump(pop_class) %>% 
  rasterToPolygons() %>% 
  st_as_sf()
pop_vec <- pop_vec %>% 
  group_by(clumps) %>% 
  summarize()
plot(pop_vec)


# Problem 3 ---------------------------------------------------------------

kenya_cities <- st_read("../Coding Exercise 1/data/africa_ctys.shp", stringsAsFactors = F) %>% 
  dplyr::select(name, pop, iso3v10) %>% 
  filter(iso3v10 == "KEN")
projection(kenya_cities)


# Problem 4 ---------------------------------------------------------------

# Transform pop_vec because kenya_cities was being difficult
pop_vec <- pop_vec %>% 
  st_transform(4326)

pop_cities <- pop_vec %>% 
  st_join(kenya_cities, join = st_is_within_distance, dist = 1000) %>% 
  group_by(clumps) %>% 
  mutate(big = max(pop)) %>% 
  filter(pop == big) %>% 
  ungroup()

# Unique?
table(pop_cities$name) # Yes


# Problem 5 ---------------------------------------------------------------

pop_cities <- pop_cities %>% 
  as("Spatial")

cities <- raster::extract(kenya_pop, pop_cities, fun = sum, na.rm = T, df = T)

pop_cities <- pop_cities %>% 
  as("sf")

pop_cities <- pop_cities %>% 
  mutate(ID = 1:nrow(.)) %>% 
  left_join(cities, by = "ID")

pop_cities$pop_2000 - pop_cities$pop # Okay, but not great










#







# End Code

