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
p_load(raster, sf, rgdal, tidyverse, haven, gstat, tmap, units)

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

# Tried copying and pasting africa_ctys into the data folder for exercise 3, but it couldn't read the copied file.
# So instead I'm loading it from the exercise 2 folder. Hope this doesn't cause problems trying to run the script.
kenya_cities <- st_read("../Coding Exercise 2/data/africa_ctys.shp") %>%
  filter(iso3v10=="KEN") %>% 
  select(name, pop, iso3v10) %>%
  st_transform("+proj=moll")
## NOT PROJECTING PROPERLY, CAN'T DO THE REST


# Problem 4 ---------------------------------------------------------------

pop_cities <- pop_vec %>% 
  st_join(kenya_cities, join = st_is_within_distance, dist = 1000)
