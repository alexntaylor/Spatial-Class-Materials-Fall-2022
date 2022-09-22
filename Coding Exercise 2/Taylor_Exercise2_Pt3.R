# Homework 2, Part 3
# Created: 9/20/22
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
rail <- st_read("./data/africa_rail.shp")


# Problem 1 ---------------------------------------------------------------

BritEA <- africa %>% 
  filter(name == "Kenya" | name == "Tanzania" | name == "Uganda") %>% 
  st_transform("+proj=moll")
BritEA <- BritEA %>% 
  st_snap(BritEA, tolerance = 1) %>% 
  group_by(continent) %>% 
  summarize()
BritEA <- st_transform(BritEA, 4326)

# Plot
plot(st_geometry(BritEA))


# Problem 2 ---------------------------------------------------------------

# Length in British East Africa
BritEA_rail <- rail %>% 
  st_intersection(BritEA) %>% 
  mutate(length = st_length(.)) %>% 
  mutate(length = set_units(length, km)) %>% 
  group_by(continent) %>% 
  summarize(rail_dist = sum(length)) 
BritEA_rail$rail_dist

# 2934.338km of railway lines in British East Africa

# Length in Specific Countries
BritEA2 <- africa %>% 
  filter(name == "Kenya" | name == "Tanzania" | name == "Uganda")

cntry_rail <- rail %>% 
  st_intersection(BritEA2) %>% 
  mutate(length = st_length(.)) %>% 
  mutate(length = set_units(length, km)) %>% 
  group_by(sovereignt) %>% 
  summarize(rail_dist = sum(length)) %>% 
  ungroup()
cntry_rail$rail_dist

# Kenya: 236.67km of railway
# Uganda: 40.3km of railway
# Tanzania: 2657.37km of railway


# Problem 3 ---------------------------------------------------------------

# Rename one distance variable
cntry_rail <- cntry_rail %>% 
  rename(cntry_rail_dist = rail_dist)

# Merge
BritEA2 <- BritEA2 %>% 
  st_join(BritEA_rail) %>% 
  st_join(cntry_rail) %>% 
  mutate(rail_share = cntry_rail_dist/rail_dist)

# Plot
plot(BritEA2["rail_share"])

# Most railways were built in Tanzania


# Problem 4 ---------------------------------------------------------------










































#





# End Code
