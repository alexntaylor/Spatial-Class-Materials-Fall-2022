# Homework 1, Exercise: Road Densities
# Created: 9/13/22
# Last Edited: 9/13/22
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

global.libraries <- c("tidyverse", "sf", "haven", "raster", "gstat", "viridis", "tmap", "units")

results <- sapply(as.list(global.libraries), pkgTest)

# library(tidyverse)
# library(sf)
# library(haven)
# library(raster)
# library(gstat)
# library(viridis)
# library(tmap)

dir <- "/Users/atay508/Documents/George Mason/2022-23 Classes/Spatial/Spatial-Class-Materials-Fall-2022/Coding Exercise 1"

setwd(dir)
getwd()


# Data --------------------------------------------------------------------

africa_sf <- st_read("./data/africa_scale.shp")
africa_roads <- st_read("./data/africa_roads.shp")


# Problem 1 ---------------------------------------------------------------

plot(africa_roads["type"], reset=F, main = "Roads by Type", key.pos=1)
plot(africa_sf[1], col = NA, add=T)


# Problem 2 ---------------------------------------------------------------

africa_roads <- africa_roads %>% 
  mutate(road_length = africa_roads %>% 
           st_length() %>% 
           set_units(km))


# Problem 3 ---------------------------------------------------------------

africa_rds_df <- africa_roads %>% 
  st_set_geometry(NULL) %>% 
  group_by(adm0_a3) %>% 
  mutate(tot_rd_lgnth = sum(road_length)) %>% 
  ungroup()


# Problem 4 ---------------------------------------------------------------

africa_sf <- africa_sf %>% 
  mutate(ctry_area = africa_sf %>% 
           st_area() %>% 
           set_units(km^2))

# Work toward merge
africa_rds_df <- africa_rds_df %>% 
  rename(iso_a3 = adm0_a3) %>% 
  dplyr::select(iso_a3, tot_rd_lgnth) %>% 
  group_by(iso_a3) %>% 
  summarize_all(mean) %>% 
  ungroup()

# Merge
africa_sf_new <- africa_sf %>% 
  left_join(africa_rds_df, by = "iso_a3")

# Correlation
africa_sf_new %>% 
  st_set_geometry(NULL) %>% 
  drop_na(ctry_area, tot_rd_lgnth) %>%
  dplyr::select(ctry_area, tot_rd_lgnth) %>% 
  cor()

# Correlation coefficient is 0.8789244


# Problem 5 ---------------------------------------------------------------

africa_sf_new <- africa_sf_new %>% 
  mutate(rd_density = tot_rd_lgnth/ctry_area)

# Plot
plot(africa_sf_new["rd_density"], main = "Road Density")

#

# End Code