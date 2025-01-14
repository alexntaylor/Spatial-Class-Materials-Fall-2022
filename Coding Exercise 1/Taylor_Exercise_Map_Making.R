# Homework 1, Exercise: Map Making
# Created: 9/12/22
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

global.libraries <- c("tidyverse", "sf", "haven", "raster", "gstat", "viridis", "tmap")

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
africa_sf <- africa_sf %>% 
  dplyr::select(admin, type, iso_a3, region_wb, pop_est)
cities <- read.csv("./data/africa_cities.csv")


# Problem 1 ---------------------------------------------------------------

# Convert pop to numeric
cities$pop <- as.numeric(cities$pop)

# Remove pop NAs
cities <- cities %>% 
  drop_na(pop)

cities_large <- cities %>% 
  group_by(iso3v10) %>% 
  filter(pop == max(pop)) %>% 
  ungroup()
cities_large

# Largest city is Kinshasa with a population of 7,273,947

cities_small <- cities %>% 
  group_by(iso3v10) %>% 
  filter(pop == min(pop)) %>% 
  ungroup()
cities_small

# Caxito, Lucapa, N'Giva, and Arta all have 0 population
# Smallest non-zero population is Harbour with a population of 10


# Problem 2 ---------------------------------------------------------------

cities_urb <- cities %>% 
  group_by(iso3v10) %>% 
  mutate(ctry_pop_tot = sum(pop, na.rm = T), pr_pop = max(pop)) %>% 
  mutate(pr_share = pr_pop/ctry_pop_tot) %>% 
  ungroup()
cities_urb


# Problem 3 ---------------------------------------------------------------

# Merge
cities_m <- cities_urb %>% 
  group_by(iso3v10) %>% 
  filter(pop == max(pop)) %>% 
  ungroup() %>% 
  rename(iso_a3 = iso3v10) %>% 
  dplyr::select(pr_share, iso_a3)
africa_n <- africa_sf %>% 
  left_join(cities_m, by = "iso_a3")

# Plot
plot(africa_n["pr_share"],
     axes = T,
     main = "% of Urban Pop in Primate City")

#

# End Code