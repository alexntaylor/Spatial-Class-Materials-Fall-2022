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


# Problem 1 ---------------------------------------------------------------

table(africa$adm0_a3)

# Yes, unique ID


# Problem 2 ---------------------------------------------------------------

africa_fixed <- africa %>% 
  st_transform("+proj=moll")

africa_fixed <- africa_fixed %>% 
  st_snap(africa_fixed, tolerance = 100) %>% 
  group_by(adm0_a3) %>% 
  summarize()

africa <- africa %>% 
  st_set_geometry(NULL)

africa_fixed <- africa_fixed %>% 
  left_join(africa, by = "adm0_a3")

africa_fixed <- africa_fixed %>% 
  st_transform(4326)

plot(africa_fixed[1])


# Problem 3 ---------------------------------------------------------------

st_is_valid(africa_fixed)
st_is_simple(africa_fixed)


# Problem 4 ---------------------------------------------------------------

plot(st_union(africa_fixed), col=NA, main="Union") # No slivers!

st_write(africa_fixed, "./data/africa_fixed.shp", append = F)











#




# End Code