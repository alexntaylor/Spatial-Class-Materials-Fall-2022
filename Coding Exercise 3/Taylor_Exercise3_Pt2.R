# Homework 3, Part 2
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

# Data
kenya_pop <- raster("./data/pop_2000.tif")
lights <- raster("./data/F142000.tif")


# Problem 1 ---------------------------------------------------------------

# Reclassify
kenya_urb_pop <- reclassify(kenya_pop, c(-Inf, 0, 0, 0, 149, 1, 149, Inf, 2))
freq(kenya_urb_pop)

# Degree of Urbanization
42521/ncell(kenya_urb_pop) # 4.576% of Kenya is urban


# Problem 2 ---------------------------------------------------------------

kenya_urb_foc <- focal(kenya_urb_pop, fun = max, w = matrix(1, nrow = 3, ncol = 3))
freq(kenya_urb_foc)

# Degree of Urbanization
84156/ncell(kenya_urb_foc) # 9.0568% of Kenya is urban


# Problem 3 ---------------------------------------------------------------

# Align
kenya_pop_lt <- projectRaster(kenya_pop, lights, method = "ngb")
plot(kenya_pop_lt)

# Light per Capita
light_pc <- overlay(lights, kenya_pop_lt, fun = function(x,y) {x/y})


# Problem 4 ---------------------------------------------------------------

# Align lights w/kenya_urb_foc
urb_foc_lt <- projectRaster(kenya_urb_foc, lights, method = "ngb")

# Average light pc

# Inf?
light_pc[light_pc==Inf] <- 0

# Average lights pc
zonal(light_pc, urb_foc_lt, fun = "mean")

# Unpopulated: 0
# Rural: 0.001883
# Urban: 0.011275







#




# End Code