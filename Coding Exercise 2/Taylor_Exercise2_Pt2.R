# Homework 2, Part 2
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


# Problem 1 ---------------------------------------------------------------

# Create and combine points
p1 <- st_point(c(0,1))
p2 <- st_point(c(1,1))
points <- st_sfc(p1,p2)

# Buffer
points_b <- st_buffer(points, dist = 1)

# Plot
plot(points_b, col=1:2)
plot(points, col=3, add=T)


# Problem 2 ---------------------------------------------------------------

# Calculate intersection
points_int <- st_intersection(points_b[1], points_b[2])

# Plot
dev.off()
plot(points_b, col=1:2)
plot(points_int, col=3, add=T)


# Problem 3 ---------------------------------------------------------------

# Method 1
dev.off()
sub1 <- st_sym_difference(points_b[1], points_b[2])
plot(sub1, col=1:2)

# Method 2
dev.off()
sub2 <- st_difference(points_b, points_int)
plot(sub2, col=1:2)


# Problem 4 ---------------------------------------------------------------

points_u <- st_union(points_b)
plot(points_u)

# The union "unites" (of course) the two buffer circles into one figure without the overlap in the middle


# Problem 5 ---------------------------------------------------------------

pt3 <- st_point(c(0.5,0.5))

# Intersections?
st_intersects(pt3, points_u, sparse = F) # Yes, this point intersects with the union of buffers
st_intersects(pt3, sub1, sparse = F) # No, this point does not intersect with the buffers minus their intersection









#




# End Code