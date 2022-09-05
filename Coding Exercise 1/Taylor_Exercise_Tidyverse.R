# Homework 1, Exercise: Tidyverse
# Created: 9/2/22
# Last Edited: 9/2/22
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

ratings <- read.csv("http://www.stern.nyu.edu/~wgreene/Text/Edition7/TableF4-3.csv")


# Problem 1 ---------------------------------------------------------------

# Select first 5 variables, change BOX to millions

ratings_5 <- ratings %>% 
  slice(1:5)
ratings_5

ratings <- ratings %>% 
  mutate(BOX = BOX/1000000)
ratings


# Problem 2 ---------------------------------------------------------------

# Create factor MPAA from MPRATING

# MPAA <- ratings$MPRATING %>%
#   factor(labels = c("G", "PG", "PG13", "R"))
# MPAA

ratings <- ratings %>% 
  mutate(MPAA = factor(MPRATING, labels = c("G","PG","PG13","R")))
ratings


# Problem 3 ---------------------------------------------------------------

m.mpaa <- ratings %>% 
  mutate(RATIO = BOX/BUDGET) %>% 
  group_by(MPAA) %>% 
  summarize_at(vars("BOX","BUDGET","RATIO"),mean) %>% 
  rename(M.BOX = BOX, M.BUDGET = BUDGET, M.RATIO = RATIO) %>% 
  ungroup()
m.mpaa

# R rated movies recover more of their initial investment than movies with other ratings


# Problem 4 ---------------------------------------------------------------

ratings <- ratings %>% 
  mutate(RATIO = BOX/BUDGET) %>% 
  left_join(m.mpaa, by = "MPAA")
ratings

ratings <- ratings %>% 
  group_by(MPAA) %>% 
  mutate(SD.RATIO = sd(RATIO)) %>% 
  ungroup()
ratings

ratings <- ratings %>% 
  mutate(DIST.RATIO = abs(RATIO - M.RATIO)/SD.RATIO)

# Top 6 Over and Under Performers

g_perform <- ratings %>% 
  filter(MPAA == "G") %>% 
  arrange(desc(DIST.RATIO)) %>% 
  slice(1:6)
g_perform # Only 2 G-rated movies in dataset, equidistant from mean

pg_perform <- ratings %>% 
  filter(MPAA == "PG") %>% 
  arrange(desc(DIST.RATIO)) %>% 
  slice(1:6)
pg_perform

pg13_perform <- ratings %>% 
  filter(MPAA == "PG13") %>% 
  arrange(desc(DIST.RATIO)) %>% 
  slice(1:6)
pg13_perform

r_perform <- ratings %>% 
  filter(MPAA == "R") %>% 
  arrange(desc(DIST.RATIO)) %>% 
  slice(1:6)
r_perform






















