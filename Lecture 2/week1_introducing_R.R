# Week 1 introduction to R code for TaD class 
# Author: Noel Johnson (adapted from many others---see below)
# Created: 1-27-2022
# Last Edited: 1-27-2022
# Lab adapted from: Lucia Motolinia, Kevin Munger, Patrick Chester,
# Leslie Huang, Pedro L. Rodriguez, and Lucia Motolinia.

# Before you start:
# Download the files "national_clinton_trump_6_20_2016.csv" and "cps08.csv" 

# TIPS:
# you should always (always!) annotate your code
# DEBUGGING: rubber duck it (see https://en.wikipedia.org/wiki/Rubber_duck_debugging
# and https://rubberduckdebugging.com)
# Google is your friend. Type your question and add "R" to the end of it.
# knitr is useful for problem sets that require showing your code 
# For bigger projects: use a dependency manager
# (packrat https://rstudio.github.io/packrat/) for projects 

#-----------------------------
# 1 SETTING UP
#-----------------------------

# 1.1 Clearing environment
rm(list = ls())
# also, some version of cmd+shift+0 (for mac) will restart your RStudio
# session and clear the workspace. I use this A LOT.

# 1.2 Working directory
# return current working directory
getwd()  
# set working directory
setwd("/Users/noeljohnson_laptop/Dropbox/Teaching/TaD_Sp2022/code/final_code_for_students/week_1")  

# 1.3 Installing and loading some useful packages from CRAN
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("xtable")
# install.packages("devtools")

# Installing packages from GitHub
# devtools::install_github("quanteda/quanteda.corpora")
# update.packages() # update packages
# (careful, some codes may not run -> you can use packrat in that case)

library(dplyr)
library(ggplot2)
library(xtable)

# Loading multiple packages in a go
libraries <- c("foreign", "stargazer")
lapply(libraries, require, character.only=TRUE)

# 1.6 Loading data
polling_data  <- read.csv("data/national_clinton_trump_6_20_2016.csv",
                          stringsAsFactors = FALSE)

cps08 <- read_csv("data/cps08.csv")

#-----------------------------
# 2 WORKING WITH DATA
#-----------------------------

# 2.1 Take a peek, get to know the structure of the data

head(polling_data)  # display first lines of an object
head(polling_data, n = 10)  # same as above but specifying number of lines 
tail(polling_data)  # display last lines of an object
dim(polling_data)  # data dimensions
nrow(polling_data)  # number of rows
ncol(polling_data)  # number of columns
colnames(polling_data)  # column names
names(polling_data)  # also column names (more general command)
rownames(polling_data) # row names
class(polling_data)  # returns class of an R object: vector, list, matrix, data frame
sapply(polling_data, class) # returns class for each variable (column)
str(polling_data)  # display structure of an R object (e.g. a dataframe)
glimpse(polling_data)
?sapply  # get R Documentation on this command (see Help panel below)

# you can use stargazer to summarize variables

library(stargazer)
cps08_no_na <- na.omit(cps08)
stargazer(as.data.frame(cps08_no_na), type="text",
          out="cps_08_stats")

# you can output html, tex, pdf, etc....
stargazer(as.data.frame(cps08_no_na), type="latex",
          out="cps_08_stats.tex")

cps08_bachmean <- cps08 %>%
  group_by(bachelor) %>%
  summarize(mean(ahe, na.rm=TRUE)) %>%
  ungroup()
cps08_bachmean

# Stargazer plays well with regression analysis etc...

glimpse(cps08)

m1 <- lm(ahe ~ age, data=cps08)
m2 <- lm(ahe ~ age + factor(bachelor), data=cps08)
m3 <- lm(ahe ~ age + factor(bachelor) + factor(female), data=cps08)
m4 <- glm(factor(female) ~ ahe + factor(bachelor),
          family=binomial(link="logit"), data=cps08)
stargazer(m1, m2, m3, m4, type="html", 
          dep.var.labels=c("dep variable 1","dep variable 2"), 
          covariate.labels=c("age","bachelor","gender"),
          out="models.html")


# 2.2 Subset dataframes ----------------------------------------------------

# A) Get column with dollar sign operator
head(polling_data$Pollster) 

# B) Matrix identifier: df[rowname, colname]
head(polling_data[, "Pollster"])
#or get column with coordinates (be careful, changes in data structure will change the coordinates of a variable)
head(polling_data[,1])

#lets try to see the pollster data
polling_data[, "Pollster"]

# That was pretty impossible to read in the console, let's try this:
View(polling_data[, c("Pollster", "Number.of.Observations")]) 
#or just a subset
View(polling_data[polling_data$Pollster == "CBS", c("Pollster", "Number.of.Observations")])
#another way to view the data is just to click on the dataframe in the environment

# C) dplyr
# a very powerful package with intuitive commands for subsetting,
# selecting, and transforming your data
# cran documentation
# https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html  
# https://r4ds.had.co.nz/tidy-data.html  # going beyond dplyr,
# tidy data principles
# https://www.tidyverse.org  # other packages in the tidy universe (tidyverse)
# note: it's always useful to be able to do things with base R functions
# (helps understanding)

# Using pipe notation
polling_data %>% select(Pollster) %>% head(.,10)
polling_data %>% select(Pollster, Number.of.Observations) %>% head()

# Alternative syntax
# stick to one syntax, repetition helps recall (note order matters)
head(select(polling_data, Pollster, Number.of.Observations))

# 2.3 How to locate row(s) in a data frame ----------------------------------------------------

# A) Dollar sign operator
# Returns the first row of the data frame in the specified column (Python users: R indexing starts at 1)
polling_data$Number.of.Observations[1] 
# Returns the first 5 rows of the data frame in the specified column
polling_data$Number.of.Observations[1:5] 
# Returns all rows for the variable "Number.of.Observations" where Pollster = Quinnipiac
polling_data$Number.of.Observations[polling_data$Pollster == "Quinnipiac"] 

# B) Column name
polling_data[1, "Number.of.Observations"] 
polling_data[1:5, "Number.of.Observations"] 
polling_data[polling_data$Pollster == "Quinnipiac","Number.of.Observations"] 

# C) dplyr
# Pipe syntax
polling_data %>% slice(1) %>% select(Number.of.Observations) 
polling_data %>% slice(1:5) %>% select(Number.of.Observations)
polling_data %>% filter(Pollster == "Quinnipiac") %>% select(Number.of.Observations)
# can keep "piping"
polling_data %>% filter(Pollster == "Quinnipiac") %>%
  slice(1) %>% select(Number.of.Observations) 
# BUT note order can matter
polling_data %>% slice(1) %>% filter(Pollster == "Quinnipiac") %>%
  select(Number.of.Observations)  

# Alternate syntax
# stick to one syntax, repetition helps recall
select(filter(polling_data, Pollster == "Quinnipiac"), Number.of.Observations)  

# 2.4 Creating new variables (columns) in a data frame ----------------------

# A) Dollar sign operator
polling_data$net_clinton_a <- polling_data$Clinton - polling_data$Trump

# B) Matrix identifier
polling_data[, "net_clinton_b"]  <- polling_data[, "Clinton"] - polling_data[, "Trump"]

# C) dplyr
# Pipe syntax
polling_data <- polling_data %>% mutate(net_clinton_c = Clinton - Trump)

# Alternate syntax
polling_data <- mutate(polling_data, net_clinton_d = Clinton - Trump)
# requires library(magrittr) see: https://magrittr.tidyverse.org
#polling_data %<>% mutate(net_clinton_e = Clinton - Trump)

# Are these variables equivalent to one another?
all.equal(polling_data$net_clinton_a,polling_data$net_clinton_b)  
all.equal(polling_data$net_clinton_b,polling_data$net_clinton_c) 
all.equal(polling_data$net_clinton_c,polling_data$net_clinton_d)  
# Yes. Yes they are.

# 2.5 Removing columns ----------------------------------------------------
polling_data$net_clinton_b <- NULL
# one way to check if deleted column was actually deleted
"net_clinton_b" %in% colnames(polling_data)  
polling_data[, "net_clinton_c"] <- NULL  # using matrix notation

# Using dplyr
polling_data <- polling_data %>% select(-net_clinton_d)
polling_data <- polling_data %>% select(-c(Source.URL, Pollster.URL))

# 2.6 Summarizing Data ----------------------------------------------------

# A) Start always by getting to know the structure of the data (see above)

# B) General summary
# summary statistics where appropriate (non-string/character variables)
summary(polling_data)  

# C) Single variable summary
mean(polling_data$net_clinton_a)
sd(polling_data$net_clinton_a)
# using dplyr
polling_data %>% summarise(mean_net_clinton = mean(net_clinton_a))  
# summary for a specific group
polling_data %>% filter(Population == "Registered Voters") %>%
  summarise(mean_net_clinton = mean(net_clinton_a))  

# D) Summary by group
# use group_by
polling_data %>% group_by(Pollster) %>%
  summarise(mean_net_clinton = mean(net_clinton_a))  
# can perform multiple summary stats
polling_data %>% group_by(Pollster) %>%
  summarise(mean_net_clinton = mean(net_clinton_a), sd_net_clinton = sd(net_clinton_a))
# can group by more than one variable
table1 <- polling_data %>% group_by(Pollster, Population) %>%
  summarise(mean_net_clinton = mean(net_clinton_a)) %>%
  ungroup %>% slice(1:5)  

View(table1)

# E) Summarizing a variable with a histogram

# Basic R graphics
hist(polling_data$net_clinton_a)

# ggplot2 graphics
plot1 <- ggplot(aes(net_clinton_a), data = polling_data) +
  geom_histogram(bins = 15) + theme_light()

plot1

# take a look at plotly for interactive plots: https://plot.ly/r/

# 2.7 Exporting data

# Exporting table to CSV
write.csv(table1,file = "table1.csv")

# Creating LaTeX table (copy output and paste in your Latex document)
xtable(table1,caption = "Average Clinton Polling Advantage by Polling Firm")
#library("stargazer")
stargazer(table1, summary = FALSE)

# Exporting graph to pdf
pdf(width = 4, height = 3, "plot1.pdf")
plot1
#dev.off()

#-----------------------------
# 3 LOOP & FUNCTIONS
#-----------------------------

# 3.1 For Loops

for(col_name in names(polling_data)){ # A loop that identifies and stores variables that contain characters
  if(is.character(polling_data[, col_name])) {
    print(col_name)
  }
}

# 3.2 Apply functions (with regex)
names(polling_data) <- sapply(names(polling_data), function(i) {
  i <- gsub("\\.", "_", i) # Replaces all instances of "." with an "_"
  i <- gsub("__", "_", i) # Replaces all instances of "__" with "_"
} )

sapply(polling_data[,c("Clinton", "Trump")], mean)  # easy to apply base functions 
sapply(polling_data[,c("Clinton", "Trump", "Undecided")], mean) # mean does not work with NAs
sapply(polling_data[,c("Clinton", "Trump", "Undecided")], mean, na.rm = TRUE) # need to specify how to deal with NAs (this is an argument of mean)
sapply(polling_data[,c("Clinton", "Trump", "Undecided")], function(x) mean(x, na.rm = TRUE)) # alternative

mean_vars1 <- sapply(polling_data[,c("Clinton", "Trump", "Undecided")], function(x) mean(x, na.rm = TRUE))  # output of sapply is a vector or a matrix
mean_vars2 <- lapply(polling_data[,c("Clinton", "Trump", "Undecided")], function(x) mean(x, na.rm = TRUE)) # output of lapply is a list

# the apply is useful when applying a function to rows OR columns
apply(polling_data[,c("Clinton", "Trump")], 2, mean, na.rm = TRUE) # 2 = columns
apply(polling_data[,c("Clinton", "Trump")], 1, mean, na.rm = TRUE) # 1 = rows

# dplyr version
polling_data %>% summarise(avg.clinton = mean(Clinton), avg.trump = mean(Trump, na.rm = TRUE))
polling_data %>% rowwise() %>% summarise(avg.row = mean(Clinton, Trump, na.rm = TRUE))
  
# Python users: The function passed to sapply() is the equivalent of a lambda function

# 3.3 User written functions

# Calculates the cosine similarity between two vectors
calculate_cosine_similarity <- function(vec1, vec2) { 
  nominator <- vec1 %*% vec2  # %*% specifies dot product rather than entry by entry multiplication (we could also do: sum(x * y))
  denominator <- sqrt(vec1 %*% vec1)*sqrt(vec2 %*% vec2)
  return(nominator/denominator)
}

set.seed(1984L)  # allows us to replicate result
x  <- rnorm(10) # Creates a vector of random normally distributed numbers
y  <- x*2 + 3

calculate_cosine_similarity(x,y)

# Python users: R cannot return multiple values from a function -- you will have to return a list of the values you want to return. 

calculate_distance <- function(vec1, vec2) { 
  nominator <- vec1 %*% vec2  # %*% specifies dot product rather than entry by entry multiplication (we could also do: sum(x * y))
  denominator <- sqrt(vec1 %*% vec1)*sqrt(vec2 %*% vec2)
  cos_dist <- nominator/denominator
  euc_dist <- sqrt(sum((vec1 - vec2)^2))
  return(list(cosine = cos_dist, euclidean = euc_dist))
}

calculate_distance(x,y)
dist_comp <- calculate_distance(x,y)  # we can store this result
dist_comp[["cosine"]]
dist_comp$cosine
dist_comp[[1]]

#-----------------------------
# 4 FINISHING UP
#-----------------------------

# 4.1 Save workspace after running it -- all objects, functions, etc  (e.g. if you have run something computationally intensive and want to save the object for later use)
# Similar to pickle() in Python

save.image("workspace.RData")

# 4.2 Pick up where you left off (but note that the workspace does not include packages. You need packrat for that)

rm(list = ls())

load("workspace.RData")

#-----------------------------
# 4 FREE RESOURCES
#-----------------------------

# UCLA
# http://www.ats.ucla.edu/stat/r/

# Rbloggers
# https://www.r-bloggers.com/how-to-learn-r-2/

# Data Camp
# https://www.datacamp.com/

# Swirl
# http://swirlstats.com/

# If you have a question, it's probably been asked before on StackOverflow/StackExchange!

