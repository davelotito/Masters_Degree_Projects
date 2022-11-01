# David Lotito
# MET CS 555

library(tidyverse)

setwd('/Users/davelotito/Desktop/MET CS555 - Foundations of Machine Learning/Assignments/Assignment1')
c.difficile <- read.csv("dataset_module1.csv", header = FALSE)
c.difficile <- unlist(c.difficile, use.names = FALSE)

