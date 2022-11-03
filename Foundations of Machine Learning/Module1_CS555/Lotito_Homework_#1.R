# David Lotito
# MET CS 555

library(tidyverse)

# (1) Save the data to a CSV file and read into R for analysis. 
setwd('/Users/davelotito/Desktop/MET CS555 - Foundations of Machine Learning/Assignments/Assignment1')
c.difficile <- read.csv("dataset_module1.csv", header = FALSE)
c.difficile <- unlist(c.difficile, use.names = FALSE)

# (2) Make a histogram of the duration of days of hospital stays.  
# Ensure the histogram is labelled appropriately.  
# Use a width of 1 day.  
# Describe the shape, center, and spread of the data.  Are there any outliers? 

hist(c.difficile, 
     probability=T, 
     col='lightblue', 
     main='Histogram of c.difficile Cases',
     xlab = 'Duration of Hospital Stays',
     breaks = seq(2, 15, by=1),
     xlim = c(2, 16),
     ylim = c(0, 0.25))

table(c.difficile)
sd(c.difficile)

# The data here is definitely right skewed with some outliers of hospital stays
# that are > 10. The mean and median are close, but the mean is grater
# than the median at 5.63 vs. 5.00 for the median.

# (3) Find the mean, median, standard deviation, first and third quartiles, 
# minimum and maximum of the durations of hospital stay in the sample.  
# Summarize these values in a table that you create in EXCEL or WORD. 
# In other words, do *not* simply copy and paste R output. 
# You should be reporting a nicely labeled and formatted table. 

### PLEASE SEE ATTACHED EXCEL FILE ###

# Given the shape of the distribution, what is the best 
# single number summary of the center of the distribution?

# The median would be the best as its not influenced by outliers and will give you the center of the
# distriution
median(c.difficile)

# 4) Assume that the literature on this topic suggests that the distribution 
# of days of hospital stay are normally distributed with a mean of 5 
# and a standard deviation of 3.  
# Use R to determine the probabilities below based on the normal distribution described above 
# (you should not be using the data set given on the following page):

# (a) What percentage of patients are in the hospital for less than 10 days? 

pnorm(10, mean=5, sd=3) * 100

# (b) Recent publications have indicated that hypervirulent strains of C. Difficile are on the rise.  
# Such strains are associated with poor outcomes, including extended hospital stays.   
# An investigator is interested in showing that the average hospital stay duration have increased versus published literature.  
# He has a sample of 35 patients from his hospital.  
# If the published data are consistent with the truth, what is the probability 
# that the sample mean in his sample will be greater than 6 days? 

pnorm(6, mean=5, sd=3/sqrt(35))

