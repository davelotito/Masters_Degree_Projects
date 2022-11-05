## Lecture Code Practice

##############################
### CONFIDENCE INTERVALS ###
##############################

# PRACTICE QUESTION #1 #

# Let’s return to the example above. Suppose the executives would like to estimate the average amount of time callers are on hold before 
# they reach a customer service representative by computing a 99%
# confidence interval. As before, assume that they randomly sample 100
# calls, that the sample mean was 30.9
# minutes, and that the population standard deviation of wait times, σ, is 15 minutes.

n <- 100
xbar <- 30.9
s <- 15


margin <- qt(0.99, df=n-1)*s/sqrt(n)

(lowerinterval <- xbar - margin)

(upperinterval <- xbar + margin)

# We are 99% confident that the true mean wait time is between 27.35 minutes and 34.45 minutes.

# PRACTICE QUESTION #2 #

# An investigator took a random sample of 36 patients from his practice and measured their systolic blood pressure. 
# See some summary statistics below for the sample. 
# Assume the population standard deviation is 30 mmHg. Calculate a 95%
# confidence interval for the population mean of systolic blood pressure.

n <- 36
xbar <- 124.3
s <- 30


margin <- qt(0.95, df=n-1)*s/sqrt(n)

(lowerinterval <- xbar - margin)

(upperinterval <- xbar + margin)

# We are 95% confident that the true mean systolic blood pressure is between 115.85
# and 132.75 mmHg.

##############################
### MARGIN OF ERROR ###
##############################

# Let’s return to the example above. 
# Suppose the executives would like to estimate the 
# average amount of time callers are on hold before they reach a 
# customer service representative by computing a 95%
# confidence interval. As before, the population standard deviation of wait times, σ is 15
# minutes. The executives would like the half width of the confidence interval to be 1.0
# minutes (that is, they’d like the margin of error to be 1.0
# How many callers must they sample to get their 95%
# confidence interval to have the desired width?

(me <- (1.960*15/2)*2)^2








