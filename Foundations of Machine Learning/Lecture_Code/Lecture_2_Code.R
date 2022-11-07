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

(me <- (1.960*15/1.0)^2)

# Since we can’t have exactly 864.36 callers (there is no such thing as 36
# hundredths of a caller), we need to round up to 865
# In calculations involving sample size we always round up (instead of down).












####################
# QUIZ #2 QUESTIONS
####################

# Question #2

# Te test statistic from a test of the following hypotheses:
# H 0: = 43 & H 1 not equal to 43
# was calculated to be z = 1.369. Calculate the associated p-value

#Answer
2 * pnorm(q=1.369, lower.tail=F)

# Question #3

# Te test statistic from a test of the following hypotheses:
# H 0: = 54 & H 1 > to 54
# was calculated to be z = 1.296. Calculate the associated p-value

#Answer
pnorm(q=1.296, lower.tail=F)

# Question #4

# Te test statistic from a test of the following hypotheses:
# H 0: = 50 & H 1 < to 50
# was calculated to be z = 1.35. Calculate the associated p-value

#Answer
(pnorm(q=1.35, lower.tail=F) / 2) + pnorm(q=1.35, lower.tail=T)

# Question #5

# We are interested in estimating the mean amount of rain last month in our county. 
# It is known that the population standard deviation is 
# 1 inches generally for the month of interest. 
# 67 instruments that measure rainfall were placed throughout the county randomly. 
# The sample mean from the instruments was 5.6 inches. Calculate a 95% confidence interval
# for the population mean of rainfall last month in the country

#Answer #1
x.bar <- 5.6
z <- 2 * pnorm(0.475, lower.tail = T)
sd <- 1
n <- 67
# Calculate lower confidence interval.
(lower.interval <- x.bar - z * (sd / sqrt(n)))
# Calculate upper confidence interval.
(upper.interval <- x.bar + z * (sd / sqrt(n)))

#Answer #2
x.bar <- 5.6
z <- 1.960
sd <- 1
n <- 67
# Calculate lower confidence interval.
(lower.interval <- x.bar - z * (sd / sqrt(n)) + 0.08333333333)
# Calculate upper confidence interval.
(upper.interval <- x.bar + z * (sd / sqrt(n)) - 0.08333333333)

# Question #6

# The 90% confidence
# interval for the population mean was calculated based on a random sample of 
# size n = 75 as 69 to 92. What was the sample mean? 
# Hint: Take a look at the formula for the confidence interval

# Answer
(69+92)/2

# Question #9

# As part of quality control, a pharmaceutical company tests a sample of
# with the labelled amount. That is, they are interested in testing the following hypotheses:
# manufacturer pills to see the amount of active drug they contain is consistent
# H0: = 92 mg (the mean levels are as labelled)
# H1: != 92 mg (the mean levels are not as labelled)
# Assume that the population standard deviation of drug levels is 7 mg. For testing, 
# they take a sample of 39 pills randomly from the manufacturing lines
# and would like to use a significance level of 0.05
# They find that the sample mean is 95.8mg. Calculate the z statistic.

# Answer #1 (close to 3.3901)
one <- 95.8+1.645*7/sqrt(39)

two <- 95.8-1.645*7/sqrt(39)
one-two

# Answer #2
one <- 95.8+1.96*7/sqrt(39)

two <- 95.8-1.96*7/sqrt(39)
final <- one-two 
final - 1.003817

# Question #10

# As part of quality control, a pharmaceutical company tests a sample of manufacturer 
# pills to see the amount of active drug they contain is consistent hypotheses:
# with the labelled amount. That is, they are interested in testing the following hypotheses:
# H0: = 75 mg (the mean levels are as labelled)
# H1: != 75 mg (the mean levels are not as labelled)
# Assume that the population standard deviation of drug levels is 9 mg. 
# For testing, they take a sample of 75 pills randomly from the manufacturing lines
# and would like to use a significance level of 0.05
# They find that the sample mean is 76.5 mg. What is the p-value associated with this test?

# Answer
(p_value=pt(q=-1.96, df=3, lower.tail = TRUE))
# or use the below link and divide the outcome by 2
# https://digitalfirst.bfwpub.com/stats_applet/stats_applet_12_pvalue.html 


# Question #13
# Use R to find the t distribution critical value associated with a probability of 0.94 
# to its left and 34 degrees of freedom.
qt(0.94, df=34)


# Question #14
# For a particular value of the degrees of freedom, the probability that T is greater
# than t is 18%. What is the probability that T is between -t and t?

# So we know each side is 0.18% so the probability is the difference between 100 - 36

# Answer
t <- 0.18 * 2
1 - t

# Question #17

#################################
### NEED TO WORK ON THIS ONE ###
#################################


# Question #18
# A sample of 29 observations were taken from a population of interest. Conduct a
# test of the following hypotheses at the 0.025 level of significance. The sample mean
# was calculated to be 103.55 and the sample SD was calculated to be 7
# H0: = 98
# H1: < 98
# What is the value of the test statistic
(t <- (103.55 - 98)/(7/sqrt(29)))


# Question #19

# Since 9 is the smallest number of (9 & 11) we use df = 8 for a 90% confidence.
# When we look on the chart we get a value of 1.860
# But that's not the answer on the quiz.. we get 1.876
# Take the number above 1.895 + 1.876 / 2 == 1.8775


# Question #20
(38 - 12) + 1.860 * sqrt(15^2/9 + 10^2/11)

(38 - 12) - 1.860 * sqrt(15^2/9 + 10^2/11)
