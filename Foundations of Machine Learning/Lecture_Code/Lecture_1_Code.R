## Lecture Code Practice

mean <- 266 
sd <-  9

pnorm(275, mean, sd) - pnorm(257, mean,sd)

pnorm(284, mean, sd) - pnorm(248, mean,sd)


(1 - pnorm(0.1) ) * 100










# Question #3
# The number of customers entering a local restaurant is shown below.
# What is the relative frequency of customers entering at 9pm

# Step #1: Add up all the frequencies which == 350
# Step #2 Divide population of interest by the total number

# So our answer is 
70/350 # 20%

# Question # 4
# What is the mean of the numbers below
numbers <- c(34 , 10 , 2 , 52 , 67 , 13)

mean(numbers)

# Question #5
# What is the median of the numbers below

numbers <- c(71 , 95 , 4 , 16 , 54 , 100)
median(numbers)

# Question #6
# Which of the following are the best measures for describing the center if the
# distribution is skewed?

# Answer: The median since it ISN'T influenced by outliers

# Question #9
# Assume that the number of days from conception to birth is normally distributed with
# a mean of 278 days and a SD of 6 days. What % of pregnancies last more than 40 weeks(280 days)

# So our answer is 
(value <- (280-278)/6) #0.3


# Question #10 Quiz
#Assume that the number of days from conception to birth is normally 
#distributed with a mean of 252 days and a standard deviation of 6 days. 
#What percentage of pregnancies last more than 39 weeks (273 days)?

# So our answer is 
value <- (252-273)/6
pnorm(value) * 100

# Question #11 Quiz
# Assume that the number of days from conception to birth is normally distributed with a
# mean of 266 days and a standard deviation of 9 days. 
# What is the probability that my pregnancy will be between 257 and 275 days? 

(257-266)/9
(275-266)/9
# Now pull up the z-score table on Google and find -1 and 1 respectively.
# 1.0 == .8413
# -1.0 == .1587

# So our answer is 
0.8413 - 0.1587

# Question #12 Quiz
# The heights of women aged 20 to 29 aapprox follow a normal distribution with 
# a mean of 64 inches and a standard deviation of 1.83 inches. 
#What percent of young women are between 60.34 and 67.66 inches?

# Answer this the exact same as above question
(60.34-64)/1.83
(67.66-64)/1.83

# So our answer is 
0.9772 - 0.0228

# Question #13
# A variable is normally distributed. The proportion of observations that are GREATER than a is 3%.
# What is the proportion of observations that is between -a and a

# We know that >a == 3% and we have two sides so we split 3% in half to 1.5% and 1.5% respectively
# 100 - 3 - 1.5 - 1.5 = 94
# OR we can use z-score table and find 1.5 -> 0.03 which is ~94%

# Question #14
# If the variable below is normally distributed with a mean of 0 and a SD of 1
# and the proportion of observations that are GREATER than a is 3%. Find the value of a

qnorm(0.03, mean = 0, sd = 1)

# Question #15
# x is normally distributed. The probability that X is between -t and t is 88%. Find the probability
# that x is less than 1

# So our answer is 
prob <- 1+0.88
prob/2


# Question #19 Quiz
# Let's assume that the distribution of gas on a given day in a town is normally 
# distributed with mean of $5.1816 with a standard deviation of $0.6832. What proportion
# of gas stations are charging less than $4.1983?
pnorm(4.1983, mean=5.1816, sd=0.6832) * 100



# Question #20 Quiz
# Find the probability that the sample mean is less than 878 if a sample size of 97
# is taken from a population with a mean of 1191 and a standard deviation of 307.
pnorm(878, mean=1191, sd=307/sqrt(97))

