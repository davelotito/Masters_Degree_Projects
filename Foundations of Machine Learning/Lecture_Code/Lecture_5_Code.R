

# Test Yourself (Multipart) 5.2
# Let's return to the example involving SAT scores where we took independent 
# simple random samples of 5 students who took the in school SAT prep course, 
# 5 students who had private SAT prep instruction, 
# and 5 students who did not take any SAT prep course. 
# Their SAT scores on the math portion are below as are some descriptive statistics by group. 
# Fill in the ANOVA table below using the values calculated earlier:

# Set up the hypotheses and select the alpha level

# H0:μIn-School = μPrivate = μNone (All underlying population means are equal)
# H1:μIn-School = μPrivate = μNone (Not all of the underlying population means are equal)

# α = 0.01

# Select the appropriate test statistic

MSB <- 195
MSW <- 333.33

# F = MSB/MSW


# State the decision rule

(test_f_stat <- qf(.99, df1=2, df2=12)) # alpha of 0.01

# Reject H0 if F ≥ 6.926608. Otherwise, do not reject


# Compute the test statistic

# Test Statistic - F Value
(F <- MSB/MSW)

F >= test_f_stat

# Conclusion

# Do not reject H0 since 0.59 is not ≥ 6.93
# We do not have evidence at the α=0.01 level that there is a difference 
# in mean SAT scores by type of SAT preparation (here, p =0.57
# as calculated using a software program).

###########################################################################
###########################################################################
###########################################################################

# The global F test above is used to test whether there are differences between the underlying 
# group means. If the global F
# test indicates that there are group differences (if the null hypothesis is rejected), 
# then it is often of interest to take the additional steps needed to determine which of the population 
# group means are different.

# To determine where the differences lie, we perform testing on each pairwise comparison of interest. 
# In order to test if μi=μj, for example, we use a t-statistic. Below is a problem with an example.


# Test Yourself (Multipart) 5.3
# Use the raw data and the ANOVA table below to test whether the mean distance of Callaway 
# brand balls is different from the mean distance of Nike brand golf balls.  
# Formally set up this test and use α=0.05
# α = 0.05

# Set up the hypotheses and select the alpha level

# H0:μCallaway = μNike
# H1:μCallaway ≠ μNike

# State the decision rule

# Two tailed t-test

# Find T
(a = 0.05/2)
(test_stat <- abs(qt(a, df=12)))

# OR

# Find T
(alpha <- 0.05)
(t.critical <- qt(1-(alpha/2), df=12))


# Decision Rule: Reject H0 if t ≥ 2.18 or if t ≤ −2.179

# Compute Test Statistic

(t <- (285 - 260)/sqrt(62.5 * (1/5 + 1/5)))

pairwise.t.test(distance, brand, p.adj="none")


# Conclusion: If TRUE then we reject H0 else accept H1 the alternative 
t >= test_stat


# Reject H0 since 5 > 2.178813. We have significant evidence at the alpha 0.05 level that μCallaway ≠ μNike

(Upper <- (285 - 260)+2.18*sqrt(62.5 * (1/5 + 1/5)))

(Lower <- (285 - 260)+2.18*sqrt(62.5 * (1/5 + 1/5)))

# We are 95% confident that the true increase in distance for Callaway is between 14.1 yards & 35.9 yards



# Family Wise Error Rate (FWER)


alpha <- 0.01
num_tests <- 10


(FWER <- abs(1 -(1 - alpha)^num_tests)*100)

# Correct alpha level with Bonferoni Test
(bonferoni_corrected_alpha <- alpha/num_tests)

# Now rerun with new alpha

(FWER <- abs(1 - (1 - bonferoni_corrected_alpha)^num_tests))


t.test(Participants,conf.level=0.90)
t.test(97.5, 12)




### ### ### ### ### ### 
### QUIZ 5 PROBLEMS ###
### ### ### ### ### ### 


qf(.95, df1 = 8, df2 = 227)



9*(9-1)/2




