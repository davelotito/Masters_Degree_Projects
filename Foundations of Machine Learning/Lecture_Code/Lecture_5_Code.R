

# Test Yourself (Multipart) 5.3
# Use the raw data and the ANOVA table below to test whether the mean distance of Callaway 
# brand balls is different from the mean distance of Nike brand golf balls.  
# Formally set up this test and use α=0.05
# α = 0.05

# Set up the hypotheses and select the alpha level

# H0:μCallaway = μNike
# H1:μCallaway ≠ μNike

# State the decision rule

# Two tailed T t-test
(a = 0.05/2)

# Find T
(test_stat <- abs(qt(a, df=12)))


# Decision Rule: Reject H0 if t ≥ 2.18 or if t ≤ −2.179

# Compute Test Statistic

(t <- (285 - 260)/sqrt(62.5 * (1/5 + 1/5)))

# Conclusion: If TRUE then we reject H0 else accept H1 the alternative 
t >= test_stat






