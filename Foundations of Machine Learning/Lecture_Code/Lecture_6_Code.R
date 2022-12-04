

# Test Yourself 6.2

# In the previous season, a professional basketball player made just 55%
# of his free throw shots. After working specifically with a coach in the 
# off-season the player is optimistic that he has improved his free throw percentage. 
# In the first few games of the new season, the player made 24 of 40
# free throws. Calculate the 90%
# confidence interval for the proportion of free throws that the player makes.

conf_90 <- 1.645
conf_95 <- 1.960
conf_99 <- 2.576

(24/40-conf_90*sqrt(24/40*(1-24/40)/40))*100
(24/40+conf_90*sqrt(24/40*(1-24/40)/40))*100

# We are 90% confident that the true proportion of free throw shots that the player is between 47%
# and 73%








##################
### QUIZ #6 ######
##################

# Question #1

# A study of stroke patients who survived 6 months after the stroke found that 6/45 men and 
# 22/63 women lived in an institution (e.g., nursing home or assisted living facility). 
# What is the risk difference of living in an institution between men and women 
# (using men as the reference group)?

men_dif <- 6/45
women_dif <- 22/63

(women - men)*100



# Question #2

#A study of stroke patients who survived 6 months after the stroke found that 6/45 men and 
# 22/63 women lived in an institution (e.g., nursing home or assisted living facility). 
# What is the odds ratio of living in an institution between men and women 
# (using men as the reference group)?

women <- 22/63
men <- 6/45

(women/(1-women)) / (men/(1-men))


# Question #3

# A study of stroke patients who survived 6 months after the stroke found that 
# 6/45 men and 22/63 women lived in an institution (e.g., nursing home or assisted living facility). 
# What is the risk ratio of living in an institution after a stroke between men and women 
# (using men as the reference group)?


(22/63) / (6/45)


# Question #4

# A study of stroke patients who survived 6 months after the stroke found that 
# 6/45 men and 22/63 women lived in an institution (e.g., nursing home or assisted living facility). 
# What is the z-statistic for testing the null hypothesis of H0 : p1 = p2 , 
# where p1 is the proportion of men living in an institution after a stroke, and p2 for women?


x <-(22+6)/(63+45)
(z <- ((22/63)-(6/45)) / (sqrt((x*(1-x)*((1/63)+(1/45))))))

# Question #5

# A study of stroke patients who survived 6 months after the stroke found that 6/45 men 
# and 22/63 women lived in an institution (e.g., nursing home or assisted living facility). 
# What is the 95% confidence interval for the risk difference in living 
# in an institution between men and women (with men as the reference group)?


p1 <- (22/63)
p2 <- (6/45)
(((22/63)-(6/45)) - 1.96 * sqrt(((p1*(1-p1))/63) + ((p2*(1-p2))/45)))*100
(((22/63)-(6/45)) + 1.96 * sqrt(((p1*(1-p1))/63) + ((p2*(1-p2))/45)))*100


