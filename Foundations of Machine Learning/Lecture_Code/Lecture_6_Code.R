























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
# What is the z-statistic for testing the null hypothesis of H0 : p1 = p2 , 
# where p1 is the proportion of men living in an institution after a stroke, and p2 for women?


x <-(22+6)/(63+45)
((22/63)-(6/45)) / (sqrt((x*(1-x)*((1/63)+(1/45)))))


