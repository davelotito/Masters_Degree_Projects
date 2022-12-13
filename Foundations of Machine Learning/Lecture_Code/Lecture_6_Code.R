

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

(men_dif <- 10/47)
(women_dif <- 24/51)

(women_dif - men_dif)*100



# Question #2

#A study of stroke patients who survived 6 months after the stroke found that 6/45 men and 
# 22/63 women lived in an institution (e.g., nursing home or assisted living facility). 
# What is the odds ratio of living in an institution between men and women 
# (using men as the reference group)?

women <- 20/58
men <- 8/50

(women/(1-women)) / (men/(1-men))


# Question #3

# A study of stroke patients who survived 6 months after the stroke found that 
# 6/45 men and 22/63 women lived in an institution (e.g., nursing home or assisted living facility). 
# What is the risk ratio of living in an institution after a stroke between men and women 
# (using men as the reference group)?


(11/56) / (2/47)


# Question #4

# A study of stroke patients who survived 6 months after the stroke found that 
# 6/45 men and 22/63 women lived in an institution (e.g., nursing home or assisted living facility). 
# What is the z-statistic for testing the null hypothesis of H0 : p1 = p2 , 
# where p1 is the proportion of men living in an institution after a stroke, and p2 for women?


x <-(22+6)/(51+50)
(z <- ((22/51)-(6/50)) / (sqrt((x*(1-x)*((1/51)+(1/50))))))

# Question #5

# A study of stroke patients who survived 6 months after the stroke found that 6/45 men 
# and 22/63 women lived in an institution (e.g., nursing home or assisted living facility). 
# What is the 95% confidence interval for the risk difference in living 
# in an institution between men and women (with men as the reference group)?


p1 <- (22/63)
p2 <- (6/45)
(((22/63)-(6/45)) - 1.96 * sqrt(((p1*(1-p1))/63) + ((p2*(1-p2))/45)))*100
(((22/63)-(6/45)) + 1.96 * sqrt(((p1*(1-p1))/63) + ((p2*(1-p2))/45)))*100


# Question #7

# We have 30-day follow-up data on 350 stroke patients and want to investigate whether the risk 
# of recurrent stroke and/or death depends on the type of stroke (cerebral embolism or not). 
# The results of the simple logistic regression of the dummy variable for cerebral embolism 
# (1 = yes, 0 = no) are shown below. Use the output to calculate the odds ratio for 
# recurrent stroke and/or death for those who had a cerebral embolism versus those who did not?

exp(2.2)

# Question #8

# We have 30-day follow-up data on 350 stroke patients and want to investigate whether 
# the risk of recurrent stroke and/or death depends on the type of stroke (cerebral embolism or not) 
# and age. The results of the multiple logistic regression of the dummy variable for cerebral embolism 
# (1 = yes, 0 = no) and age are shown below. 
# What is the risk of recurrent stroke/death for a patient without a cerebral 
# embolism who is 60 years of age?

exp(-16.3+0.2*54) / (1 + exp(-16.3+0.2*54))

# Question #9

# We have 30-day follow-up data on 350 stroke patients and want to investigate 
# whether the risk of recurrent stroke and/or death depends on the type of stroke 
# (cerebral embolism or not) and age. 
# The results of the multiple logistic regression of the dummy variable 
# for cerebral embolism (1 = yes, 0 = no) and age are shown below. Calculate the
#  odds ratio comparing the odds of recurrent stroke/death for a patient who is 65 versus 64, 
# controlling for cerebral embolism.

exp(0.23)*(52-51)

# Question #10

# We have 30-day follow-up data on 350 stroke patients and want to investigate whether the 
# risk of recurrent stroke and/or death depends on the type of stroke (cerebral embolism or not) 
# and age. The results of the multiple logistic regression of the dummy variable for 
# cerebral embolism (1 = yes, 0 = no) and age are shown below. 
# Calculate the odds ratio comparing the odds of recurrent stroke/death for 
# a patient who is 65 versus 55, controlling for cerebral embolism.

b1 <- 0.28*(67-38)

exp(b1)

# Question #11

# We have 30-day follow-up data on 350 stroke patients and want to investigate whether the risk 
# of recurrent stroke and/or death depends on the type of stroke (cerebral embolism or not) and age. 
# The results of the multiple logistic regression of the dummy variable for cerebral embolism 
# (1 = yes, 0 = no) and age are shown below. Calculate the 95% confidence interval for the odds ratio 
# comparing the odds of recurrent stroke/death for patients with a cerebral 
# embolism versus those without, controlling for age.

exp(-0.4627-1.960*0.4329)
exp(-0.4627+1.960*0.4329)



# Question #16

# A study was conducted to determine key predictors of chromosomal fetal abnormalities. 
# Using the multiple logistic regression model and a cut off selected by the investigator, 
# 78 fetuses were predicted by the model of having an abnormality. 
# However, only 14 of the 78 that were predicted to have the abnormality actually did. 
# Of the 122 fetuses that the model predicted did not have the abnormality, 6 of them actually did. 
# Construct a 2 by 2 table of these results to help you calculate 
# the sensitivity of the model using this cutoff.

abnormality_1 <- 23
abnormality_2 <- 19

total <- abnormality_1 + abnormality_2


(abnormality_1 / total)*100



abnormality_1 <- 14
abnormality_2 <- 6

total <- abnormality_1 + abnormality_2


(abnormality_1 / total)*100



