# 1) Summarize the data by whether children participated in the meal preparation or not.  
# Use an appropriately labelled table to show the results.  
# Also include a graphical presentation that shows the distribution of calories for participants vs. non-participants.  
# Describe the shape of each distribution and comment on the similarity (or lack thereof) 
# between the distributions in each group.

Participants <- c(230.16,210.99,288.73,590.28,582.59,635.21,249.86,441.66,572.43,357.78,
                  396.79,298.38,282.99,368.51,388.59,256.32,
                  408.8,424.94,477.96,428.74,432.52,428.27,
                  596.79,456.30,446.38)

Non_Participants <- c(614.61,503.46,425.22,688.77,184,299.73,350.65,394.94,261.55,295.28,
                     139.69,462.78,179.59,301.75,436.58,371.39,
                     469.02,378.09,287.31,448.55,332.64,403.98)

summary(Participants)

length(Participants)

sd(Participants)

summary(Non_Participants)

length(Non_Participants)

sd(Non_Participants)

hist(Participants, main = "Histogram of Participants", col = blues9)
hist(Non_Participants, main= "Histogram of Non-Participants", col = blues9)

# The shape of the two distributions are not normal distributions, we can see
# That these two distributions are not similar as the calories for non-participant spiked
# between 300-500 calories indicating that non-participants are likely to have more calories
# vs. the participants in the study. We can see that the participants had a lot of calories between
# 200-300 calories and then again at 400-500 calories. From our initial summary we can see that the sum of 
# total calories for participant vs. non-participant shows  us that Non_Participants consumed less overall calories
# but we also had less observations with 3 missing.

# (2) Does the mean calorie consumption for those who participated in the 
# meal preparation differ from 425?  
# Formally test at the alpha = 0.05 level using the 5 steps outlined in the module. 

# Steps
# 1) Set up the hypotheses and select the alpha level
# 2) Select the appropriate test statistic
# 3) State the decision rule
# 4) Compute the test statistic and the associated p-value
# 5) State your conclusion
abs(qt(0.05,24))

t.test(Participants,mu=425,alternative="two.sided")

# (3) Calculate a 90% confidence interval for the mean calorie intake for participants in the meal 
# preparation. Interpret the confidence interval. 
abs(qt(0.05,45))

t.test(Participants,conf.level=0.90)

# (4) Formally test whether or not participants consumed more calories than non-participants at the 
# alpha = 0.05 level using the 5 steps outlined in the module.
t.test(Participants,Non_Participants)











