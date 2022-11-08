# (1) Save the data to a CSV file and read into R for analysis. 
setwd('/Users/davelotito/Desktop/MET CS555 - Foundations of Machine Learning/Assignments/Assignment2')
cal_info <- read.csv("Calorie_Info.csv", header = T)

# 1) Summarize the data by whether children participated in the meal preparation or not.  
# Use an appropriately labelled table to show the results.  
# Also include a graphical presentation that shows the distribution of calories for participants vs. non-participants.  
# Describe the shape of each distribution and comment on the similarity (or lack thereof) 
# between the distributions in each group.
cal_info
aggregate(cal_info, by=list(cal_info$Participants, cal_info$Non_Participants), FUN=summary)

par(mfrow = c(1, 2))
hist(cal_info$Participant, main = "Histogram of Participants", col = blues9)
hist(cal_info$Non_Participants, main= "Histogram of Non-Participants", col = blues9)

# The shape of the two distributions are not normal distributions, we can see
# That these two distributions are not similar as the calories for non-participant spiked
# between 300-500 calories indicating that non-participants are likely to have more calories
# vs. the participants in the study. We can see that the participants had a lot of calories between
# 200-300 calories and then again at 400-500 calories. From our initial summary we can see that the sum of 
# total calories for participant vs. non-participant shows  us that Non_Participants consumed less overall calories
# but we also had less observations with 3 missing.

