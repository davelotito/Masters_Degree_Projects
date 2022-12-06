# David Lotito
# MET CS 555
# Assignment #6

# Save the data to a CSV file and read into R for analysis. 
setwd('/Users/davelotito/Code/Masters_Degree_Projects/Foundations of Machine Learning/Module6_CS555/')
body.temp <- read.csv("body_temp.csv", header = T)


View(body.temp)

attach(body.temp)

# 1) We are interested in whether the proportion of men and women with 
# body temperatures greater than or equal to 98.6 degrees Fahrenheit are equal. 
# Therefore, we need to dichotomize the body temperature variable. 
# Create a new variable, called “temp_level” 
# in which temp_level = 1 if body temperature >= 98.6 
# and temp_level=0 if body temperature < 98.6. 

body.temp$temp_level <- ifelse(body.temp$temp >=98.6,
                               c(1),
                               c(0))

temp.gender <- table(Temp = body.temp$temp_level, Gender=sex)
rownames(temp.gender) <- c("Below Average", 'Above Average')
colnames(temp.gender) <- c('Male', 'Female')

temp.gender
























