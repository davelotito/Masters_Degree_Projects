# David Lotito
# MET CS 555

# Save the data to a CSV file and read into R for analysis. 
setwd('/Users/davelotito/Code/Masters_Degree_Projects/Foundations of Machine Learning/Module5_CS555/')
students <- read.csv("Assignment5_Data.csv", header = T)

attach(students)

summary(students)
aggregate(iq, by=list(group), summary)
aggregate(age, by=list(group), summary)

#Graphical summary
par(mfrow = c(1, 2))
boxplot(iq~group, data=students, main="IQ by Student Group", xlab="group",  ylab="IQ")
boxplot(age~group, data=students, main="Age by Student Group", xlab="group",  ylab="Age")
par(mfrow = c(1, 1))









































