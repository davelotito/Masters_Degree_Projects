# David Lotito
# MET CS 555


library(car)
library(emmeans)

# Save the data to a CSV file and read into R for analysis. 
setwd('/Users/davelotito/Code/Masters_Degree_Projects/Foundations of Machine Learning/Module5_CS555/')
students <- read.csv("Assignment5_Data.csv", header = T)

attach(students)


# (1)	How many students are in each group?  
# Summarize the data relating to both test score and age by the student group (separately).  
# Use appropriate numerical and/or graphical summaries. 
sum(group == "Physics student")
sum(group == "Math student")
sum(group == "Chemistry student")

summary(students)
aggregate(iq, by=list(group), summary)
aggregate(age, by=list(group), summary)

#Graphical summary
par(mfrow = c(1, 2))
boxplot(iq~group, data=students, main="IQ by Student Group", xlab="group",  ylab="IQ")
boxplot(age~group, data=students, main="Age by Student Group", xlab="group",  ylab="Age")
par(mfrow = c(1, 1))

# (2)	Do the test scores vary by student group?  
# Perform a one way ANOVA using the aov or Anova function in R to assess.  
# Use a significance level of α=0.05. Summarize the results using the 5-step procedure.  
# If the results of the overall model are significant, perform the appropriate 
# pairwise comparisons using Tukey’s procedure to adjust for multiple comparisons and summarize these results.

#check to see if the variable group is a factor variable.
is.factor(group)

#Get test statistic
qf(.95, df1=3, df2=42)

#Compute one way ANOVA test
m <- aov(iq~group, data=students)
summary(m)

#Tukeys Test of Honest Significance
TukeyHSD(m)

emm_options(contrasts=c("contr.treatment", "contr.poly"))

emmeans(m, specs = "group", contr="pairwise", adjust='tukey')







































