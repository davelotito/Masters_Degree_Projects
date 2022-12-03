# David Lotito
# MET CS 555


library(car)
library(emmeans)

# Save the data to a CSV file and read into R for analysis. 
setwd('/Users/davelotito/Code/Masters_Degree_Projects/Foundations of Machine Learning/Module5_CS555/')
students <- read.csv("Assignment5_Data.csv", header = T)

attach(students)

nrow(students)

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

# check to see if the variable group is a factor variable.
is.factor(group)

group <- factor(group)

#Get test statistic
qf(.95, df1=2, df2=42)

#Compute one way ANOVA test
(m <- aov(iq~group, data=students))

summary(m)

#Tukeys Test of Honest Significance

emm_options(contrasts=c("contr.treatment", "contr.poly"))

emmeans(m, specs = "group", contr="pairwise", adjust='tukey')

# (3)	Create an appropriate number of dummy variables for student group and re-run the one-way ANOVA 
# using the lm function with the newly created dummy variables.  
# Set chemistry students as the reference group.  
# Confirm the results are the same (specifically point out test statistics, p-values, etc. that show the results are equivalent).  
# What is the interpretation of the beta estimates from the regression model? 

students$g0 <- ifelse(group=='Chemistry student', 1, 0) #don't need to create, but did for consistency
students$g1 <- ifelse(group=='Math student', 1, 0)
students$g2 <- ifelse(group=='Physics student', 1, 0)
(model_1 <- aov(lm(iq ~ students$g1 + students$g2)))


summary(lm(iq ~ group, data=students))
summary(lm(iq ~ students$g1 + students$g2))


summary(lm(formula = iq ~ students$g2 + students$g1, data = students))



# (4)	Re-do the one-way ANOVA adjusting for age (ANCOVA).  
# Focus on the output relating to the comparisons of test score by student type.  
# Explain how this analysis differs from the analysis in step 2 above 
# (not the results but how does this analysis differ in terms of the questions it answers as opposed to the one above).  
# Did you obtain different results?  
# Summarize briefly (no need to go through the 5 –step procedure here).   
# Lastly, present the least square means and interpret these.  

(model_3 <- Anova(lm(iq~group + age), type=3))

my.model<-lm(iq ~ group + age,  data = students)

lsmeans(my.model, ~group)

























