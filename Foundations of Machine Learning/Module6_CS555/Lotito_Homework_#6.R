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

View(body.temp)

# (2) Summarize the data relating to body temperature level 
# (i.e., the variable you created above) by sex. 

temp.gender

addmargins(prop.table(table(body.temp$temp_level, body.temp$sex),2))


# (3) Calculate the risk difference for high body temperature level between men and women.  
# Formally test (at the alpha=0.05 level) whether the proportion of people with 
# higher body temperatures (greater than or equal to 98.6) 
# is the same across men and women based on this effect measure. 
# You should be showing all 5 steps in the 5-step recipe for testing. 

prop.males.above <- temp.gender[2,1]/colSums(temp.gender)[1]
prop.males.above

prop.females.above <- temp.gender[2,2]/colSums(temp.gender)[2]
prop.females.above

#risk difference
(risk_dif <- 0.2153846 - 0.5384615)


test.stat <- (prop.females.above - prop.males.above) / (sqrt((risk.dif*(1-risk.dif)*((1/65)+(1/65)))))
test.stat

prop.test(x = temp.gender[1,], n=colSums(temp.gender), correct=FALSE, alternative ='two.sided' )



# (4) Perform a logistic regression with sex as the only explanatory variable. 
# Formally test (at the alpha=0.05 level) 
# if the odds of having a temperature greater than or equal to 98.6 
# is the same between males and females.   
# Again, please show all 5 steps. 
# Additionally, include the odds ratio for sex and the associated 95% 
# confidence interval in your summary, and interpret the value of the odds ratio.  
# Lastly, what is the c-statistic for this model? 
library(ROCR)
#create dummy variables
body.temp$men_dummy <- ifelse(sex== 1,1,0)
body.temp$women_dummy <- ifelse(sex== 2, 1, 0)

m <- glm(body.temp$temp_level ~ body.temp$sex, family=binomial )
summary(m)

library(pROC)
exp(cbind(OR = coef(m),
          confint.default(m, level = 0.95)))

body.temp$prob_temp <- predict(m, type='response')
g <- roc(body.temp$temp_level ~ body.temp$prob_temp)
g

# (5) Perform multiple logistic regression predicting body temperature level from sex and heart rate.  
# Briefly summarize the output from this model (no need to go through all 5 steps).  
# Give the odds ratio for sex. Also, report the odds ratio for heart rate (for a 10-beat increase).  
# What is the c-statistic of this model?  

library(aod)

m1 <- glm(body.temp$temp_level ~ body.temp$sex + body.temp$Heart.rate, family = binomial)
summary(m1)

wald.test(b=coef(m1), Sigma=vcov(m1), Terms = 2:3)

exp(cbind(OR = coef(m1),
          confint.default(m1, level = 0.95)))

exp(cbind(OR = coef(m1)*10,
          confint.default(m1, level = 0.95)*10))

exp(coef(m1)[3]*10)

body.temp$prob.temp <- predict(m, type='response')

g2 <- roc(body.temp$temp_level ~body.temp$prob.temp)
g2

# (6) Which model fit the data better?  Support your response with evidence from your output.  
# Present the ROC curve for the model you choose.
plot(g, main="Roc Curve", col = 'Blue')

?plot
