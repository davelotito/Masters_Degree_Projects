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


prop.test(x = temp.gender[1,], n=colSums(temp.gender), correct=FALSE, alternative ='two.sided' )

# (4) Perform a logistic regression with sex as the only explanatory variable. 
# Formally test (at the alpha=0.05 level) 
# if the odds of having a temperature greater than or equal to 98.6 
# is the same between males and females.   
# Again, please show all 5 steps. 
# Additionally, include the odds ratio for sex and the associated 95% 
# confidence interval in your summary, and interpret the value of the odds ratio.  
# Lastly, what is the c-statistic for this model? 

#create dummy variables
body.temp$men_dummy <- ifelse(sex== 1,1,0)
body.temp$women_dummy <- ifelse(sex== 2, 1, 0)

m <- glm(body.temp$temp_level ~ body.temp$sex, family=binomial )
summary(m)


exp(cbind(OR = coef(m),
          confint.default(m)))

body.temp$prob_temp <- predict(m, type='response')
g <- roc(body.temp$temp_level ~ body.temp$prob_temp)
g


