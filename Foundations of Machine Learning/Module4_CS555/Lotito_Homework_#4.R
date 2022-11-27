# David Lotito
# MET CS 555

# Save the data to a CSV file and read into R for analysis. 
setwd('/Users/davelotito/Code/Masters_Degree_Projects/Foundations of Machine Learning/Module4_CS555/')
data <- read.csv("Assignment4_Data.csv", header = T)

attach(data)
View(data)

#1. To get a sense of the data, generate a scatterplot to examine the association between 
#prestige score and years of education. Briefly describe the form, direction, and strength of 
#the association between the variables. Calculate the correlation.

plot(Education.Level..years., Prestige.Score, main = "Scatterplot of Prestige Score vs. Education")
abline(score.education)

cor(Education.Level..years., Prestige.Score)

# 2) Perform a simple linear regression with prestige score and years of education, 
# and briefly summarize your conclusions (no need to do the 5-step procedure here).  
# Generate a residual plot.  
# Assess whether the model assumptions are met.  
# Are there any outliers or influence points?  
# If so, identify them by ID and comment on the effect of each on the regression. 

(score.education <- lm(Prestige.Score ~ Education.Level..years.))
score.education

summary(score.education)

Residual <- resid(score.education)
plot(Education.Level..years., Residual, main = "Residual Plot for the Regression of Prestige Score on Education")
abline(0,0)

hist(Residual, main= "Histogram of Residuals for the Regression of Prestige Score on Education")

# Outliers 
outlier_iqr <- function(x){
  iqr <- IQR(x, na.rm = T, type = 7)
  q <- quantile(x)
  upper_bound <- q[4]+(iqr*1.5)
  lower_bound <- q[2]-(iqr*1.5)
  outliers <- which ((x > upper_bound) | (x < lower_bound))
  return(outliers)
}
outlier_iqr(Prestige.Score)
outlier_iqr(Education.Level..years.)

# Influence points
which(cooks.distance(score.education) > (4/nrow(data)))



# (3) Calculate the least squares regression equation that predicts prestige score from education, income, 
# and percentage of women.  
# Formally test (using the 5-step procedure) whether the set of these predictors are associated with prestige score 
# at the α = 0.05 level (Hint: You should be performing the global test).

(m <- lm(formula = Prestige.Score ~ (Education.Level..years. + Income.... +  Percent.of.Workforce.that.are.Women), data=data))

anova(m)

qf(.95, df1 = 3, df2 = 98)

(modelSum <- summary(m))

(F_stat <- modelSum$fstatistic[1])


# (4) If the overall model was significant, summarize the information about the contribution of each variable separately at the same 
# significance level as used for the overall model (no need to do a formal 5-step procedure for each one, just comment on the results of the tests).  
# Provide interpretations for any estimates (of the slopes) that are significant.   
# Calculate 95% confidence intervals for any estimates that are significant
(m1 <- lm(formula = Prestige.Score ~ Education.Level..years., data=data))
(m2 <- lm(formula = Prestige.Score ~ Income...., data=data))
(m3 <- lm(formula = Prestige.Score ~ Percent.of.Workforce.that.are.Women, data=data))
summary(m1)
qf(.95, df1 = 1, df2 = 100)
summary(m2)
summary(m3)

qf(.95, df1 = 3, df2 = 98)



# (5) Generate a residual plot showing the fitted values from the regression against the residuals.  
# Is the fit of the model reasonable? Are there any outliers or influence points?  

#plot residual vs. fitted value

resid <- resid(m)
hist(resid, main= "Histogram of Residuals for the Regression of Model")
plot(fitted(m), resid(m), axes = TRUE, frame.plot=TRUE, xlab="Fitted Values", ylab= "Residual", main = "Whole Model Residual by Fitted")
abline(h=0)

hist(Residual, main= "Histogram of Residuals for the Regression of Prestige Score on Education")

outlier_iqr(Income....)
outlier_iqr(Percent.of.Workforce.that.are.Women)
outlier_iqr(Education.Level..years.)
which(cooks.distance(m) > (4/nrow(data)))


