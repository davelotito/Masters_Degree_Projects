# David Lotito
# CS555 - Term Project Fall 2022

setwd('/Users/davelotito/Code/Masters_Degree_Projects/Foundations of Machine Learning/Term_Project')

heart_data <- read.csv('heart.csv', header = T)

View(heart_data)

hist(Age)

## We want to understand if the mean age is equal to 50
# H0 mean = 50
# H1 mean != 50

sd(Age)


(z <- (mean(Age) - 50) / (sd(Age)/sqrt(nrow(heart_data))))

2*pnorm(-abs(z))


cor(heart_data$Age, heart_data$RestingBP)

r <- c(0.2543994)
#beta_1 calculation
s.x <- c(sd(heart_data$Age))
s.y <- c(sd(heart_data$RestingBP))
beta.1 <- r * (s.y/s.x)
beta.1

#beta_0 calculation
x.bar <- c(mean(heart_data$Age))
y.bar <- c(mean(heart_data$RestingBP))
beta.0 <- y.bar - (beta.1 * x.bar)
beta.0

plot(heart_data$RestingBP ~ heart_data$Age, main = "Scatterplot of Age & Resting Blood Pressure", xlab= "Age",
     ylab = "Resting Blood Pressure Level", xlim = c(25, 80), ylim = c(50, 225), pch=1, col='blue')
abline(m)
anova(m)

(my.model <- lm(heart_data$RestingBP ~ heart_data$Age))
abline(my.model, col = "red")

(residual <- resid(my.model))
hist(residual, main= "Histogram of Residuals for RestingBP on Age")

anova(my.model)

summary(my.model)

qf(.90, df1=1, df2 = 916)

(F_Statistic <- 20342.7/320.9)


confint(my.model, level=0.90)


# Multiple linear regression

(m2 <- lm(formula = heart_data$RestingBP ~ (heart_data$Age + heart_data$MaxHR + 
                                 heart_data$Cholesterol), data = heart_data))

qf(.95, df1=3, df2 = 915)

anova(m2)

summary(m2)

modelsum <- summary(m2)

#GLobal F-Stat
(F_stat <- modelSum$fstatistic[1])


residual_2 <- resid(m2)
hist(residual_2, main= "Histogram of Residuals for the Regression of Model")
plot(fitted(m2), resid(m2), axes = TRUE, frame.plot=TRUE, xlab="Fitted Values", ylab= "Residual", main = "Whole Model Residual by Fitted", col = "Blue")

abline(h=0, col="Red")

which(cooks.distance(m2) > (4/nrow(heart_data)))












