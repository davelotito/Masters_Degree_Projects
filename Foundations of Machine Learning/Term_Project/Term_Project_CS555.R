# David Lotito
# CS555 - Term Project Fall 2022

setwd('/Users/davelotito/Code/Masters_Degree_Projects/Foundations of Machine Learning/Term_Project')

heart_data <- read.csv('heart.csv', header = T)

View(heart_data)

attach(heart_data)

hist(Age)

## We want to understand if the mean age is equal to 50
# H0 mean = 50
# H1 mean != 50

sd(Age)

z.test(Age, mu=50, alternative = 'two.sided')

(z <- (mean(Age) - 50) / (sd(Age)/sqrt(nrow(heart_data))))

2*pnorm(-abs(z))


(m <- lm(Age ~ RestingBP, data=heart_data))

quartiles <- quantile(Age, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(Age)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

Age <- subset(Age, Age > Lower & Age < Upper)

length(data_no_outlier)

plot(Age ~ RestingBP, main = "Scatterplot of Age vs. Resting Blood Pressure", xlab= "Resting Blood Pressure Level",
     ylab = "Age", xlim = c(65, 200), ylim = c(25, 80), pch=1, col='blue')
abline(m)
anova(m)

cor(Age, HeartDisease)

cor(Age, FastingBS)

cor(Age, Cholesterol)

cor(Age, RestingBP)

cor(Age, RestingBP)





























