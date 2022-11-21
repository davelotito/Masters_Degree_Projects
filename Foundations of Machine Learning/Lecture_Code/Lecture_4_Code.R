
data <- read.csv("https://raw.githubusercontent.com/kiat/R-Examples/master/Datasets/CEO_salary.csv")
attach(data)
salary1 <- salary/1000
data1 <- data.frame(age, height, salary1)
cor(data1)
pairs(data1)



m <- lm(salary1 ~ age + height)

summary(m)

resid(m)

fitted(m)




##########
# Quiz #4
##########

# Question #3

resting_heart_rate_independent_var <- 70

diastoic_blood_pressure_dependent_var <- 60

# Equation == ÿ = 1.2x - 20

-(1.2*(70) - 20 - diastoic_blood_pressure_dependent_var)

# Question #7

# Calculate the ANOVA Table


# Question #8
residual <- 119
total <- 157

(regression <- total-residual)/total*100

# Question #11

n <- 20
k <- 2

t <- 1.5

df <- 20 - k - 1

(test <- abs(qt(0.05, df)))


t > test

# Question #12

B0_intercept <- 11.1337

B1_slope <- -0.0618

B2_slope <- 0.0344

x1_gestage <- 35

x2_estrilo <- 40

# Regression Equation

B0_intercept + B1_slope * x1_gestage + B2_slope * x2_estrilo






