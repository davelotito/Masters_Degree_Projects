# David Lotito
# MET CS 555

# (1) Save the data to a CSV file and read into R for analysis. 
setwd('/Users/davelotito/Desktop/MET CS555 - Foundations of Machine Learning/Assignments/Assignment3')
data <- read.csv("dataset_fish_mercury.csv", header = T)

head(data)

# (1) To get a sense of the data, generate a scatter plot (using an appropriate window, label the axes, and title the graph).  
# Consciously decide which variable should be on the x-axis and which should be on the y-axis.  
# Using the scatter plot, describe the form, direction, and strength of 
# the association between the variables.

attach(data)

plot(Number.of.meals.with.fish, Total.Mercury.in.mg.g, main = "Scatterplot of Number of Fish Meals vs. Mercury Levels", xlab= "Number of Fish Meals",
     ylab = "Mercury Levels", xlim = c(0, 20), ylim = c(0, 10), pch=1, col='blue')


#(2) Calculate the correlation coefficient. What does the correlation tell us? (Q3 - 2 points)
cor(data)

#(4) Find the equation of the least squares regression equation, and write out the equation. Add the
#regression line to the scatter plot you generated above. (Q4 - 4 points)
#beta_1 calculation
r <- c(0.6991094)
s.x <- c(sd(Number.of.meals.with.fish))
s.y <- c(sd(Total.Mercury.in.mg.g))
beta.1 <- r * (s.y/s.x)
beta.1

#beta_0 calculation
x.bar <- c(mean(Number.of.meals.with.fish))
y.bar <- c(mean(Total.Mercury.in.mg.g))
beta.0 <- y.bar - (beta.1 * x.bar)
beta.0

#setting up linear model to put in regression line
(my.model <- lm(data$Total.Mercury.in.mg.g ~ data$Number.of.meals.with.fish))
abline(my.model, col = "red")

# (5) Calculate the ANOVA table AND the table which gives the standard error of  .  
# Formally test the hypothesis that  = 0 using either the F-test or the t-test at the   level. 
# Either way, present your results using the 5-step procedure, as described in the course notes.
anova(my.model)

summary(my.model)

#calculate the F-distribution with 1, 98 degrees of freedom and associated with alpha = 0.10
qf(.95, df1=1, df2 = 98)

confint(my.model, level=0.90)

