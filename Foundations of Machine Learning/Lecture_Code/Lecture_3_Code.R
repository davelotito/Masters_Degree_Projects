# Lecture Code Practice














# Question #8

# A least-squares simple linear regression model was fit predicting duration (in minutes) 
# of a dive from depth of the dive (in meters) from a sample of 45 penguins' 
# diving depths and times.
# Calculate the F-statistic for the regression by filling in the ANOVA table.

total_SS <- 129638.3374

regression_df <- 1

residual_df <- 43

residual_MS <- 1395.2428

(residual_SS <- residual_MS * residual_df )

(regression_SS <- total_SS - residual_SS)

(regression_MS <- regression_SS / regression_df)

(f_statistic <- regression_SS / residual_MS)



# Question #9

# A least-squares simple linear regression model was "t predicting duration (in minutes) 
# of a dive from depth of the dive (in meters) from a sample of 40 penguins' 
# diving depths and times.
# Calculate the R-squared value for the regression by "lling in the ANOVA table.


residual_MS <- 1979.33
residual_df <- 38
total_SS <- 374553.247

(residual_SS <- residual_MS * residual_df )

(regression_SS <- total_SS - residual_SS)

(regression_MS <- regression_SS / regression_df)

(r_squared <- regression_MS / total_SS)



# Question #11

1000 - 38(20) = 240


# Question #12
data2 <- read.csv("/Users/davelotito/Desktop/test_data.csv", header = T)


cor(data2)