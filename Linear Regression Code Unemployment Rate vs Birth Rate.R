library(tidyverse)
install.packages("rcompanion")
library(rcompanion)
library(car)
library(rstatix)
library(dplyr)


#Loading the Data Set
global_edu <- read.csv("Cleaned Global Education.csv")
View("Cleaned Gloabal Education.csv")

#box plot to check for outliers 
boxplot(global_edu$Birth_Rate, global_edu$Unemployment_Rate,
        names = c("Birth Rate", "Unemployment Rate"),
        main = "Boxplot Comparing the Unemployment Rate vs the Birth Rate",
        ylab = "Rate Value")
#there are 4 outliers for the unemployment rate, will check if these are extreme values
#there are no outliers for the Birth Rate

birth_outliers <- global_edu %>%
  identify_outliers(Birth_Rate)
birth_outliers
#no outliers for birth rate

unemployment_outliers <- global_edu %>%
  identify_outliers(Unemployment_Rate)
unemployment_outliers
#the 4 outliers are not extreme, can continue with the data

#Testing Assumptions
#Check Linearity (scatter plot)
var <-global_edu[,c("Unemployment_Rate", "Birth_Rate")]


plot(x = var$Unemployment_Rate, y= var$Birth_Rate,
     xlab = "Unemployment Rate",
     ylab = "Birth Rate",
     main = "Unemployment Rate vs Birth Rate")

#Relationship between the Birth Rate and Unemployment Rate is non-linear
#Applying a nonlinear transformation bc linearity assumption is violated. 
#I will use the log to transform the data
#There are zeros, doing the log of 0, gets infinite value, will remove the zeros

# Count how many zeros are in the Birth_Rate column
br_zero_count <- sum(global_edu$Birth_Rate == 0, na.rm = TRUE)
br_zero_count
#1 zero in birth rate

ur_zero_count <- sum(global_edu$Unemployment_Rate == 0, na.rm = TRUE)
ur_zero_count


# Count zeros in Birth_Rate
birth_zero_count <- sum(global_edu$Birth_Rate == 0, na.rm = TRUE)
print(paste("Number of zeros in Birth_Rate:", birth_zero_count))
print(paste("Percentage of zeros in Birth_Rate:", round((birth_zero_count / nrow(global_edu)) * 100, 2), "%"))
#1 zero which is 0.91% of dataset, will remove the 1 of the rows

# Count zeros in Unemployment_Rate
unemployment_zero_count <- sum(global_edu$Unemployment_Rate == 0, na.rm = TRUE)
print(paste("Number of zeros in Unemployment_Rate:", unemployment_zero_count))
print(paste("Percentage of zeros in Unemployment_Rate:", round((unemployment_zero_count / nrow(global_edu)) * 100, 2), "%"))
#1 zero which is 0.91% of dataset, will remove the 1 of the rows


#Removing zeros from Birth_Rate
global_edu_temp <- global_edu[global_edu$Birth_Rate > 0, ]

#Removing zeros from Unemployment_Rate
global_edu_filtered <- global_edu_temp[global_edu_temp$Unemployment_Rate > 0, ]

# Total rows removed
total_removed <- nrow(global_edu) - nrow(global_edu_filtered)
total_removed
print(paste("Percentage of data removed:", round((total_removed/nrow(global_edu))*100, 2), "%"))

global_edu_filtered$log_Birth_Rate <- log(global_edu_filtered$Birth_Rate)
global_edu_filtered$log_Unemployment_Rate <- log(global_edu_filtered$Unemployment_Rate)

#scatter plot after data was transformed
log_var <-global_edu_filtered[,c("log_Unemployment_Rate", "log_Birth_Rate")]

plot(x = log_var$log_Unemployment_Rate, y= log_var$log_Birth_Rate,
  xlab = "Log Unemployment Rate",
  ylab = "Log Birth Rate",
  main = "Log Unemployment Rate vs Log Birth Rate")


#Check for Normality 
qqnorm(global_edu_filtered$log_Birth_Rate)
qqline(global_edu_filtered$log_Birth_Rate)
#Most of the data in the middle lies along the line
#however towards the tails the data starts to deviate away from the diagonal line


#Data transformation
transformed_Birth_Rate <- transformTukey(global_edu_filtered$log_Birth_Rate)
#global_edu_filtered$transformed_birth_rate <- transformTukey(global_edu_filtered$log_Birth_Rate)
qqnorm(transformed_Birth_Rate)
qqline(transformed_Birth_Rate)
#transformed the data to improve the normality, so we can assume that this assumption is met


qqnorm(global_edu_filtered$log_Unemployment_Rate)
qqline(global_edu_filtered$log_Unemployment_Rate)
#we can assume that the data is approximately normal
#The data that is in the middle lies along the diagonal line, or close to the line



#linear regression model
#model <- lm(log_Unemployment_Rate~y_transform,data=global_edu_filtered)
model <- lm(log_Unemployment_Rate ~ transformed_Birth_Rate, data = global_edu_filtered)

#model summary
summary(model)


#Check for Homoscedasticity
res <- resid(model)
plot(fitted(model), res)

#adding a horizontal line at 0
abline(0,0)
#The Homoscedasticity assumption is met.The residuals are randomly and evenly scatterd
#This shows that the varaince is constant
#distributed throughout the chart around 0, and the residuals do not show any noticeable pattern.


#Check for Independence
#since the data is not a time series, checking for this assumption is not required

#Check for normality of residuals
#Q-Q plot for residuals
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res)

#Most of the residuals lie along the diagonal line which shows that the residuals follow
#a normal distribution. A few of the residuals at the tails do not lie along the line,but 
#since most of the data lies along the line this assumption is met





