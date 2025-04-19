library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)

global_edu <- read.csv("Cleaned Global Education.csv")
View("Cleaned Gloabal Education.csv")


#Descriptive stats for birth rate & unemployment rate
summary(global_edu[, c("Birth_Rate", "Unemployment_Rate")])


#Box plots for the unemployment rate & the birth rate
boxplot(global_edu$Birth_Rate, global_edu$Unemployment_Rate,
        names = c("Birth Rate", "Unemployment Rate"),
        main = "Boxplot Comparing the Unemployment Rate vs the Birth Rate",
        ylab = "Rate Value")


#Scatter plot for the unemployment rate & the birth rate
var <-global_edu[,c("Birth_Rate", "Unemployment_Rate")]

plot(x = var$Birth_Rate, y= var$Unemployment_Rate,
     xlab = "Birth Rate",
     ylab = "Unempoyment Rate",
     main = "Birth Rate vs Unemployment Rate")
     


