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

#Descriptive stats for primary female completion and female youth literacy rates
summary(global_edu[,c("Completion_Rate_Primary_Female","Youth_15_24_Literacy_Rate_Female")])

#Scatter plot for primary female completion and female youth literacy rates
CRPF = c(global_edu$Completion_Rate_Primary_Female)
YLRF = c(global_edu$Youth_15_24_Literacy_Rate_Female)

ggplot(Global_Edu, aes(x = CRPF, y = YLRF)) +
  labs(title = "Correlation Between Female Primary Completion & Youth Literacy",
       x = "Primary Completion Rate (Female)",
       y = "Youth Literacy Rate (Female)") +
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed")




