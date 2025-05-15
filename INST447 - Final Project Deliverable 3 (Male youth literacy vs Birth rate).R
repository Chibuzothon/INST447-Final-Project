library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)
library(ggplot2)

globalEducation <- read.csv("Cleaned Global Education.csv")
view(globalEducation)

# Box Plot
boxplot(globalEducation$Youth_15_24_Literacy_Rate_Male, 
        globalEducation$Birth_Rate,
        main = "Birth Rate vs Male Youth Literacy",
        names = c("Male Youth Literacy Rate", "Birth Rate"))

# There are no outliers.

# Assumptions: Linearity, Independence, Homoscedasticity, Normality of Residuals

#Linearity
ggplot(globalEducation, aes(x = Youth_15_24_Literacy_Rate_Male, y = Birth_Rate)) + geom_point() + geom_smooth(method = "lm")

# Linearity assumption was violated. Log transform Birth_Rate and Youth_15_24_Literacy_Rate_Male.
globalEducation$Birth_Rate_log <- log(globalEducation$Birth_Rate)
globalEducation$Youth_15_24_Literacy_Rate_Male_log <- log(globalEducation$Youth_15_24_Literacy_Rate_Male)

# Removing invalid values from model
globalEducation <- globalEducation[is.finite(globalEducation$Birth_Rate_log),]
globalEducation <- globalEducation[is.finite(globalEducation$Youth_15_24_Literacy_Rate_Male_log),]

# Check linearity using transformed Youth_15_24_Literacy_Rate_Male data
ggplot(globalEducation, aes(x = Youth_15_24_Literacy_Rate_Male_log, y = Birth_Rate_log)) + geom_point() + geom_smooth(method = "lm", na.rm = TRUE)

# Create the lm
model <- lm(Birth_Rate_log ~Youth_15_24_Literacy_Rate_Male_log, data = globalEducation)

# Homoscedasticity
res <- resid(model)
plot(fitted(model), res)
abline(0,0)

# Normality of Residuals
qqnorm(resid(lm(Birth_Rate_log ~ Youth_15_24_Literacy_Rate_Male_log, data = globalEducation)))
qqline(resid(lm(Birth_Rate_log ~ Youth_15_24_Literacy_Rate_Male_log, data = globalEducation)))

# Summary statistics
sum <- summary(model)