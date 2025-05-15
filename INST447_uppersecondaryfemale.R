library(rcompanion)
library(tidyverse)
library(ggpubr)
library(rstatix)

global_edu <- read.csv("Cleaned Global Education.csv")
edu_female <- global_edu %>%
  select(Birth_Rate, Completion_Rate_Upper_Secondary_Female)

#Boxplots for outlier inspection
boxplot(edu_female$Birth_Rate, edu_female$Completion_Rate_Upper_Secondary_Female,
        names = c("Birth Rate", "Upper Secondary Female Completion"),
        main = "Boxplot: Birth Rate & Female Upper Sec Completion",
        ylab = "Rate")

# Outlier detection
edu_female %>% identify_outliers(Birth_Rate)
edu_female %>% identify_outliers(Completion_Rate_Upper_Secondary_Female)

# Scatter plot before transformation
ggplot(edu_female, aes(x = Completion_Rate_Upper_Secondary_Female, y = Birth_Rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  labs(title = "Birth Rate vs Female Upper Secondary Completion",
       x = "Upper Secondary Completion Rate (Female)",
       y = "Birth Rate") +
  theme_minimal()

# qqplots before tukey's
ggqqplot(edu_female$Birth_Rate, title = "QQ Plot - Birth Rate (Before Tukey)")
ggqqplot(edu_female$Completion_Rate_Upper_Secondary_Female,
         title = "QQ Plot - Upper Secondary Completion (Female, Before Tukey)")

# tukeys
edu_female$Birth_Tukey <- transformTukey(edu_female$Birth_Rate)
edu_female$Completion_Tukey <- transformTukey(edu_female$Completion_Rate_Upper_Secondary_Female)

#QQ Plots after tukeys
ggqqplot(edu_female$Birth_Tukey, title = "QQ Plot - Birth Rate (After Tukey)")
ggqqplot(edu_female$Completion_Tukey, title = "QQ Plot - Completion Rate (After Tukey)")

#Linear Regression Model
model <- lm(Birth_Tukey ~ Completion_Tukey, data = edu_female)
summary(model)

#Residuals vs Fitted Plot
plot(model$fitted.values, model$residuals,
     main = "Residuals vs Fitted",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

#QQ plot of residuals 
ggqqplot(model$residuals, title = "QQ Plot - Residuals of Model")

# Scale Location plot
par(mfrow = c(1, 1))
plot(model, which = 3)

# Scatter plot with regression line on Tukey-transformed data
ggplot(edu_female, aes(x = Completion_Tukey, y = Birth_Tukey)) +
  geom_point(color = "darkblue") +
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  labs(title = "Linear Model: Transformed Birth Rate vs Transformed Completion Rate",
       x = "Completion Rate (Tukey-transformed)",
       y = "Birth Rate (Tukey-transformed)") +
  theme_minimal()


