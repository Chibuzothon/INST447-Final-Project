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

# Created dataframe for measuring
BR_M <- select(global_edu,
                    Unemployment_Rate,
                    Gross_Primary_Education_Enrollment,
                    Gross_Tertiary_Education_Enrollment,
                    Birth_Rate,
                    Completion_Rate_Upper_Secondary_Male,
                    Completion_Rate_Upper_Secondary_Female
)

# Descriptive statistics
cat("Descriptive Statistics")
print(summary(BR_M))

# Unemployment vs Primary Enrollment
p1 <- ggplot(
  data = BR_M,
  aes(
    x = Unemployment_Rate,
    y = Gross_Primary_Education_Enrollment
  )
) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Unemployment vs Primary Enrollment",
    x     = "Unemployment Rate",
    y     = "Gross Primary Enrollment"
  ) +
  theme_minimal()
print(p1)

# Unemployment vs Tertiary Enrollment
p2 <- ggplot(
  data = BR_M,
  aes(
    x = Unemployment_Rate,
    y = Gross_Tertiary_Education_Enrollment
  )
) +
  geom_point(color = "orange") +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Unemployment vs Tertiary Enrollment",
    x     = "Unemployment Rate",
    y     = "Gross Tertiary Enrollment"
  ) +
  theme_minimal()
print(p2)

# Birth Rate vs Upper Secondary Completion (Male)
p3 <- ggplot(
  data = BR_M,
  aes(
    x = Birth_Rate,
    y = Completion_Rate_Upper_Secondary_Male
  )
) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Birth Rate vs Completion (Male)",
    x     = "Birth Rate",
    y     = "Upper Secondary Completion (Male)"
  ) +
  theme_minimal()
print(p3)

# Birth Rate vs Upper Secondary Completion (Female)
p4 <- ggplot(
  data = BR_M,
  aes(
    x = Birth_Rate,
    y = Completion_Rate_Upper_Secondary_Female
  )
) +
  geom_point(color = "darkred") +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Birth Rate vs Completion (Female)",
    x     = "Birth Rate",
    y     = "Upper Secondary Completion (Female)"
  ) +
  theme_minimal()
print(p4)

#Descriptive Statistics for birthrate and male youth (15-24) literacy rates
summary(global_edu[, c("Birth_Rate", "Youth_15_24_Literacy_Rate_Male")])

# Visualizations
#Scatter plot comparing birth rate and male youth literacy rate
plot(global_edu$Birth_Rate, 
     global_edu$Youth_15_24_Literacy_Rate_Male,
     main = "Correlation between birth rate and male youth literacy",
     xlab = "Birth Rate", 
     ylab = "Male Youth Literacy Rate")

# Box plot comparing birth Rate and male youth literacy rate
boxplot(global_edu$Birth_Rate, 
        global_edu$Youth_15_24_Literacy_Rate_Male,
        main = "Correlation between birth rate and male youth literacy",
        names = c("Birth Rate", "Male Youth Literacy Rate"))


#OOSR vs Completion rate Description stats

summary(global_edu[, c("Completion_Rate_Primary_Female", "OOSR_Primary_Age_Female")])

#scatter plot OOSR vs Completion rate
plot(global_edu$Completion_Rate_Primary_Female, global_edu$OOSR_Primary_Age_Female,
     xlab = "Out-of-School Rate",
     ylab = "Completion Rate",
     main="Out-of-School Rate vs. Completion Rate (Primary-Aged Females)",
     pch = 19,
     col = "darkblue")
#regression line
abline(lm(Completion_Rate_Primary_Female ~ OOSR_Primary_Age_Female, data = global_edu), col = "red", lwd = 2)

#histogram completion rate:
hist(global_edu$Completion_Rate_Primary_Female,
     main = "Distribution of Female Primary Completion Rates",
     xlab = "Completion Rate",
     col = "skyblue", 
     border = "white")

#histogram out of school rate:
hist(global_edu$OOSR_Primary_Age_Female,
     main = "Distribution of Out-of-School Rates (Females)",
     xlab = "Out-of-School Rate",
     col = "salmon",
     border = "white")

