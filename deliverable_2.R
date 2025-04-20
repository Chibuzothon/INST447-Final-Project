install.packages("ggplot2")
install.packages("psych")
install.packages("readr")
install.packages("dplyr")
install.packages("reshape2")
install.packages("gridExtra")

library(ggplot2)
library(psych)
library(readr)
library(dplyr)
library(reshape2)
library(gridExtra)

df <- read_csv("Cleaned Global Education.csv")

subset_df <- df %>%
  select(Unemployment_Rate,
         Gross_Primary_Education_Enrollment,
         Gross_Tertiary_Education_Enrollment,
         Birth_Rate,
         Completion_Rate_Upper_Secondary_Male,
         Completion_Rate_Upper_Secondary_Female)


cat("Descriptive Statistics")
print(describe(subset_df))

#Pearson Correlation Matrix
cor_matrix <- cor(subset_df, use = "complete.obs")
cat("Pearson Correlation Matrix")
print(round(cor_matrix, 2))

#Heatmap of Correlation Matrix
melted_cor <- melt(cor_matrix)

ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Pearson Correlation") +
  theme_minimal() +
  coord_fixed() +
  labs(title = "Correlation Heatmap of Selected Indicators", x = "", y = "")

# Unemployment vs Primary Enrollment
p1 <- ggplot(subset_df, aes(x = Unemployment_Rate, y = Gross_Primary_Education_Enrollment)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(title = "Unemployment vs Primary Enrollment",
       x = "Unemployment Rate",
       y = "Gross Primary Enrollment") +
  theme_minimal()

# Unemployment vs Tertiary Enrollment
p2 <- ggplot(subset_df, aes(x = Unemployment_Rate, y = Gross_Tertiary_Education_Enrollment)) +
  geom_point(color = "orange") +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(title = "Unemployment vs Tertiary Enrollment",
       x = "Unemployment Rate",
       y = "Gross Tertiary Enrollment") +
  theme_minimal()

# Birth Rate vs Upper Secondary Completion (Male)
p3 <- ggplot(subset_df, aes(x = Birth_Rate, y = Completion_Rate_Upper_Secondary_Male)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(title = "Birth Rate vs Completion (Male)",
       x = "Birth Rate",
       y = "Upper Secondary Completion (Male)") +
  theme_minimal()

# Birth Rate vs Upper Secondary Completion (Female)
p4 <- ggplot(subset_df, aes(x = Birth_Rate, y = Completion_Rate_Upper_Secondary_Female)) +
  geom_point(color = "darkred") +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(title = "Birth Rate vs Completion (Female)",
       x = "Birth Rate",
       y = "Upper Secondary Completion (Female)") +
  theme_minimal()

grid.arrange(p1, p2, p3, p4, ncol = 2)

