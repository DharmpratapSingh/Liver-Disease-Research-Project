# Load necessary libraries
library(tidyverse)
library(car)

# Read and preprocess the data
data_viable <- read_csv("hcvdat.csv") %>%
  mutate(
    Category = factor(Category, levels = c("0=Blood Donor", "1=Hepatitis", "2=Fibrosis", "3=Cirrhosis")),
    Sex = factor(Sex, levels = c("m", "f")),
    AgeGroup = cut(Age, breaks = c(0, 30, 50, 75, Inf), labels = c("Young", "Middle-aged", "Senior", "Elderly"))
  )

# Perform Levene's tests for homogeneity of variances
levene_test_category <- leveneTest(GGT ~ Category, data = data_viable)
levene_test_sex <- leveneTest(GGT ~ Sex, data = data_viable)
levene_test_agegroup <- leveneTest(GGT ~ AgeGroup, data = data_viable)
levene_test_cat_sex <- leveneTest(GGT ~ Category:Sex, data = data_viable)

# Print the results of the Levene's tests
print(levene_test_category)
print(levene_test_sex)
print(levene_test_agegroup)
print(levene_test_cat_sex)

# Log-transform GGT and add to the dataset
data_viable <- data_viable %>%
  mutate(log_GGT = log(GGT + 1))

# Perform ANOVA on log-transformed GGT with interaction terms
anova_log_ggt <- aov(log_GGT ~ Category * Sex * AgeGroup, data = data_viable)
print(summary(anova_log_ggt))

# Fit a simple linear model
simple_model <- lm(log_GGT ~ Category + Sex + AgeGroup, data = data_viable)
print(summary(simple_model))

# Create a plot showing interactions
p <- ggplot(data_viable, aes(x = Category, y = log_GGT, color = Sex, shape = AgeGroup)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  facet_wrap(~AgeGroup) +
  labs(
    title = "Interaction of Category, Sex, and Age on log(GGT) Levels",
    y = "Log of Gamma-Glutamyl Transferase",
    x = "Category"
  ) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

# Print the plot
print(p)
