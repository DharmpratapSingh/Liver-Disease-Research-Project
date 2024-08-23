# Load necessary libraries
library(tidyverse)
library(readr)
library(caret)
library(randomForest)

# Read and preprocess the data
data <- read_csv("hcvdat.csv") %>%
  mutate(
    AgeGroup = cut(Age, breaks = c(0, 30, 50, Inf), labels = c("Young", "Middle-aged", "Senior")),
    Sex = factor(Sex, levels = c("m", "f")),
    ALB_log = log(ALB + 1),
    Age2 = Age^2
  ) %>%
  na.omit()

# Split the data into training and validation sets
set.seed(123)
train_indices <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_indices, ]
validation_data <- data[-train_indices, ]

# Train a random forest model
fitControl <- trainControl(method = "cv", number = 10, savePredictions = "final")
rf_model <- train(
  ALB_log ~ Sex + Age + ALB + ALP + ALT + AST + BIL + CHE + CHOL + CREA + GGT + PROT + Age2,
  data = train_data,
  method = "rf",
  ntree = 500,
  trControl = fitControl,
  importance = TRUE
)

# Print the model summary
print(rf_model)
varImpPlot(rf_model$finalModel, main = "Variable Importance")

# Make predictions and calculate performance metrics
predictions <- predict(rf_model, newdata = validation_data)
actuals <- validation_data$ALB_log
residuals <- actuals - predictions
RMSE <- sqrt(mean(residuals^2))
R2 <- cor(actuals, predictions)^2

# Print RMSE and R-squared
print(paste("RMSE:", RMSE))
print(paste("R-squared:", R2))

# Plot residuals vs fitted values
plot_data <- data.frame(Fitted = predictions, Residuals = residuals)
ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals")

# Additional data manipulation and plotting
data <- data %>%
  mutate(AgeGroup = cut(Age, breaks = c(0, 30, 50, 70), labels = c("0-30", "31-50", "51-70")))

age_group_means <- data %>%
  group_by(AgeGroup) %>%
  summarise(
    Mean_ALP = mean(ALP, na.rm = TRUE),
    Mean_ALT = mean(ALT, na.rm = TRUE),
    Mean_AST = mean(AST, na.rm = TRUE)
  )

age_group_means %>%
  gather(key = "Enzyme", value = "Mean_Level", -AgeGroup) %>%
  ggplot(aes(x = AgeGroup, y = Mean_Level, fill = Enzyme)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Average Liver Enzyme Levels by Age Group", x = "Age Group", y = "Average Enzyme Level") +
  theme_minimal()
