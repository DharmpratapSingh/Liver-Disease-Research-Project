# Load necessary libraries
library(tidyverse)
library(caret)
library(e1071)
library(nnet)
library(pROC)
library(randomForest)

# Read and preprocess the data
data <- read_csv("hcvdat.csv") %>%
  mutate(
    Category = factor(Category, levels = c("0=Blood Donor", "1=Hepatitis", "2=Fibrosis", "3=Cirrhosis"), 
                      labels = c("Blood_Donor", "Hepatitis", "Fibrosis", "Cirrhosis")),
    Sex = factor(Sex, levels = c("m", "f")),
    log_ALB = log(ALB + 1),
    log_BIL = log(BIL + 1)
  ) %>%
  na.omit()

# Split the data into training and test sets
set.seed(123)
training_indices <- createDataPartition(data$Category, p = 0.8, list = FALSE)
training_data <- data[training_indices, ]
test_data <- data[-training_indices, ]

# Set up cross-validation
fitControl <- trainControl(method = "cv", number = 10, summaryFunction = multiClassSummary, classProbs = TRUE, savePredictions = "final")

# Train a Random Forest model
model_rf <- train(Category ~ log_ALB + log_BIL + AST + ALT + GGT + Sex, data = training_data, method = "rf", trControl = fitControl, metric = "Accuracy")

# Train an SVM model
model_svm <- train(Category ~ log_ALB + log_BIL + AST + ALT + GGT + Sex, data = training_data, method = "svmRadial", trControl = fitControl, metric = "Accuracy")

# Make predictions with Random Forest
predictions_rf <- predict(model_rf, newdata = test_data)
confusionMatrix_rf <- confusionMatrix(predictions_rf, test_data$Category)

# Calculate ROC curves and AUCs for Random Forest
probabilities_rf <- predict(model_rf, newdata = test_data, type = "prob")
roc_results_rf <- lapply(levels(test_data$Category), function(class) roc(response = test_data$Category, predictor = probabilities_rf[, class]))
aucs_rf <- sapply(roc_results_rf, auc)

# Make predictions with SVM
probabilities_svm <- predict(model_svm, newdata = test_data, type = "prob")
roc_results_svm <- lapply(levels(test_data$Category), function(class) roc(response = test_data$Category, predictor = probabilities_svm[, class]))
aucs_svm <- sapply(roc_results_svm, auc)

# Print AUCs
print(list(RandomForest_AUCs = aucs_rf, SVM_AUCs = aucs_svm))

# Confusion matrix heatmap
confusionMatrix_rf <- confusionMatrix(predictions_rf, test_data$Category)
heatmap(as.matrix(confusionMatrix_rf$table), Rowv = NA, Colv = NA, scale = "column", margins = c(5,5), xlab = "Predicted", ylab = "Actual", main = "Confusion Matrix Heatmap")

# Plot ROC curves
plot(roc_results_rf[[1]], col = "red", main = "ROC Curves for Disease Categories")
for (i in 2:length(roc_results_rf)) {
  plot(roc_results_rf[[i]], col = i, add = TRUE)
}
legend("bottomright", legend = levels(test_data$Category), col = 1:length(roc_results_rf), lwd = 2)

# Variable Importance Plot for Random Forest
varImpPlot(model_rf$finalModel, main = "Variable Importance in Predicting Liver Disease Severity")

