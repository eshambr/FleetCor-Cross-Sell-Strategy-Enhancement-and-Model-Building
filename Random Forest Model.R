library(readxl)
library(caret)
library(randomForest)
library(ggplot2)

# Load the data
data <- read_excel("Data_For_Modelling.xlsx")

# Converting categorical columns to factors
data$FUEL_ONLY_PARENT_ACCT <- as.factor(data$FUEL_ONLY_PARENT_ACCT)
data$NSF_PMTS <- as.factor(data$NSF_PMTS)
data$WO_AMOUNT <- as.factor(data$WO_AMOUNT)

# Removing the ACCT_CODE column
data$ACCT_CODE <- NULL

# Splitting data into training and test sets
set.seed(123)
trainIndex <- createDataPartition(data$NSF_PMTS, p = .8, list = FALSE, times = 1)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Training the Random Forest model
rf_fit <- randomForest(NSF_PMTS ~ ., data = trainData, ntree=500, mtry=2, importance=TRUE)

# Predicting on the test data and obtain class predictions
predicted_classes_rf <- predict(rf_fit, testData)

# Computing model accuracy and other metrics
confusionMatrix_rf <- confusionMatrix(predicted_classes_rf, testData$NSF_PMTS)

# Printing the model and performance metrics
print(rf_fit)
print(confusionMatrix_rf)

# Plotting the importance of variables
importance_df <- as.data.frame(importance(rf_fit))
importance_df$Variable <- row.names(importance_df)
ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_minimal() +
  labs(title = "Variable Importance in Random Forest Model", x = "Variables", y = "Importance (Gini Decrease)") +
  coord_flip()

# Saving the importance plot to a file
ggsave("Variable_Importance_Plot.png", width = 10, height = 8)
