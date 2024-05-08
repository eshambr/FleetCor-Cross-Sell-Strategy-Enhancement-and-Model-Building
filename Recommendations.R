library(tidyverse)
library(caret)
library(pROC)
library(readxl)

data_base <- read_excel("Data_For_Modelling.xlsx")

# Converting categorical variables to factors
data_base$FUEL_ONLY_PARENT_ACCT <- as.factor(data_base$FUEL_ONLY_PARENT_ACCT)
data_base$NSF_PMTS <- as.factor(data_base$NSF_PMTS)
data_base$WO_AMOUNT <- as.factor(data_base$WO_AMOUNT)

# Transforming 'NSF_PMTS' to a binary outcome
data_base$NSF_PMTS <- ifelse(data_base$NSF_PMTS == "Not Risky", 0, 1)

# Removing the identifier and prepare data for modeling
data <- data_base %>% select(-ACCT_CODE)

# Splitting the data into training and validation sets
set.seed(123)
training_rows <- createDataPartition(data$NSF_PMTS, p = 0.7, list = FALSE)
training <- data[training_rows, ]
validation <- data[-training_rows, ]

# Fitting logistic regression model
model <- glm(NSF_PMTS ~ ., data = training, family = binomial)

# Predicting on training and validation sets and save the predictions as probabilities
training_prob <- predict(model, training, type = "response")
validation_prob <- predict(model, validation, type = "response")

# Converting probabilities to risk category based on threshold of 0.5
training_pred <- ifelse(training_prob > 0.5, 1, 0)
validation_pred <- ifelse(validation_prob > 0.5, 1, 0)

# Confusion matrices for training and validation sets
confusion_train <- table(Actual = training$NSF_PMTS, Predicted = training_pred)
confusion_val <- table(Actual = validation$NSF_PMTS, Predicted = validation_pred)

# Calculating accuracy for training and validation predictions
accuracy_train <- sum(diag(confusion_train)) / sum(confusion_train)
accuracy_val <- sum(diag(confusion_val)) / sum(confusion_val)

# Assigning predictions to the original data frame and reorder columns
data$Predictions <- NA
data$Predictions[training_rows] <- ifelse(training_prob > 0.5, "Risky", "Not Risky")
data$Predictions[-training_rows] <- ifelse(validation_prob > 0.5, "Risky", "Not Risky")
data_base$Predictions <- data$Predictions
data_base <- data_base %>% select(Predictions, everything())


# Summary of results
list(
  confusion_train = confusion_train,
  confusion_val = confusion_val,
  accuracy_train = accuracy_train,
  accuracy_val = accuracy_val,
  auc_train = auc(roc(training$NSF_PMTS, training_prob)),
  auc_val = auc(roc(validation$NSF_PMTS, validation_prob))
)


# Assuming revenue increase is correlated inversely with risk and directly with credit limit
expected_revenue_increase <- mean(ifelse(validation_prob < 0.5, validation$CREDIT_LIMIT, 0)) * 100

# Print expected financial metrics
print(paste("Expected Revenue Increase:", expected_revenue_increase, "%"))

# Determining which customers to swap based on business rules

swap_out <- data_base %>% 
  filter(Predictions == "Risky" & FUEL_ONLY_PARENT_ACCT == "Universal Card")

swap_in <- data_base %>% 
  filter(Predictions == "Not Risky" & FUEL_ONLY_PARENT_ACCT != "Universal Card")

# Outputing the ACCT_CODEs of customers to swap and their counts
print("Customers to Swap Out:")
print(swap_out$ACCT_CODE)
print(paste("Total Accounts to Swap Out:", nrow(swap_out)))

print("Customers to Swap In:")
print(swap_in$ACCT_CODE)
print(paste("Total Accounts to Swap In:", nrow(swap_in)))
