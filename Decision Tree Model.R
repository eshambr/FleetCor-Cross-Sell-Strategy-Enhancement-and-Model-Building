library(readxl)
library(caret)
library(rpart)
library(rpart.plot)

# Loading the data
data <- read_excel("Data_For_Modelling.xlsx")

# Convert categorical columns to factors
data$FUEL_ONLY_PARENT_ACCT <- as.factor(data$FUEL_ONLY_PARENT_ACCT)
data$NSF_PMTS <- as.factor(data$NSF_PMTS)
data$WO_AMOUNT <- as.factor(data$WO_AMOUNT)

# Removing the ACCT_CODE column
data <- data[, !(names(data) %in% "ACCT_CODE")]

# Splitting data into training and test sets
set.seed(123)
trainIndex <- createDataPartition(data$NSF_PMTS, p = .8, list = FALSE, times = 1)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Training the decision tree model with a specified cp value
cp_value <- 0.007
fit <- rpart(NSF_PMTS ~ ., data = trainData, method = "class", cp = cp_value)

# Predicting on the test data and obtain class predictions directly
predicted_classes <- predict(fit, testData, type = "class")

# Computing model accuracy and other metrics
confusionMatrix <- confusionMatrix(predicted_classes, testData$NSF_PMTS)

# Printing the model and performance metrics
print(fit)
print(confusionMatrix)

# Plotting the decision tree

rpart.plot(fit, main="Decision Tree Plot", 
           box.palette="RdBu",
           border.col="darkblue",
           extra=104,
           under=TRUE,
           faclen=0)
