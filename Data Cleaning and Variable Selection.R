# Reading the excel data

library(readxl)
data_for_analysis <- read_excel("DataForAnalysis.xlsx")

# Transforming and Converting 'FUEL_ONLY_PARENT_ACCT':

data_for_analysis$FUEL_ONLY_PARENT_ACCT <- ifelse(data_for_analysis$FUEL_ONLY_PARENT_ACCT == '0', 
                                                  "Universal Card", "Fuel Only Card")
data_for_analysis$FUEL_ONLY_PARENT_ACCT <- as.factor(data_for_analysis$FUEL_ONLY_PARENT_ACCT)

levels(data_for_analysis$FUEL_ONLY_PARENT_ACCT)

# Transforming and Converting 'NSF_PMTS':

data_for_analysis$NSF_PMTS <- ifelse(data_for_analysis$NSF_PMTS == 0, 
                                     "Not Risky", "Risky")
data_for_analysis$NSF_PMTS <- as.factor(data_for_analysis$NSF_PMTS)

levels(data_for_analysis$NSF_PMTS)

# Transforming and Converting 'WO_AMOUNT':

data_for_analysis$WO_AMOUNT <- ifelse(data_for_analysis$WO_AMOUNT == 0, 
                                      "No WO Payments", "WO Payments")
data_for_analysis$WO_AMOUNT <- as.factor(data_for_analysis$WO_AMOUNT)

levels(data_for_analysis$WO_AMOUNT)

numeric_columns <- sapply(data_for_analysis, is.numeric)  # Identifying numeric columns
numeric_columns["ACCT_CODE"] <- FALSE  # Excluding ACCT_CODE from normalization

# Applying Min-Max normalization

data_for_analysis[numeric_columns] <- apply(data_for_analysis[numeric_columns], 2, function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
})

# Viewing changes to ensure normalization has been applied
summary(data_for_analysis[numeric_columns])

# Re-identifying numeric columns post normalization, if not already defined
numeric_columns <- sapply(data_for_analysis, is.numeric)
numeric_columns["ACCT_CODE"] <- FALSE  # Excluding ACCT_CODE from transformations

# Applying Square Root Transformation as a general approach
data_for_analysis[numeric_columns] <- sapply(data_for_analysis[numeric_columns], sqrt)

# Identifying columns that can benefit from log transformation (strictly positive values)
log_transformable <- sapply(data_for_analysis[numeric_columns], function(x) min(x, na.rm = TRUE) > 0.01)

# Filtering the names of columns that are log transformable based on the previous check
log_transformable_columns <- names(data_for_analysis)[numeric_columns][log_transformable]

# Applying log transformation where applicable
data_for_analysis[log_transformable_columns] <- sapply(data_for_analysis[log_transformable_columns], function(x) log(x + 1))

# Load necessary libraries
library(ggplot2)
library(reshape2)  # or library(tidyr) for a tidyverse approach

# Melting the data frame for ggplot
melted_data <- melt(data_for_analysis, measure.vars = names(data_for_analysis)[numeric_columns])

# Now plotting
ggplot(melted_data, aes(x = value)) + 
  geom_histogram(bins = 30) + 
  facet_wrap(~variable, scales = 'free_x') +
  labs(title = "Histogram of Numeric Variables after Transformation", x = "Transformed Values", y = "Frequency")

# VIF score

# Building a Linear Model for Analysis

# Converting NSF_PMTS to a binary numeric variable 
data_for_analysis$NSF_PMTS_numeric <- as.numeric(data_for_analysis$NSF_PMTS) - 1

# Updating numeric_predictors to exclude NSF_PMTS
numeric_predictors <- setdiff(numeric_predictors, "NSF_PMTS")

# Fitting a logistic regression model
model_formula <- as.formula(paste("NSF_PMTS_numeric ~", paste(numeric_predictors, collapse = "+")))
logistic_model <- glm(model_formula, data = data_for_analysis, family = binomial())

# Calculating VIF to check for multicollinearity in logistic model
# Loading necessary library

library(car)
vif_values <- vif(logistic_model)

# Printing the VIF values
print(vif_values)

# Setting a threshold high VIF
high_vif_threshold <- 5

# Identifying predictor names with high VIF scores
high_vif_columns <- names(vif_values[vif_values > high_vif_threshold])

# Printing high VIF columns
print(high_vif_columns)

# Dropping high VIF columns from the data frame
data_for_analysis <- data_for_analysis[, !colnames(data_for_analysis) %in% high_vif_columns]

# Dropping NSF_PMTS_numeric and NSF_AMTS

data_for_analysis$NSF_PMTS_numeric <- NULL
data_for_analysis$NSF_AMT <- NULL

# Loading the openxlsx library
library(openxlsx)

# Write the DataFrame to an Excel file
write.xlsx(data_for_analysis, "Data_For_Modelling.xlsx")
