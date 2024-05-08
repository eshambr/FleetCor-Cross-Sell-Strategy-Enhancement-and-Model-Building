# Loading required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# Loading the data from the Excel file
data <- read_excel("Data_For_Modelling.xlsx")

# Converting 'WO_AMOUNT' to binary variable
data$WO_BINARY <- ifelse(data$WO_AMOUNT == "No WO Payments", 0, 1)

# Summary statistics for UNBILLED_BALANCE by FUEL_ONLY_PARENT_ACCT
grouped_spend <- data %>%
  group_by(FUEL_ONLY_PARENT_ACCT) %>%
  summarise(
    count = n(),
    mean = mean(UNBILLED_BALANCE),
    std = sd(UNBILLED_BALANCE),
    min = min(UNBILLED_BALANCE),
    q25 = quantile(UNBILLED_BALANCE, 0.25),
    median = median(UNBILLED_BALANCE),
    q75 = quantile(UNBILLED_BALANCE, 0.75),
    max = max(UNBILLED_BALANCE)
  )

# Summary statistics for WO_BINARY by FUEL_ONLY_PARENT_ACCT
grouped_loss <- data %>%
  group_by(FUEL_ONLY_PARENT_ACCT) %>%
  summarise(mean_loss = mean(WO_BINARY))

# Visualizing the Spend Distribution
ggplot(data, aes(x = FUEL_ONLY_PARENT_ACCT, y = UNBILLED_BALANCE, fill = FUEL_ONLY_PARENT_ACCT)) +
  geom_boxplot() +
  scale_fill_manual(values = c("coral", "deepskyblue3")) +
  labs(title = "Distribution of UNBILLED_BALANCE by Account Type",
       subtitle = "Boxplot showing median, quartiles, and outliers",
       x = "Account Type",
       y = "Unbilled Balance") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Visualizing the Loss Distribution
ggplot(grouped_loss, aes(x = FUEL_ONLY_PARENT_ACCT, y = mean_loss, fill = FUEL_ONLY_PARENT_ACCT)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("coral", "deepskyblue3")) +
  labs(title = "Proportion of Write-Offs by Account Type",
       subtitle = "Bar chart illustrating the average proportion of write-offs",
       x = "Account Type",
       y = "Proportion of Write-Offs") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Printing summary statistics
print(grouped_spend)
print(grouped_loss)
