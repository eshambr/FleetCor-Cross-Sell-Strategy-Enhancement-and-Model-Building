# Loading necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)

# Loading the data
data <- read_excel("Data_For_Modelling.xlsx")

# Grouping by CLI_AMOUNT and calculating the percentage of risky accounts
bad_rate_data <- data %>%
  group_by(CLI_AMOUNT) %>%
  summarise(Bad_Rate_Percent = mean(NSF_PMTS == "Risky") * 100)

# Visualizing the data
ggplot(bad_rate_data, aes(x = CLI_AMOUNT, y = Bad_Rate_Percent)) +
  geom_line(color = "#2c3e50", size = 1) +
  geom_point(color = "#e74c3c", size = 3, shape = 21, fill = "#e74c3c") +
  labs(title = "Bad Rate by CLI Amount",
       subtitle = "Visualizing the percentage of risky accounts by credit limit increments",
       x = "CLI Amount (Normalized)",
       y = "Bad Rate (%)",
       caption = "Data source: FleetCor Database") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 1),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

# Saving the plot
ggsave("bad_rate_by_cli.png", width = 10, height = 7)
