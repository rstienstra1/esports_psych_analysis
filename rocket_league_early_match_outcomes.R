# Rachel Stienstra
# ISTA 498 Senior Capstone
# April 3, 2025

# libraries -----------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(caret)
library(randomForest)
library(ggplot2)
library(readr)
library(yardstick)
library(broom)
set.seed(1999)
options(scipen=999)




# data load + clean -------------------------------------------------------

# Constructs the URL for the Google sheet that contains the Rocket League data
sheet_id <- "1JWk84PgKI_DNqgl8ncdx8_KJpBi7l7ZuoHrCutoBnGc"
gid <- "0"
url <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/export?format=csv&gid=", gid)

rocket_league <- read_csv(url) # construct data frame

rocket_league <- rocket_league %>%
  rename_all(tolower) # make all column names lowercase





# wrangling ---------------------------------------------------------------

# Make new columns indicating if team a won the match and if they scored the first goal
rocket_data <- rocket_league %>%
  mutate(match_win = ifelse(winner == team_a, 1, 0),
         early_win_indicator = ifelse(first_goal_a > first_goal_b, 1, 0))





# train + test split ------------------------------------------------------

# Split rocket league data into training 80% and testing 20% sets based on match win (the target)
train_index <- createDataPartition(rocket_data$match_win, p = 0.8, list = FALSE)
train_data <- rocket_data[train_index, ]
test_data <- rocket_data[-train_index, ]





# model -------------------------------------------------------------------

# Fit the logistic regression model
log_model <- glm(match_win ~ early_win_indicator, family = "binomial", data = train_data)

# Check the model summary - p value less than 0.05
summary(log_model)





# predictions + evaluation -------------------------------------------------------------

# Generate predictions using the logistic regression model on the test data
predictions <- predict(log_model, newdata = test_data, type = "response")
# Convert predicted probabilities into 1 for win, 0 for loss
predicted_class <- ifelse(predictions >= 0.5, 1, 0)

# Confusion Matrix to compare predictions vs actual
conf_matrix <- table(predicted_class, test_data$match_win)
print(conf_matrix)




# Added model performance metrics with yardstick
# Convert estimate and truth to factor
test_results <- test_data %>%
  mutate(predicted = factor(predicted_class),
         match_win = factor(match_win))  # Convert match_win to factor
# Calculate accuracy based on actual results and predictions
accuracy_score <- accuracy(test_results, truth = match_win, estimate = predicted)
# Print accuracy score
print(accuracy_score)




# Calculate odds ratio for rocket league
tidy(log_model, exponentiate = TRUE, conf.int = TRUE)






# visualization -----------------------------------------------------------

# Plot distribution of win rates based on early win indicator
rocket_data %>%
  ggplot(aes(x = factor(early_win_indicator), fill = factor(match_win))) +
  geom_bar(position = "fill") +
  labs(x = "Early Win (1 = Yes)", y = "Proportion", fill = "Match Win")

