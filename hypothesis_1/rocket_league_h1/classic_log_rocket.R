# Rachel Stienstra
# ISTA 498 Senior Capstone
# April 3, 2025

# This code performs a classic logistic regression on rocket league match data

# libraries -----------------------------------------------------------------

library(tidyverse)
library(caret)
library(ggplot2)
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
classic_log_model <- glm(match_win ~ early_win_indicator, family = "binomial", data = train_data)

# Check the model summary - p value less than 0.05
summary(classic_log_model)





# predictions + evaluation -------------------------------------------------------------

# Generate predictions using the logistic regression model on the test data
predictions <- predict(classic_log_model, newdata = test_data, type = "response")
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
rocket_odds <- tidy(classic_log_model, exponentiate = TRUE, conf.int = TRUE)

print(rocket_odds)
# Interpretation: An odds ratio > 1 means early round wins increase match win probability.



# visualization -----------------------------------------------------------

# confusion matrix heatmap
conf_df <- as.data.frame(conf_matrix)
colnames(conf_df) <- c("Predicted", "Actual", "Count")

ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "white", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Confusion Matrix Heatmap (Rocket League)",
    subtitle = "Classic Logistic Regression"
  )
