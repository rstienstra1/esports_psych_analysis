# Rachel Stienstra
# ISTA 498 Senior Capstone
# April 6, 2025

# This code performs a bayesian logistic regression on rocket league match data

# libraries -------------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(caret)
library(readr)
library(yardstick)
library(brms)
set.seed(1999)
options(scipen=999)





# data load _ clean -------------------------------------------------------

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

# Fitting a bayesian logistic regression model with a weak normal prior
prior <- set_prior("normal(0, 1)", class = "b")

bayesian_log_model <- brm(match_win ~ early_win_indicator, 
                          family = bernoulli(), 
                          data = train_data,
                          prior = prior)





# model evaluation --------------------------------------------------------

# summary stats
summary(bayesian_log_model)





# Generate predictions (posterior predictive draws)
predictions <- predict(bayesian_log_model, newdata = test_data, type = "response")
# Average across the draws for each observation
mean_preds <- rowMeans(predictions)
# Convert predicted probabilities into 1 for win or 0 for loss
predicted_class <- ifelse(mean_preds >= 0.5, 1, 0)

# Confusion Matrix to compare predictions vs actual
conf_matrix <- table(predicted_class, test_data$match_win)
print(conf_matrix)




# Add model eval with yardstick
test_results <- test_data %>%
  mutate(pred = rowMeans(predict(bayesian_log_model, newdata = test_data, type = "response")),
         pred_class = factor(ifelse(pred >= 0.5, 1, 0)),
         match_win = factor(match_win))

# Accuracy
accuracy(test_results, truth = match_win, estimate = pred_class)



