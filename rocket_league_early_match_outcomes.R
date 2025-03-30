# Rachel Stienstra
# ISTA 498 - Senior Capstone
# March 30, 2025

# imports -----------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(caret)
library(randomForest)
library(ggplot2)
library(readr)
set.seed(1999)

# data load + clean -------------------------------------------------------


sheet_id <- "1JWk84PgKI_DNqgl8ncdx8_KJpBi7l7ZuoHrCutoBnGc"
gid <- "0"
url <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/export?format=csv&gid=", gid)

rocket_league <- read_csv(url) 

rocket_league <- rocket_league %>%
  rename_all(tolower)


# wrangling ---------------------------------------------------------------

rocket_data <- rocket_league %>%
  mutate(match_win = ifelse(winner == team_a, 1, 0),
         early_win_indicator = ifelse(first_goal_a > first_goal_b, 1, 0))


# train + test split ------------------------------------------------------

train_index <- createDataPartition(rocket_data$match_win, p = 0.8, list = FALSE)
train_data <- rocket_data[train_index, ]
test_data <- rocket_data[-train_index, ]

# model -------------------------------------------------------------------

# Fit the logistic regression model
log_model <- glm(match_win ~ early_win_indicator, family = "binomial", data = train_data)

# Check the model summary
summary(log_model)

# predictions -------------------------------------------------------------

predictions <- predict(log_model, newdata = test_data, type = "response")
predicted_class <- ifelse(predictions >= 0.5, 1, 0)

# confusion matrix
conf_matrix <- table(predicted_class, test_data$match_win)
print(conf_matrix)
