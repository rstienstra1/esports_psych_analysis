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

# data load + clean --------------------------------------------------------------

sheet_id <- "1JWk84PgKI_DNqgl8ncdx8_KJpBi7l7ZuoHrCutoBnGc"
gid <- "716521061"
url <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/export?format=csv&gid=", gid)

r6 <- read_csv(url) 

r6 <- r6 %>%
  rename_all(tolower)

# wrangling ---------------------------------------------------------------

r6_data <- r6 %>%
  mutate(match_win = ifelse(winner == team_a, 1, 0),
         early_win_indicator = ifelse(early_rounds_won_a > early_rounds_won_b, 1, 0))
# If team_a has more early rounds won AND won the match, early_win_indicator will equal 1


# train and test split ----------------------------------------------------

train_index <- createDataPartition(r6_data$match_win, p = 0.8, list = FALSE)
train_data <- r6_data[train_index, ]
test_data <- r6_data[-train_index, ]

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


# visualizations ----------------------------------------------------------

summary_data <- r6_data %>%
  group_by(early_win_indicator) %>%
  summarize(total_wins = sum(match_win)) %>%
  ungroup()

ggplot(summary_data, aes(x = factor(early_win_indicator), y = total_wins, fill = factor(early_win_indicator))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Wins achieved by Teams with or without Early Wins", x = "Early Win Indicator", y = "Total Wins", fill = "Early Win Indicator")

