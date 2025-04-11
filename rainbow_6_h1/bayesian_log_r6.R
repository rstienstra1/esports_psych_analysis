# Rachel Stienstra & Jaythan Baythavong
# ISTA 498 Senior Capstone
# April 6, 2025

# This code performs a bayesian logistic regression on R6 pro match data

# libraries ---------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(caret)
library(readr)
library(yardstick)
library(brms)
library(ggplot2)
set.seed(1999)
options(scipen=999)





# data load _ clean -------------------------------------------------------

# Constructs the URL for the Google sheet that contains the R6 data
sheet_id <- "1JWk84PgKI_DNqgl8ncdx8_KJpBi7l7ZuoHrCutoBnGc"
gid <- "716521061"
url <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/export?format=csv&gid=", gid)

r6 <- read_csv(url) # construct data frame

r6 <- r6 %>%
  rename_all(tolower) # make all column names lowercase





# wrangling ---------------------------------------------------------------

# Make new columns indicating if team a won the match and if they had more early round wins
r6_data <- r6 %>%
  mutate(match_win = ifelse(winner == team_a, 1, 0),
         early_win_indicator = ifelse(early_rounds_won_a > early_rounds_won_b, 1, 0))





# train + test split ------------------------------------------------------

# Split R6 data into training 80% and testing 20% sets based on match win (the target)
train_index <- createDataPartition(r6_data$match_win, p = 0.8, list = FALSE)
train_data <- r6_data[train_index, ]
test_data <- r6_data[-train_index, ]





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





# data viz --------------------------------------------------------

# confusion matrix heatmap
conf_df <- as.data.frame(conf_matrix)
colnames(conf_df) <- c("Predicted", "Actual", "Count")

ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "white", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Confusion Matrix Heatmap")

# predicted probability distribution
ggplot(test_results, aes(x = pred, fill = match_win)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Predicted Probabilities by Actual Match Outcome",
       x = "Predicted Probability of Win", y = "Density")

# Plot distribution of win rates based on early win indicator
r6_data %>%
  mutate(early_win_indicator = factor(early_win_indicator, labels = c("Lost Early", "Won Early")),
         match_win = factor(match_win, labels = c("Loss", "Win"))) %>%
  ggplot(aes(x = early_win_indicator, fill = match_win)) +
  geom_bar(position = "fill") +
  labs(title = "Match Win Proportion by Early Round Outcome",
       x = "Early Round Win?", y = "Proportion", fill = "Match Result")


