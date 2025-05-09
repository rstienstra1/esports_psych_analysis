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

# Interpret Bayesian model's odds ratio
posterior_summary(bayesian_log_model, probs = c(0.025, 0.975)) %>%
  as_tibble(rownames = "term") %>%
  filter(str_detect(term, "early_win_indicator")) %>%
  mutate(odds_ratio = exp(Estimate),
         lower = exp(Q2.5),
         upper = exp(Q97.5))

# posterior predictive check
pp_check(bayesian_log_model)

# Compare to frequentist logistic regression
glm(match_win ~ early_win_indicator, family = binomial(), data = train_data)

# Simulate predicted win probabilities
newdata <- tibble(early_win_indicator = c(1, 0))
posterior_epred(bayesian_log_model, newdata = newdata) %>%
  rowMeans()

# data viz --------------------------------------------------------

# confusion matrix heatmap
conf_df <- as.data.frame(conf_matrix)
colnames(conf_df) <- c("Predicted", "Actual", "Count")

ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "white", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Confusion Matrix Heatmap (Rocket League)")

# predicted probability distribution
ggplot(test_results, aes(x = pred, fill = match_win)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Predicted Probabilities by Actual Match Outcome",
       x = "Predicted Probability of Win", y = "Density")

# Plot distribution of win rates based on early win indicator
rocket_data %>%
  mutate(early_win_indicator = factor(early_win_indicator, labels = c("Lost Early", "Won Early")),
         match_win = factor(match_win, labels = c("Loss", "Win"))) %>%
  ggplot(aes(x = early_win_indicator, fill = match_win)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("Loss" = "darkblue", "Win" = "lightblue")) +
  labs(title = "Match Win Proportion by Early Round Outcome (Rocket League)",
       x = "Early Round Win?", y = "Proportion", fill = "Match Result")

# Plot posterior distribution of odds ratio
bayesian_log_model %>%
  spread_draws(b_early_win_indicator) %>%
  mutate(or = exp(b_early_win_indicator)) %>%
  ggplot(aes(x = or)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  labs(title = "Posterior Distribution of Odds Ratio for Early Win",
       x = "Odds Ratio", y = "Density")
