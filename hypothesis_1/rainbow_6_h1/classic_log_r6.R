# Rachel Stienstra
# ISTA 498 Senior Capstone
# April 3, 2025

# this code performs a classic logistic regression on R6 pro match data

# libraries -----------------------------------------------------------------

library(tidyverse)
library(caret)
library(ggplot2)
library(yardstick)
library(broom)
set.seed(1999)
options(scipen=999)





# data load + clean --------------------------------------------------------------

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





# train and test split ----------------------------------------------------

# Split R6 data into training 80% and testing 20% sets based on match win (the target)
train_index <- createDataPartition(r6_data$match_win, p = 0.8, list = FALSE)
train_data <- r6_data[train_index, ]
test_data <- r6_data[-train_index, ]





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
  mutate(predicted = factor(predicted_class, levels = c(0, 1)),
         match_win = factor(match_win, levels = c(0, 1)))
# Calculate accuracy based on actual results and predictions
accuracy_score <- accuracy(test_results, truth = match_win, estimate = predicted)
# Print accuracy score
print(accuracy_score)



# Calculate odds ratio for r6
r6_odds <- tidy(classic_log_model, exponentiate = TRUE, conf.int = TRUE)

print(r6_odds)
# Interpretation: An odds ratio > 1 means early round wins increase match win probability.


# visualizations ----------------------------------------------------------

# confusion matrix heatmap
conf_df <- as.data.frame(conf_matrix)
colnames(conf_df) <- c("Predicted", "Actual", "Count")

ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "white", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Confusion Matrix Heatmap (R6)",
    subtitle = "Classic Logistic Regression"
  )
