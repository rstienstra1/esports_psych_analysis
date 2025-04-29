# Rachel Stienstra
# ISTA 498 Senior Capstone
# April 3, 2025

# This code makes a RandomForest model for rocket league pro match data

# libraries ---------------------------------------------------------------

library(tidyverse)
library(randomForest)
library(caret)
library(ggplot2)

# Set seed for reproducibility
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
  mutate(
    match_win = ifelse(winner == team_a, 1, 0),
    early_win_indicator = ifelse(first_goal_a > first_goal_b, 1, 0),
    match_win = as.factor(match_win),
    tournament = as.factor(tournament)
  )





# train + test split ------------------------------------------------------

# Split rocket league data into training 80% and testing 20% sets based on match win (the target)
train_index <- createDataPartition(rocket_data$match_win, p = 0.8, list = FALSE)
train_data <- rocket_data[train_index, ]
test_data <- rocket_data[-train_index, ]





# model -------------------------------------------------------------------

# use early win indicator and tournament to build rf model
rf_model <- randomForest(
  match_win ~ early_win_indicator + tournament,
  data = train_data,
  importance = TRUE
)


# Save the model for reproducibility
saveRDS(rf_model, file = "rf_model.rds")


# model evaluation --------------------------------------------------------

# Predictions
predictions <- predict(rf_model, newdata = test_data)

# Confusion Matrix
conf_matrix <- table(predictions, test_data$match_win)

# Accuracy
rf_accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy: ", rf_accuracy))


# variable importance
var_importance <- importance(rf_model)
print(var_importance)
# Interpretation: A higher value indicates a stronger impact on match outcome predictions

# Confusion Matrix
confusionMatrix(predictions, test_data$match_win)


# data viz ---------------------------------------------------------------

# variable importance plot
varImpPlot(rf_model)

test_data$match_win <- factor(test_data$match_win, levels = levels(predictions))

# confusion matrix plot
conf_matrix_df <- as.data.frame(as.table(conf_matrix))
colnames(conf_matrix_df) <- c("Predicted", "Actual", "Count")

ggplot(conf_matrix_df, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "white", size = 6) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Confusion Matrix Heatmap - RandomForest (RL)", x = "Actual", y = "Predicted")

# Bar plot of prediction distribution
ggplot(as.data.frame(predictions), aes(x = predictions)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Predicted Match Outcomes", x = "Prediction", y = "Count")



