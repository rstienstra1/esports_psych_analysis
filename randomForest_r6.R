# Rachel Stienstra
# ISTA 498 Senior Capstone
# April 3, 2025

# This code makes a RandomForest model for Rainbow 6 Siege pro match data

# libraries ---------------------------------------------------------------

library(tidyverse)
library(randomForest)
library(caret)
library(ggplot2)
set.seed(1999)
options(scipen=999)





# data load + clean -------------------------------------------------------

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

rf_model <- randomForest(match_win ~ early_win_indicator, data = train_data, importance = TRUE)

varImpPlot(rf_model)






# model evaluation --------------------------------------------------------

# Predictions
predictions <- predict(rf_model, newdata = test_data)

# Confusion Matrix
conf_matrix <- table(predictions, test_data$match_win)

# Accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy: ", accuracy))




