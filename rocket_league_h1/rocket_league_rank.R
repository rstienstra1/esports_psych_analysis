# Steven Aske
# ISTA 498 Senior Capstone
# April 6, 2025

# This code creates a logistic regression model of rocket league pro match data

# libraries -------------------------------------------------------------------------------------

library(tidyverse)
library(lme4)
set.seed(1999)
options(scipen=999)





# data load + clean ------------------------------------------------------------------------------

# Constructs the URL for the Google sheet that contains the rocket league data
sheet_id <- "1JWk84PgKI_DNqgl8ncdx8_KJpBi7l7ZuoHrCutoBnGc"
gid <- "0"
url <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/export?format=csv&gid=", gid)

df <- read_csv(url) 

df <- df %>%
  rename_all(tolower) %>%
  rename(
    first_goal_a = first_goal_a,
    first_goal_b = first_goal_b,
    score_a = score_a,
    score_b = score_b,
    winner = winner
  )





# testing -----------------------------------------------------------------------------------

# Compute previous wins for each team
df <- df %>%
  group_by(winner) %>%
  mutate(prev_wins = row_number() - 1) %>%
  ungroup()

# Create a binary variable for match win (1 for winner, 0 otherwise)
df <- df %>%
  mutate(match_win = ifelse(winner == team_a, 1, 0))

# Determine early losses (first half of matches as early stage proxy)
df <- df %>%
  group_by(tournament, team_a) %>%
  mutate(match_id = row_number()) %>%
  ungroup() %>%
  mutate(early_loss = ifelse(match_id <= max(match_id) / 2 & match_win == 0, 1, 0))

# Run logistic regression with interaction model
logit_model <- glm(match_win ~ early_loss + prev_wins + early_loss:prev_wins, 
                   data = df, 
                   family = binomial)
summary(logit_model)






