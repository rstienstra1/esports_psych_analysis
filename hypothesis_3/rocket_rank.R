# Steven Aske
# ISTA 498 Senior Capstone
# April 10, 2025

# This code creates a logistic regression model of rocket league pro match data

# libraries -------------------------------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(ggplot2)
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




# preprocessing and analysis -------------------------------------------------------------------

# Store original team info for reference and pivot dataset
rocket_long <- df %>%
  mutate(game_num = row_number()) %>%
  rowwise() %>%
  mutate(team1 = team_a, team2 = team_b) %>%
  ungroup() %>%
  pivot_longer(cols = c(team1, team2),
               names_to = "team_position",
               values_to = "team") %>%
  mutate(opponent = ifelse(team_position == "team1", team_b, team_a),
         score = ifelse(team_position == "team1", score_a, score_b),
         opp_score = ifelse(team_position == "team1", score_b, score_a),
         match_win = ifelse(winner == team, 1, 0),
         early_loss = ifelse((first_goal_a == 0 & team_position == "team1") |
                               (first_goal_b == 0 & team_position == "team2"), 1, 0)) %>%
  select(match_id, team, opponent, score, opp_score, match_win, early_loss)


# Add seed proxy based on cumulative wins before each match
rocket_long <- rocket_long %>%
  group_by(team) %>%
  arrange(match_id) %>%
  mutate(seed_proxy = lag(cumsum(match_win), default = 0)) %>%
  ungroup()

# Logistic regression
rocket_model <- glm(match_win ~ early_loss * seed_proxy, 
                    data = rocket_long, family = binomial())
summary(rocket_model)

# Create a sequence of seed_proxy values to simulate predictions
seed_range <- seq(min(rocket_long$seed_proxy), max(rocket_long$seed_proxy), by = 1)

# Create a new dataframe for prediction
newdata <- expand.grid(
  early_loss = c(0, 1),
  seed_proxy = seed_range
)

# Predict probabilities using the logistic model
newdata$predicted_prob <- predict(rocket_model, newdata = newdata, type = "response")

# Label early_loss groups
newdata$early_loss_label <- ifelse(newdata$early_loss == 1, "Early Loss", "No Early Loss")





# visualization -------------------------------------------------------------------------------------

# Plot
ggplot(newdata, aes(x = seed_proxy, y = predicted_prob, color = early_loss_label)) +
  geom_line(size = 1.2) +
  labs(
    title = "Win Probability by Seed Strength & Early Loss",
    subtitle = "Rocket League",
    x = "Seed Proxy (Cumulative Wins Before Match)",
    y = "Predicted Probability\n(Winning)",
    color = "Early Loss"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title.position = "plot"
  )

