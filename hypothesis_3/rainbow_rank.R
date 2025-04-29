# Steven Aske
# ISTA 498 Senior Capstone
# April 6, 2025

# This code creates a logistic regression model of rainbow pro match data

# libraries -------------------------------------------------------------------------------------

library(tidyverse)
library(lme4)
library(brms)
set.seed(1999)
options(scipen=999)





# data load + clean ------------------------------------------------------------------------------

# Constructs the URL for the Google sheet that contains the R6 data
sheet_id <- "1JWk84PgKI_DNqgl8ncdx8_KJpBi7l7ZuoHrCutoBnGc"
gid <- "716521061"
url <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/export?format=csv&gid=", gid)

rainbow_df <- read_csv(url) # construct data frame

rainbow_df <- rainbow_df %>%
  rename_all(tolower) # make all column names lowercase





# preprocessing and analysis ---------------------------------------------------------------

# Clean and prepare Rainbow Six data for modeling
rainbow_long <- rainbow_df %>%
 
# Store original team info for reference and pivot dataset
  mutate(team1 = team_a,
         team2 = team_b) %>%
  pivot_longer(cols = c(team1, team2),
               names_to = "team_position",
               values_to = "team") %>%
  mutate(
    opponent = ifelse(team_position == "team1", team_b, team_a),
    score = ifelse(team_position == "team1", final_score_a, final_score_b),
    opp_score = ifelse(team_position == "team1", final_score_b, final_score_a),
    early_wins = ifelse(team_position == "team1", early_rounds_won_a, early_rounds_won_b),
    opp_early_wins = ifelse(team_position == "team1", early_rounds_won_b, early_rounds_won_a),
    early_loss = ifelse(early_wins < opp_early_wins, 1, 0),
    match_win = ifelse(winner == team, 1, 0)
  ) %>%
  select(match_id, team, opponent, score, opp_score, early_loss, match_win)

# Add seed proxy based on cumulative wins before the match
rainbow_long <- rainbow_long %>%
  group_by(team) %>%
  arrange(match_id) %>%
  mutate(seed_proxy = lag(cumsum(match_win), default = 0)) %>%
  ungroup()

# Fit logistic regression model
rainbow_model <- glm(match_win ~ early_loss * seed_proxy,
                     data = rainbow_long, family = binomial())

summary(rainbow_model)

# Create a sequence of seed values
seed_range_r6 <- seq(min(rainbow_long$seed_proxy), max(rainbow_long$seed_proxy), by = 1)

# Build a prediction grid
newdata_r6 <- expand.grid(
  early_loss = c(0, 1),
  seed_proxy = seed_range_r6
)

# Predict win probabilities
newdata_r6$predicted_prob <- predict(rainbow_model, newdata = newdata_r6, type = "response")

# label
newdata_r6$early_loss_label <- ifelse(newdata_r6$early_loss == 1, "Early Loss", "No Early Loss")





# visualization -------------------------------------------------------------------------------------

# Plot
ggplot(newdata_r6, aes(x = seed_proxy, y = predicted_prob, color = early_loss_label)) +
  geom_line(size = 1.2) +
  labs(
    title = "Win Probability by Seed Strength & Early Loss",
    subtitle = "Rainbow 6 Siege",
    x = "Seed Proxy (Cumulative Wins Before Match)",
    y = "Predicted Probability\n(Winning)",
    color = "Early Loss"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title.position = "plot"
  )


