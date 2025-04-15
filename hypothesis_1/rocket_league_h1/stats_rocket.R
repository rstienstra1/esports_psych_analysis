# Kevin Li
# ISTA 498 Senior Capstone
# April 6, 2025

# This code performs a proportion test to examine if the proportion of matches
# that team a wins differs when they score early goals versus when they don't.
# It also performs a chi-squared test of independence.

# imports -----------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
set.seed(1999)
options(scipen=999)





# data load + clean -----------------------------------------------------------------------

# Constructs the URL for the Google sheet that contains the rocket league data
sheet_id <- "1JWk84PgKI_DNqgl8ncdx8_KJpBi7l7ZuoHrCutoBnGc"
gid <- "0"
url <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/export?format=csv&gid=", gid)

rocket_league <- read_csv(url) # construct the data frame

rocket_league <- rocket_league %>%
  rename_all(tolower) %>%
  rename(
    first_goal_a = first_goal_a,
    first_goal_b = first_goal_b,
    score_a = score_a,
    score_b = score_b,
    winner = winner
  )





# wrangling ------------------------------------------------------------------------------

rocket_data <- rocket_league %>%
  mutate(
    # Binary indicator if team A won the match
    team_a_win = ifelse(winner == team_a, 1, 0),
    
    # Binary indicator if team A scored first goal
    team_a_scored_first = ifelse(first_goal_a == 1, 1, 0),
    
    # Calculate goal difference
    goal_diff = score_a - score_b,
    
    # Create categorical variable for first goal scorer
    first_goal_scorer = case_when(
      first_goal_a == 1 ~ "Team A",
      first_goal_b == 1 ~ "Team B",
      TRUE ~ "Neither" # In case there are matches with no first goal data
    )
  ) %>%
  filter(!is.na(first_goal_scorer)) # Remove any NA values





# statistical tests -------------------------------------------------------

# 1. Proportion test for win rate when team scores first vs doesn't
# Create contingency table
contingency_table <- table(
  First_Goal = rocket_data$team_a_scored_first,
  Match_Win = rocket_data$team_a_win
)

print("Contingency Table:")
print(contingency_table)

# Proportion test
prop_test_result <- prop.test(
  x = c(
    sum(rocket_data$team_a_win[rocket_data$team_a_scored_first == 1]), 
    sum(rocket_data$team_a_win[rocket_data$team_a_scored_first == 0])
  ),
  n = c(
    sum(rocket_data$team_a_scored_first == 1), 
    sum(rocket_data$team_a_scored_first == 0)
  ),
  correct = FALSE
)

print("\nProportion Test Results:")
print(prop_test_result)

# Chi-square test of independence
chi_test_result <- chisq.test(contingency_table, correct = FALSE)

print("\nChi-square Test Results:")
print(chi_test_result)

# Effect size calculation (Phi coefficient for 2x2 table)
phi_coefficient <- sqrt(chi_test_result$statistic / sum(contingency_table))
print(paste("\nPhi coefficient (effect size):", round(phi_coefficient, 3)))





# visualizations ----------------------------------------------------------

# Visualization 1: Win proportion by first goal scorer
ggplot(rocket_data, aes(x = first_goal_scorer, fill = factor(team_a_win))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("red", "green"), 
                    labels = c("Team B Won", "Team A Won"),
                    name = "Match Outcome") +
  labs(title = "Match Win Proportion by First Goal Scorer",
       x = "Team That Scored First Goal",
       y = "Proportion of Matches") +
  theme_minimal() +
  theme(legend.position = "top")

# Visualization 2: Average goal difference by first goal scorer
rocket_data %>%
  group_by(first_goal_scorer) %>%
  summarise(
    avg_goal_diff = mean(goal_diff),
    win_rate = mean(team_a_win),
    n_matches = n()
  ) %>%
  ggplot(aes(x = first_goal_scorer, y = avg_goal_diff, fill = first_goal_scorer)) +
  geom_col() +
  geom_text(aes(label = round(avg_goal_diff, 2)), vjust = -0.5) +
  labs(title = "Average Goal Difference by First Goal Scorer",
       subtitle = "Positive values favor Team A",
       x = "Team That Scored First Goal",
       y = "Average Goal Difference (Team A - Team B)") +
  theme_minimal() +
  theme(legend.position = "none")

# Additional metrics
cat("\nAdditional Metrics:\n")
rocket_data %>%
  group_by(first_goal_scorer) %>%
  summarise(
    matches = n(),
    win_rate = mean(team_a_win),
    avg_goal_diff = mean(goal_diff)
  ) %>%
  arrange(desc(win_rate)) %>%
  print()
