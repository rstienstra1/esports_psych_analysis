# Kevin Li
# ISTA 498 Senior Capstone
# April 6, 2025

# This code performs a proportion test to examine if the proportion of matches
# that team a wins differs when they win early rounds versus when they don't.
# It also performs a chi-squared test of independence.

# imports -----------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(caret)
library(ggplot2)
library(readr)
set.seed(1999)
options(scipen=999)





# data load + clean --------------------------------------------------------------

# Constructs the URL for the Google sheet that contains the R6 data
sheet_id <- "1JWk84PgKI_DNqgl8ncdx8_KJpBi7l7ZuoHrCutoBnGc"
gid <- "716521061"
url <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/export?format=csv&gid=", gid)

r6 <- read_csv(url) # construct data frame

r6 <- r6 %>%
  rename_all(tolower) %>%
  rename(
    early_rounds_won_a = early_rounds_won_a,
    early_rounds_won_b = early_rounds_won_b,
    final_score_a = final_score_a,
    final_score_b = final_score_b,
    winner = winner
  )





# wrangling ---------------------------------------------------------------

r6_data <- r6 %>%
  mutate(
    # Create binary indicator if team A won the match
    team_a_win = ifelse(winner == team_a, 1, 0),
    
    # Create indicator if team A won more early rounds (first 3 rounds)
    team_a_won_early = ifelse(early_rounds_won_a > early_rounds_won_b, 1, 0),
    
    # Calculate total rounds played in the match
    total_rounds = final_score_a + final_score_b,
    
    # Create a categorical variable for early round outcomes
    early_outcome = case_when(
      early_rounds_won_a > early_rounds_won_b ~ "Team A won early",
      early_rounds_won_a < early_rounds_won_b ~ "Team B won early",
      early_rounds_won_a == early_rounds_won_b ~ "Tie in early rounds"
    )
  )





# statistical tests -------------------------------------------------------

# 1. Proportion of matches won by Team A when they win early rounds vs. don't
# Filter out ties in early rounds for proportion test
filtered_data <- r6_data %>% 
  filter(early_rounds_won_a != early_rounds_won_b)

# Create contingency table
contingency_table <- table(
  Early_Rounds = filtered_data$team_a_won_early,
  Match_Outcome = filtered_data$team_a_win
)

print("Contingency Table:")
print(contingency_table)



# Proportion test
prop_test_result <- prop.test(
  x = c(
    sum(filtered_data$team_a_win[filtered_data$team_a_won_early == 1]), 
    sum(filtered_data$team_a_win[filtered_data$team_a_won_early == 0])
  ),
  n = c(
    sum(filtered_data$team_a_won_early == 1), 
    sum(filtered_data$team_a_won_early == 0)
  ),
  correct = FALSE
)

print("\nProportion Test Results:")
print(prop_test_result)



# Chi-square test of independence (including ties)
full_contingency <- table(
  Early_Outcome = r6_data$early_outcome,
  Match_Winner = ifelse(r6_data$team_a_win == 1, "Team A", "Team B")
)

print("\nFull Contingency Table (including ties):")
print(full_contingency)

chi_test_result <- chisq.test(full_contingency, correct = FALSE)

print("\nChi-square Test Results (including ties):")
print(chi_test_result)

# Effect size calculation (Cramer's V for larger tables)
cramers_v <- sqrt(chi_test_result$statistic / (sum(full_contingency) * (min(dim(full_contingency)) - 1)))
print(paste("\nCramer's V (effect size):", round(cramers_v, 3)))





# visualizations ----------------------------------------------------------

# Visualization 1: Win proportion by early round outcome
ggplot(r6_data, aes(x = early_outcome, fill = factor(team_a_win))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("red", "green"), 
                    labels = c("Team B Won Match", "Team A Won Match"),
                    name = "Match Outcome") +
  labs(title = "Match Win Proportion by Early Round Outcome",
       subtitle = "Early rounds = first 3 rounds of the match",
       x = "Early Round Outcome",
       y = "Proportion of Matches") +
  theme_minimal() +
  theme(legend.position = "top")

# Visualization 2: Distribution of early round wins
ggplot(r6_data, aes(x = early_rounds_won_a)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = 0:3) +
  labs(title = "Distribution of Early Round Wins by Team A",
       subtitle = "Out of first 3 rounds",
       x = "Number of Early Rounds Won by Team A",
       y = "Number of Matches") +
  theme_minimal()

# Additional analysis: Average final score difference based on early round outcome
r6_data %>%
  mutate(score_diff = final_score_a - final_score_b) %>%
  group_by(early_outcome) %>%
  summarise(
    avg_score_diff = mean(score_diff),
    win_rate = mean(team_a_win),
    n_matches = n()
  ) %>%
  arrange(desc(avg_score_diff)) %>%
  print()

