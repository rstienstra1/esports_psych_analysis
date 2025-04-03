# Rachel Stienstra
# ISTA 498 Senior Capstone
# April 3, 2025

# libraries ---------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
set.seed(1999)





# data load + clean --------------------------------------------------------------

# Constructs the URL for the Google sheet that contains the R6 data
sheet_id <- "1JWk84PgKI_DNqgl8ncdx8_KJpBi7l7ZuoHrCutoBnGc"
gid <- "716521061"
url <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/export?format=csv&gid=", gid)

r6 <- read_csv(url) # construct data frame

r6 <- r6 %>%
  rename_all(tolower) # make all column names lowercase





# visualizations -----------------------------------------------------------

# Create dataframe that calculates each team's matches played, wins, and win rates
team_stats <- r6 %>%
  pivot_longer(cols = c(team_a, team_b), names_to = "role", values_to = "team") %>%
  mutate(win = ifelse(team == winner, 1, 0)) %>%
  group_by(team) %>%
  summarize(matches_played = n(), wins = sum(win), win_rate = wins / matches_played)
# Bar plot win rates for each team
ggplot(team_stats, aes(x = reorder(team, win_rate), y = win_rate, fill = team)) +
  geom_col() +
  coord_flip() +
  labs(title = "Win Rates by Team",
       x = "Team",
       y = "Win Rate")





# Counts number of times each map was played across the collected tournament data
map_counts <- r6 %>%
  count(map, name = "times_played")
# Plots map counts
ggplot(map_counts, aes(x = reorder(map, times_played), y = times_played, fill = map)) +
  geom_col() +
  coord_flip() +
  labs(title = "Number of Matches Played per Map",
       x = "Map",
       y = "Times Played") +
  theme(legend.position = "none")





# Calculates win rates for team a + b based on number of early rounds won
win_rate_data <- r6 %>%
  mutate(team_a_won = ifelse(winner == team_a, 1, 0),
         team_b_won = ifelse(winner == team_b, 1, 0)) %>%
  group_by(early_rounds_won_a, early_rounds_won_b) %>%
  summarise(win_rate_a = mean(team_a_won), 
            win_rate_b = mean(team_b_won), 
            match_count = n(), 
            .groups = "drop") %>%
  pivot_longer(cols = c(win_rate_a, win_rate_b), 
               names_to = "team", 
               values_to = "win_rate") %>%
  mutate(team = ifelse(team == "win_rate_a", "Team A", "Team B"))
# Bar plot comparing win rates by team based on the number of early rounds won
ggplot(win_rate_data, aes(x = factor(ifelse(team == "Team A", early_rounds_won_a, early_rounds_won_b)), 
                          y = win_rate, fill = team)) +
  geom_col(position = "dodge", color = "black") +
  labs(title = "Win Rate Based on Early Rounds Won",
       x = "Early Rounds Won",
       y = "Win Rate",
       fill = "Team")
