# Jaythan Baythavong
# ISTA 498 Senior Capstone
# April 13, 2025


# libraries ---------------------------------------------------------------

library(readxl)
library(dplyr)
library(ggplot2)
library(zoo)
library(depmixS4)
library(tidyverse)





# data load + clean -------------------------------------------------------

# Constructs the URL for the Google sheet that contains the R6 data
sheet_id <- "1JWk84PgKI_DNqgl8ncdx8_KJpBi7l7ZuoHrCutoBnGc"
gid <- "716521061"
url <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/export?format=csv&gid=", gid)

df <- read_csv(url) # construct data frame

df <- df %>%
  rename_all(tolower) # make all column names lowercase





# preprocessing -------------------------------------------------------

# Long-format data: one row per team per match
long_df <- df %>%
  rowwise() %>%
  do(rbind(
    data.frame(
      match_id = .$match_id,
      map = .$map,
      team = .$team_a,
      opponent = .$team_b,
      team_score = .$final_score_a,
      opponent_score = .$final_score_b,
      result = ifelse(.$winner == .$team_a, 1, 0)
    ),
    data.frame(
      match_id = .$match_id,
      map = .$map,
      team = .$team_b,
      opponent = .$team_a,
      team_score = .$final_score_a,
      opponent_score = .$final_score_b,
      result = ifelse(.$winner == .$team_b, 1, 0)
    )
  ))

# Order by team and match number
long_df <- long_df %>%
  arrange(team, match_id) %>%
  group_by(team) %>%
  mutate(match_number = row_number()) %>%
  ungroup()






# analysis -------------------------------------------------------

teams <- unique(long_df$team)

for (team in teams) {
  team_df <- long_df %>% filter(team == team)
  
  # Rolling win rate (k = 3)
  team_df <- team_df %>%
    arrange(match_number) %>%
    mutate(RollingWinRate = rollmean(result, k = 3, fill = NA, align = "right"))
  
  # W/L Time Series
  p1 <- ggplot(team_df, aes(x = match_number, y = result)) +
    geom_line(group = 1) +
    geom_point(aes(color = factor(result)), size = 3) +
    scale_color_manual(values = c("red", "green"), labels = c("Loss", "Win")) +
    labs(title = paste("W/L Time Series -", team), y = "Result", x = "Match") +
    theme_minimal()
  print(p1)
  
  # Rolling Win Rate
  p2 <- ggplot(team_df, aes(x = match_number, y = RollingWinRate)) +
    geom_line(color = "steelblue") +
    geom_point(size = 2, color = "darkblue") +
    labs(title = paste("Rolling Win Rate (k=3) -", team), y = "Win Rate", x = "Match") +
    theme_minimal()
  print(p2)
}

# --- Hidden Markov Models ---

for (team in teams) {
  team_df <- long_df %>% filter(team == team)
  
  if (nrow(team_df) >= 6) {
    hmm_model <- depmix(response = result ~ 1, family = binomial(), nstates = 2, data = team_df)
    fitted <- fit(hmm_model, verbose = FALSE)
    team_df$State <- posterior(fitted)$state
    
    p3 <- ggplot(team_df, aes(x = match_number, y = result)) +
      geom_point(aes(color = factor(State)), size = 3) +
      geom_line(alpha = 0.5, group = 1) +
      scale_color_manual(values = c("blue", "orange"), labels = c("State 1", "State 2")) +
      labs(title = paste("HMM State Detection -", team), y = "Result", x = "Match") +
      theme_minimal()
    print(p3)
  }
}

# --- Momentum Check: Win after 2-win streaks ---

momentum_check <- long_df %>%
  group_by(team) %>%
  mutate(Prev1 = lag(result), Prev2 = lag(result, 2)) %>%
  filter(Prev1 == 1 & Prev2 == 1) %>%
  summarise(NextWins = sum(result == 1, na.rm = TRUE),
            Total = n(),
            WinRateAfterStreak = NextWins / Total) %>%
  filter(Total > 0)

print(momentum_check)

