# Jaythan Baythavong
# ISTA 498 Senior Capstone
# April 9, 2025


# libraries ---------------------------------------------------------------

library(readxl)
library(dplyr)
library(ggplot2)
library(zoo)
library(depmixS4)
library(tidyverse)





# data load + clean -------------------------------------------------------

# Constructs the URL for the Google sheet that contains the Rocket League data
sheet_id <- "1JWk84PgKI_DNqgl8ncdx8_KJpBi7l7ZuoHrCutoBnGc"
gid <- "0"
url <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/export?format=csv&gid=", gid)

df <- read_csv(url) # construct data frame

df <- df %>%
  rename_all(tolower) # make all column names lowercase

# Preview structure
str(df)
head(df)





# preprocessing -------------------------------------------------------

# Create one row per team per match
long_df <- df %>%
  rowwise() %>%
  do(rbind(
    data.frame(
      match_id = .$match_id,
      team = .$team_a,
      opponent = .$team_b,
      team_score = .$score_a,
      opponent_score = .$score_b,
      result = ifelse(.$winner == .$team_a, 1, 0)
    ),
    data.frame(
      match_id = .$match_id,
      team = .$team_b,
      opponent = .$team_a,
      team_score = .$score_b,
      opponent_score = .$score_a,
      result = ifelse(.$winner == .$team_b, 1, 0)
    )
  ))

# Sort by team and match
long_df <- long_df %>%
  arrange(team, match_id) %>%
  group_by(team) %>%
  mutate(match_number = row_number()) %>%
  ungroup()

# Preview
head(long_df)





# time series rolling analysis -------------------------------------------------------

# Plot W/L time series and rolling winrate for each team
teams <- unique(long_df$team)

for (team in teams) {
  team_df <- long_df %>% filter(team == team)
  
  # Rolling win rate (k = 3)
  team_df <- team_df %>%
    arrange(match_number) %>%
    mutate(RollingWinRate = rollmean(result, k = 3, fill = NA, align = "right"))
  
  # Plot: Time series of W/L
  p1 <- ggplot(team_df, aes(x = match_number, y = result)) +
    geom_line(group = 1) +
    geom_point(aes(color = factor(result)), size = 3) +
    scale_color_manual(values = c("red", "green"), labels = c("Loss", "Win")) +
    labs(title = paste("W/L Time Series -", team), y = "Result", x = "Match") +
    theme_minimal()
  
  print(p1)
  
  # Plot: Rolling Win Rate
  p2 <- ggplot(team_df, aes(x = match_number, y = RollingWinRate)) +
    geom_line(color = "steelblue") +
    geom_point(size = 2, color = "darkblue") +
    labs(title = paste("Rolling Win Rate (k=3) -", team), y = "Win Rate", x = "Match") +
    theme_minimal()
  
  print(p2)
}





# hidden markov models (HMM) -------------------------------------------------------

for (team in teams) {
  team_df <- long_df %>% filter(team == team)
  
  if (nrow(team_df) >= 6) {  # Minimum matches for meaningful HMM
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





# --- MOMENTUM EFFECT (AFTER 2 WINS) ---

momentum_check <- long_df %>%
  group_by(team) %>%
  mutate(Prev1 = lag(result), Prev2 = lag(result, 2)) %>%
  filter(Prev1 == 1 & Prev2 == 1) %>%
  summarise(NextWins = sum(result == 1, na.rm = TRUE),
            Total = n(),
            WinRateAfterStreak = NextWins / Total) %>%
  filter(Total > 0)

print(momentum_check)
