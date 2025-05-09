# Tom Oswald
# ISTA 498 Senior Capstone
# April 16, 2025

# Hypothesis 2: Psychological Momentum and Comebacks

# libraries----------------------------------------------------------------------------

# Load required libraries
library(brms)
library(tidyverse)
library(tidybayes)
library(ggplot2)





# data load in ------------------------------------------------------------------------

sheet_id <- "1JWk84PgKI_DNqgl8ncdx8_KJpBi7l7ZuoHrCutoBnGc"
r6_gid <- "716521061"   # Rainbow Six
rl_gid <- "0"           # Rocket League

# Rainbow Six
url_r6 <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id,
                 "/export?format=csv&gid=", r6_gid)
r6 <- read_csv(url_r6) %>%
  rename_all(tolower) %>%
  mutate(game = "Rainbow Six")

# Rocket League
url_rl <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id,
                 "/export?format=csv&gid=", rl_gid)
rl <- read_csv(url_rl) %>%
  rename_all(tolower) %>%
  mutate(game = "Rocket League")

# Combine datasets
all_matches <- bind_rows(r6, rl)





# Preprocess and Identify Series ------------------------------------------------------

all_matches <- all_matches %>%
  mutate(
    team1 = pmin(team_a, team_b),
    team2 = pmax(team_a, team_b),
    series_id = paste(tournament, team1, team2, sep = "_"),
    match_order = ave(match_id, series_id, FUN = seq_along),
    winner_norm = case_when(
      winner == team1 ~ "Team1",
      winner == team2 ~ "Team2",
      TRUE ~ "Unknown"
    )
  )





# Feature Extraction Function ---------------------------------------------------------

extract_features <- function(df) {
  outcomes <- df %>% arrange(match_order) %>% pull(winner_norm)
  
  streaks <- rle(outcomes)
  longest_streak <- max(streaks$lengths[streaks$values %in% c("Team1", "Team2")])
  
  alternations <- sum(
    outcomes[-1] != outcomes[-length(outcomes)] &
      outcomes[-1] != "Unknown" &
      outcomes[-length(outcomes)] != "Unknown"
  )
  
  team1_wins <- sum(outcomes == "Team1")
  team2_wins <- sum(outcomes == "Team2")
  
  series_winner <- if (team1_wins > team2_wins) "Team1"
  else if (team2_wins > team1_wins) "Team2"
  else "Tie"
  
  tibble(
    longest_streak = longest_streak,
    alternations = alternations,
    series_winner = series_winner,
    game = df$game[1]
  )
}





# Apply Feature Extraction ---------------------------------------------------------

series_features <- all_matches %>%
  group_by(series_id) %>%
  group_modify(~ extract_features(.x)) %>%
  ungroup() %>%
  filter(series_winner != "Tie") %>%
  mutate(
    series_win = if_else(series_winner == "Team1", 1, 0),
    series_win = factor(series_win, levels = c(0, 1), labels = c("Team2 Wins", "Team1 Wins"))
  )





# data viz -------------------------------------------------------------------------

# 1. Boxplot: Longest Streak vs. Series Win
ggplot(series_features, aes(x = series_win, y = longest_streak)) +
  geom_boxplot(fill = "skyblue") +
  facet_wrap(~ game) +
  labs(
    title = "Longest Streak vs Series Win (by Game)",
    x = "Series Winner", y = "Longest Consecutive Wins"
  ) +
  theme_minimal()

# 2. Boxplot: Alternations vs. Series Win
ggplot(series_features, aes(x = series_win, y = alternations)) +
  geom_boxplot(fill = "steelblue") +
  facet_wrap(~ game) +
  labs(
    title = "Alternations vs Series Win (by Game)",
    x = "Series Winner", y = "Number of Win Alternations"
  ) +
  theme_minimal()

# 3. Time Series View of Sample Series
example_series <- all_matches %>%
  filter(series_id %in% sample(unique(series_id), 5)) %>%
  arrange(series_id, match_order)

ggplot(example_series, aes(
  x = match_order,
  y = fct_reorder(str_wrap(series_id, width = 30), match_order, .fun = max, .desc = TRUE),
  fill = winner_norm
)) +
  geom_tile(color = "white", height = 0.9, width = 0.9) +
  scale_fill_manual(
    values = c("Team1" = "darkblue", "Team2" = "lightblue", "Unknown" = "gray70"),
    name = "Match Winner"
  ) +
  labs(
    title = "Match Outcome Sequences by Series",
    x = "Match Order",
    y = "Series"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )

# 4. Transition Matrix (Heatmap version)
get_transitions <- function(outcomes) {
  tibble(
    from = outcomes[-length(outcomes)],
    to = outcomes[-1]
  ) %>%
    filter(from != "Unknown", to != "Unknown")
}

transitions_df <- all_matches %>%
  group_by(series_id) %>%
  summarise(transitions = list(get_transitions(winner_norm)), .groups = "drop") %>%
  unnest(transitions)

transition_counts <- transitions_df %>%
  count(from, to)

ggplot(transition_counts, aes(x = from, y = to, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  geom_text(aes(label = n), color = "black", size = 4) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Win Transition Matrix (Heatmap)",
    x = "Previous Match Result",
    y = "Next Match Result",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# bayesian logistic regression --------------------------------------------------------

# 5. Basic Bayesian Logistic Regression
bayes_model <- brm(
  series_win ~ longest_streak + alternations,
  data = series_features,
  family = bernoulli(),
  chains = 2, iter = 2000, cores = 2,
  seed = 123
)

# Visualize fixed effects using tidybayes
bayes_model %>%
  gather_draws(b_longest_streak, b_alternations) %>%
  ggplot(aes(x = .value, y = .variable)) +
  stat_halfeye(.width = 0.95, fill = "skyblue") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Basic Bayesian Logistic Regression",
    subtitle = "Pooled effect estimates across both games",
    x = "Effect Size (Log-Odds)",
    y = "Model Parameter"
  ) +
  theme_minimal()



summary(bayes_model)
plot(bayes_model)

# 6. Hierarchical Bayesian Model by Game
bayes_hier_model <- brm(
  series_win ~ longest_streak + alternations + (1 + longest_streak | game),
  data = series_features,
  family = bernoulli(),
  chains = 2, iter = 2000, cores = 2,
  seed = 456
)

# Extract the fixed effect for longest_streak
base_streak <- fixef(bayes_hier_model)["longest_streak", "Estimate"]

# Visualize game-specific total effects (fixed + random)
bayes_hier_model %>%
  spread_draws(r_game[game, longest_streak]) %>%
  mutate(total_effect = r_game + base_streak) %>%
  ggplot(aes(x = total_effect, y = reorder(game, total_effect))) +
  stat_halfeye(.width = 0.95, fill = "skyblue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "Hierarchical Bayesian Model: Longest Streak Effect by Game",
    subtitle = "Estimated effect of win streaks on series win probability",
    x = "Effect Size (Log-Odds)",
    y = "Game"
  ) +
  theme_minimal()


summary(bayes_hier_model)
plot(bayes_hier_model)






