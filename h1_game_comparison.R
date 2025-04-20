# Rachel Stienstra
# ISTA 498 Senior Capstone
# April 19, 2025

# Please run the classic logistic regressions for Rainbow 6 Siege and for
# Rocket League before running this script. This script uses the odds ratio
# data from each of those scripts to plot the difference in impact of early
# wins for those two games. Thanks!


# combine odds ratio data for both games ----------------------------------

rocket_odds$Game <- "Rocket League"
r6_odds$Game <- "Rainbow 6 Siege"

# Filter to just the early win predictor (change the term if needed!)
rocket_early <- subset(rocket_odds, term == "early_win_indicator")
r6_early <- subset(r6_odds, term == "early_win_indicator")

combined_odds <- rbind(rocket_early, r6_early)

# visualization -----------------------------------------------------------

# Bar plot the combined odds comparison
ggplot(combined_odds, aes(x = Game, y = estimate)) +
  geom_col(fill = "lightblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  scale_y_continuous(trans = "log10") +
  labs(title = "Impact of Early Wins on Match Outcomes",
       y = "Odds Ratio (log scale)",
       x = "Game Title") +
  theme_minimal()






