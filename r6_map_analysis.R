# Kevin Li
# ISTA 498 Senior Capstone
# April 11, 2025

# This code provides some analysis of maps in pro R6 matches

# imports -----------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(caret)
library(ggplot2)
library(readr)
set.seed(1999)





# data load + clean --------------------------------------------------------------

# This constructs the url that links to our data
sheet_id <- "1JWk84PgKI_DNqgl8ncdx8_KJpBi7l7ZuoHrCutoBnGc"
gid <- "716521061"
url <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/export?format=csv&gid=", gid)

r6 <- read_csv(url) 

r6 <- r6 %>%
  rename_all(tolower) %>%
  rename(
    early_rounds_won_a = early_rounds_won_a,
    early_rounds_won_b = early_rounds_won_b,
    final_score_a = final_score_a,
    final_score_b = final_score_b,
    winner = winner
  )





# wrangling and model ---------------------------------------------------------------

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

r6_data <- r6_data %>%
  mutate(
    team_a_win = as.factor(team_a_win),
    team_a_won_early = as.factor(team_a_won_early),
    map = as.factor(map),
    team_a = as.factor(team_a),
    team_b = as.factor(team_b)
  )

# Create a team identifier that combines both teams (for random effects)
r6_data <- r6_data %>%
  mutate(match_id = factor(paste(team_a, team_b, sep = "_vs_")))

library(lme4)

# Mixed-effects logistic regression model
model <- glmer(team_a_win ~ team_a_won_early + map + (1 | team_a) + (1 | team_b),
               data = r6_data,
               family = binomial(link = "logit"))

summary(model)

# To get odds ratios and confidence intervals
exp(fixef(model))
exp(confint(model, method = "Wald"))





# visualization ---------------------------------------------------------------

# Plot map effects
map_effects <- as.data.frame(confint(model, parm = "map", method = "Wald"))
map_effects$map <- rownames(map_effects)

ggplot(map_effects, aes(x = reorder(map, Estimate), y = exp(Estimate))) +
  geom_point() +
  geom_errorbar(aes(ymin = exp(`2.5 %`), ymax = exp(`97.5 %`)), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(title = "Map Effects on Team A Win Probability",
       x = "Map",
       y = "Odds Ratio (Team A Win)") +
  coord_flip() +
  theme_minimal()


