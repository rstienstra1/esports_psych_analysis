# Jaythan Baythavong
# ISTA 498 Senior Capstone
# April 3, 2025

# libraries ---------------------------------------------------------------------------

library(tidyverse)
library(caret)
library(ggplot2)
library(brms)
library(readr)
library(bayestestR)  # Added for probability of direction analysis
library(bayesplot)    # Added for posterior predictive checks
set.seed(1999)       # Set seed for reproducibility





# load and clean data ------------------------------------------------------------------------

# Constructs the URL for the Google sheet that contains the R6 data
sheet_id <- "1JWk84PgKI_DNqgl8ncdx8_KJpBi7l7ZuoHrCutoBnGc"
gid <- "716521061"
url <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/export?format=csv&gid=", gid)

Capstone_Data <- read_csv(url) # construct data frame

# Ensure column names are lowercase
Capstone_Data <- Capstone_Data %>%
  rename_all(tolower)





# transform data for bayesian analysis ---------------------------------------------------

# Construct longer dataframe to track whether or not each team won and/or had more early wins
# for each match
df_long <- Capstone_Data %>%
  pivot_longer(cols = c(team_a, team_b), names_to = "role", values_to = "team") %>%
  mutate(
    win = ifelse(team == winner, 1, 0),
    early_wins = ifelse(
      (role == "team_a" & early_rounds_won_a == 1) | 
        (role == "team_b" & early_rounds_won_b == 1), 1, 0)
  ) %>%
  select(team, win, early_wins)

# Ensure 'team' is a factor for random effects
df_long$team <- as.factor(df_long$team)





# bayesian logistic regression ----------------------------------------------------------

# Fits a bayesian logistic regression model predicting the probability of a win based on 
# whether a team scored the first goal, with a random effect for team strength, using 
# the brm function
bayes_model_r6 <- brm(
  win ~ early_wins + (1 | team),  # Random effect for team strength
  data = df_long, 
  family = bernoulli(),
  prior = c(set_prior("normal(0, 1)", class = "b")),  # Weakly informative prior
  chains = 4, iter = 2000, warmup = 500, cores = 4
)





# check model summary ------------------------------------------------------------------

# view estimates
summary(bayes_model_r6)

# visualize model's coefficients and check fit
plot(bayes_model_r6)





# View 95% Credible Intervals -------------------------------------------------------------

posterior_intervals <- posterior_samples(bayes_model_r6) %>%
  summarise(
    lower = quantile(b_first_goal, 0.025),
    median = quantile(b_first_goal, 0.5),
    upper = quantile(b_first_goal, 0.975)
  )
print(posterior_intervals)





# Check for the Probability of Direction (effect significance) ----------------------------

p_dir <- p_direction(posterior_samples(bayes_model_r6))
print(p_dir)





# Posterior Predictive Simulations ----------------------------------------------------------

pp_check(bayes_model_r6, nsamples = 100)  # Compare simulated vs observed outcomes





# Simulate match outcomes if a team wins the first 3 rounds ----------------------------------

new_data <- data.frame(early_wins = 1, team = unique(df_long$team))
posterior_predictions <- posterior_predict(bayes_model_r6, newdata = new_data, nsamples = 100)
print(posterior_predictions)





# Save the model for future use --------------------------------------------------------------

save(bayes_model_r6, file = "bayes_model_r6.RData")






