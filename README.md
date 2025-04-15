# The Role of Psychological Momentum in Esports: Analyzing Early Wins and Match Outcomes

This project investigates whether **early game performance influences the final match outcome** in two competitive esports titles: **Rainbow 6 Siege** and **Rocket League**. Using statistical modeling and real pro match data, we assess how early-round wins impact the likelihood of winning a match.

## Project Overview

**Hypothesis 1:**  
> Teams that secure more early-round wins or score the first goal are significantly more likely to win the match.

**Methods Used:**
- Bayesian Logistic Regression (`brms`)
- Classic Logistic Regression (`glm`)
- Random Forest Modeling (`randomForest`)
- Chi-Square & Proportion Testing

**Tools & Languages:**
- R (tidyverse, brms, yardstick, randomForest, ggplot2)
- R Markdown / Quarto
- Git & GitHub for version control

## Key Findings

### Rainbow 6 Siege
- Early wins significantly increase the odds of winning a match.
- Logistic and Bayesian models show a **~3.3x increase** in win odds when early rounds are secured.
- Random Forest model achieved **~73% accuracy**, with early_win_indicator as the most important variable.

### Rocket League
- Scoring early goals strongly predicts match victory.
- Bayesian model shows a **~11.9x increase** in win odds with early goals.
- Random Forest model achieved **~79% accuracy**, with early_win_indicator again dominating feature importance.

---

**Hypothesis 2:**  
> Teams with consecutive wins win the full series more often than those with alternating patterns.

**Methods Used:**
- Bayesian Logistic Regression (brms)
- Hierarchical Bayesian Modeling (random slopes by game)
- Exploratory Data Visualization (boxplots, transition matrix, sequence plots)

**Tools & Languages:**
- R (tidyverse, brms, ggplot2, dplyr)
- R Markdown / Quarto
- Git & GitHub for version control

## Key Findings

### Rainbow 6 Siege AND Rocket League
- Longer win streaks are positively associated with winning a series.
- Fewer alternations (i.e., back-and-forth wins between teams) are linked to higher series win probability.
- Bayesian models indicate a meaningful relationship between these momentum-like features and match outcomes, though the effect size varies by game.
- Hierarchical modeling reveals considerable variation in how win streaks impact outcomes across games. Some uncertainty remains, but the overall trend supports a momentum effect at the series level.

---

**Hypothesis 3:**  
> Higher-seeded teams are less impacted by early losses.


**Methods Used:**
- 

**Tools & Languages:**
- 

## Key Findings

### Rainbow 6 Siege
- 

### Rocket League
-
