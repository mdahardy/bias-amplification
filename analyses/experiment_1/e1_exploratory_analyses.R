### --- Script for running the exploratory (non-preregistered) analyses for experiment 1 --- ###

library(lme4)
library(dplyr)
source('utils.R') # helper functions

### --- --- --- --- --- --- --- --- --- ###
### --- Variables for model fitting --- ###
### --- --- --- --- --- --- --- --- --- ###

# Directory to write the results (matched and nonmatched described below)
matched_directory = 'stats/e1_exploratory/matched'
nonmatched_directory = 'stats/e1_exploratory/nonmatched'

# Dependent variables to investigate
dependent_variables = c('chose_correct','chose_bias')

# glmer or lmer, depending on type of dependent_variables
regression_type = 'glmer'

# The formula for the glmer that all models share
common_model_formula = '+(1|condition_replication)'

# Addition to common_formula for pre reg models
addition_for_prereg = '+ (1 | participant_id)'

### --- --- RUN EXPLORATORY ANALYSES --- --- --- ###
# How can social-bias participants be both more accurate and more biased than asocial-control participants?
# Investigate models predicting accuracy when data is partitioned into trials
# where the true color matched their bias (matched trials) and didn't (nonmatched trials)
### --- --- --- --- --- --- --- --- --- --- --- --- ###

# Load experiment 1 data
e1_data = load_e1_data()
matched_data = subset(e1_data,matched==T)
nonmatched_data = subset(e1_data,matched==F)

# First get condition comparisons for matched trials
matched_comparisons = matched_data %>%
  compare_conditions(dependent_variables,regression_type,common_model_formula,addition_for_prereg) %>%
  mutate(matched=T)

# Now get condition comparisons for unmatched trials
nonmatched_comparisons = nonmatched_data %>%
  compare_conditions(dependent_variables,regression_type,common_model_formula,addition_for_prereg) %>%
  mutate(matched=F)

# Write test statistics, p values, and condition means for direct use in the paper
print('-- -- -- Writing statistical tests -- -- --')
write_tests(matched_comparisons,matched_directory,F)
write_tests(nonmatched_comparisons,nonmatched_directory,F)
print('-- -- -- Writing means -- -- --')
write_means(matched_data,matched_directory,dependent_variables)
write_means(nonmatched_data,nonmatched_directory,dependent_variables)

# Print condition means for both matched and non-matched trials
print('-- -- -- Means -- -- --')
e1_data %>%
  group_by(condition,matched) %>%
  summarise(accuracy = mean(chose_correct))

# Print comparisons
comparisons = rbind(matched_comparisons,nonmatched_comparisons)
print('-- -- -- Condition comparisons -- -- --')
print(comparisons)