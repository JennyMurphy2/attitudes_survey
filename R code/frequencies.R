
if(!require(tidyverse))install.packages("tidyverse")
## NEED TO RUN "surveydata.R" first.

#  Frequencies (count and percent) -----------------------------------------------------------------------------------------

## Familiarity with terms --------------------------------------------------------------------------------------------------

familiar_repro_perc <- overalldata %>%
  group_by(familiar_repro) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round)

familiar_rep_perc <- overalldata %>%
  group_by(familiar_rep) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round)

## Percent crisis --------------------------------------------------------------------------------------------------------

percent_crisis <- 
  overalldata %>%
  select(crisis) %>%
  count(crisis = factor(crisis)) %>% 
  mutate(pct = prop.table(n))

## Flagged crisis --------------------------------------------------------------------------------------------------------

flagged_crisis_perc <- overalldata %>%
  group_by(flagged_crisis) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round)

## Published results percentage ------------------------------------------------------------------------------------------

published_results_perc <- overalldata %>%
  group_by(published_results) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round) %>%
  drop_na()

published_results_perc <- 
  renameddata %>%
  select(published_results) %>%
  count(published_results = factor(published_results)) %>% 
  mutate(pct = prop.table(n)) %>%
  arrange(match(published_results, c("Very likely", "Fairly likely", "Somewhat likely", "Neither likely nor unlikely", "Somewhat unlikely", "Fairly unlikely", "Very unlikely"))) %>%
  drop_na()


## Level of rep percentage ------------------------------------------------------------------------------------------------

level_rep_perc <- overalldata %>%
  group_by(level_rep) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round)

level_rep_perc <- 
  overalldata %>%
  select(level_rep) %>%
  count(level_rep = factor(level_rep)) %>% 
  mutate(pct = prop.table(n))

## Major problem my field -------------------------------------------------------------------------------------------------

major_problem_myfield_perc <- overalldata %>%
  group_by(major_problem_myfield) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round) %>%
  arrange(match(major_problem_myfield, c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree")))

## Major problem all fields -----------------------------------------------------------------------------------------------

major_problem_allfields_perc <- overalldata %>%
  group_by(major_problem_allfields) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round) %>%
  arrange(match(major_problem_allfields, c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree")))


## Think rep percentage ---------------------------------------------------------------------------------------------------

often_think_rep_perc <- overalldata %>%
  group_by(often_think_rep) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round)

## Talk colleagues percentage ----------------------------------------------------------------------------------------------

talk_colleagues_rep_perc <- overalldata %>%
  group_by(talk_colleagues_rep) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round)

## Question rep percentage -------------------------------------------------------------------------------------------------

question_rep_perc <- overalldata %>%
  group_by(question_rep) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round)

# Funder frequencies ------------------------------------------------------------------------------------------------------

# Funder effort percentage

funder_efforts_perc <- funder_data %>%
  group_by(funder_efforts) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round)

# Funder effort helpful percentage

funder_efforts_helpful_perc <- funder_data %>%
  group_by(funder_efforts_helpful) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round)

# Funder positive changes percentage

funder_positive_changes_perc <- funder_data %>%
  group_by(funder_positive_changes) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round)

# Funder encourage percentage

funder_encourage_perc <- funder_data %>%
  group_by(funder_encourage) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round)

# Journal frequencies ----------------------------------------------------------------------------------------------------

# Journal efforts percentage

journal_efforts_perc <- journal_data %>%
  group_by(journal_efforts) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round)

# Journal efforts positive percentage

journal_efforts_positive_perc <- journal_data %>%
  group_by(journal_efforts_positive) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round)

# Journal efforts helpful percentage

journal_efforts_helpful_perc <- journal_data %>%
  group_by(journal_efforts_helpful) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round)

# Journal encourage percentage

journal_encourage_perc <- journal_data %>%
  group_by(journal_encourage) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round)

# Labgroup frequencies ----------------------------------------------------------------------------------------------------

# Labgroup procedures (overall)

labgroup_procedures_perc <- labgroup_data %>%
  group_by(labgroup_procedures) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round)

# IMPORTANT n = 286 established procedures for reproducibility and reproducibility therefore n = 223 had skip logic for the following questions

# Labgroup established procedures percentage 

labgroup_establish_procedures_perc <- labgroup_data %>%
  group_by(labgroup_establish_procedures) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(286)*100)) %>%
  mutate_if(is.numeric, round) %>%
  drop_na()

# Labgroup procedures changes percentage

labgroup_procedures_changes_perc <- labgroup_data %>%
  group_by(labgroup_procedures_changes) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(286)*100)) %>%
  mutate_if(is.numeric, round) %>%
  drop_na()

# Labgroup procedures impact percentage

labgroup_procedures_impact_perc <- labgroup_data %>%
  group_by(labgroup_procedures_impact) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(286)*100)) %>%
  mutate_if(is.numeric, round) %>%
  drop_na()

# Labgroup further changes percentage

labgroup_further_changes_perc <- labgroup_data %>%
  group_by(labgroup_further_changes) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(286)*100)) %>%
  mutate_if(is.numeric, round) %>%
  drop_na()

# Barriers frequencies ----------------------------------------------------------------------------------------------------

renameddata %>%
  group_by(barriers_changes) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round)

# Failure factor frequencies ----------------------------------------------------------------------------------------------------

failure_data %>%
  group_by(fraud) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round) %>%
  drop_na()

failure_data %>%
  group_by(publish_pressure) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round) %>%
  drop_na()

failure_data %>%
  group_by(insufficient_mentor) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round) %>%
  drop_na()

failure_data %>%
  group_by(insufficient_peer_review) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round) %>%
  drop_na()

renameddata %>%
  group_by(bad_luck) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round) %>%
  drop_na()