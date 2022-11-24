
if(!require(gt))install.packages("gt")

## NEED TO RUN "surveydata.R" then this code

# Experiment statements ----------------------------------------------------------------------------------------------------

## Prepare the data ----------------------------------------------------------------------------------------------------

structure(experiment_data)
summarise(failure_data)

#only need the following code if ID has been inserted to the dataframe

#have to drop the n now to make the next bit work

experiment_data <-
  experiment_data %>%
  select(-ID)

experiment_data_longer <-
  experiment_data %>%
  pivot_longer(cols = everything())

experiment_data_longer <-
  experiment_data_longer %>%
  group_by(name) %>%
  count(value) %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round)

experiment_data_longer <-
  experiment_data_longer %>%
  select(-c(n))

experiment_data_wider <-
  experiment_data_longer %>%
  pivot_wider(names_from = name, values_from = percent) %>%
  drop_na()

experiment_data_wider <-
  experiment_data_wider %>%
  arrange(match(value, c("Yes", "No", "I can't remember")))

## Create experiment table ----------------------------------------------------------------------------------------------------

experiment_data_wider %>%
  gt() %>%
  tab_header(title = "Experiment Statements",
             subtitle = NULL) %>%
  cols_align("center",
             columns = everything()) %>%
  cols_width(starts_with("value") ~ px(150),
             everything() ~ px(100)) %>%
  cols_label(
    value = "Response",
    failed_own = "Tried and failed to reproduce or replicate one of your own experiments",
    failed_another = "Tried and failed to reproduce or replicate someone else's experiment",
    published_successful_another = "Published a successful attempt to reproduce or replicate someone else's work",
    published_failed_another = "Published a failed attempt to reproduce or replicate someone else's work",
    failed_publish_successful = "Tried and failed to publish a successful reproduction or replication",
    failed_publish_unsuccesful = "Tried and failed to publish an unsuccessful reproduction or replication",
    told_about_own_failed = "Has anyone ever told you that they could not reproduce or replicate results from one of your own experiments?") %>%
  tab_source_note(source_note = "N = 511 responses reported as percentage") %>%
  tab_options(
    table.width = 100,
    heading.title.font.weight = "bolder",
    #not working
    column_labels.padding = 10,
    heading.title.font.size = 24,
    table.border.top.color = "black",
    heading.border.bottom.color = "black",
    table.border.bottom.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(3),
  ) %>%
  gtsave("experimentstatementspercentarr.png", vwidth = 1500, vheight = 1000)

# Often statements ----------------------------------------------------------------------------------------------------

## Prepare the data ----------------------------------------------------------------------------------------------------

often_data <-
  renameddata %>%
  select(often_think_rep, talk_colleagues_rep, question_rep)

often_data_longer <-
  often_data %>%
  pivot_longer(cols = everything())

often_data_longer <-
  often_data_longer %>%
  group_by(name) %>%
  count(value) %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate_if(is.numeric, round)

often_data_longer <-
  often_data_longer %>%
  select(-n)

often_data_wider <-
  often_data_longer %>%
  pivot_wider(names_from = name, values_from = percent) %>%
  drop_na()

often_data_wider <-
  often_data_wider %>%
  arrange(match(value, c("Daily", "Weekly", "Monthly", "Quarterly", "Never")))


## Create often statements table ----------------------------------------------------------------------------------------------------

often_data_wider %>%
  gt() %>%
  tab_header(title = "Often Statements",
             subtitle = NULL) %>%
  cols_align("center",
             columns = everything()) %>%
  cols_width(starts_with("value") ~ px(150),
             everything() ~ px(100)) %>%
cols_width(
    value ~ px(150),
    everything() ~ px(200)) %>%
  cols_label(
    value = "Response",
    often_think_rep = "Think about reproducibility or replicability of your research",
    talk_colleagues_rep = "Speak to your colleagues about reproducibility or replicability",
    question_rep = "Question the reproducibility or replicability of other scientists’ work") %>%
  tab_source_note(source_note = "N = 511 responses reported as percentage") %>%
  tab_options(
    table.width = 100,
    heading.title.font.weight = "bolder",
    #not working
    column_labels.padding = 10,
    heading.title.font.size = 24,
    table.border.top.color = "black",
    heading.border.bottom.color = "black",
    table.border.bottom.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(3),
  ) %>%
  gtsave("oftenstatements.png", vwidth = 1500, vheight = 1000)
