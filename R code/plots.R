
if(!require(ggplot2))install.packages("ggplot2")
if(!require(scales))install.packages("scales")
if(!require(reshape2))install.packages("reshape2")

## NEED TO RUN "surveydata.R" then "frequencies.R" and then this code

# Graph settings ----------------------------------------------------------------------------------------------------------
#setting theme for all graphs

theme_set(theme_bw())
  
# Crisis percent plot ---------------------------------------------------------------------------------------------------

 percent_crisis %>%
  ggplot(aes(x = crisis, y = pct, fill = crisis, label = scales::percent(pct))) +
    geom_col(position = 'dodge', show.legend = FALSE, color = "black") + 
    geom_text(position = position_dodge(width = .9),    # move to center of bars
              vjust = -0.5,    # nudge above top of bar
              size = 5) + 
    scale_y_continuous(name = "Percentage (%)", labels = scales::percent) +
      scale_x_discrete(name = "", 
                       labels = c("I don't know", "No crisis", "Significant crisis" , "Slight crisis", "Unanswered")) +
  theme_bw() +
      theme(axis.text.x = element_text(hjust = 0.5, size=12, face="bold"),
            axis.title.y = element_text(vjust = 2, size=12, face="bold"),
            axis.text.y = element_text(size=10, face="bold"),
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.border = element_rect(color = "grey", size = 2)) +
  scale_fill_viridis_d(option = "E")


ggsave("percentcrisis.png",
  plot = last_plot(),
  device = "png",
  units = "mm",
  width = 200,
  height = 160,
  dpi = 300,
  limitsize = TRUE
)

# Proportion of published results ----------------------------------------
# Survey question: "In your opinion, how likely are published results in the field of Sports and Exercise Science reproducible or replicable?"  

published_results_perc$published_results <- factor(published_results_perc$published_results, levels = published_results_perc$published_results)

published_results_perc %>%
  ggplot(aes(x = published_results, y = pct, fill = published_results, label = scales::percent(pct))) +
  geom_col(position = 'dodge', show.legend = FALSE, color = "black") + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 5) + 
  scale_y_continuous(name = "Percentage (%)", labels = scales::percent, expand = expansion(mult = c(0, .1))) +
  scale_x_discrete(name = "",
                   labels = wrap_format(10)) +
  theme(axis.text.x = element_text(hjust = 0.5, size=12, face="bold"),
        axis.title.y = element_text(vjust = 2, size=12, face="bold"),
        axis.text.y = element_text(size=10, face="bold"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(color = "grey", fill = NA, size = 2)) +
  scale_fill_viridis_d(option = "E")

ggsave("publishedresults.png",
       plot = last_plot(),
       device = "png",
       units = "mm",
       width = 200,
       height = 160,
       dpi = 300,
       limitsize = TRUE
)

## Level of repro or replic in the field ----------------------------------------
# Survey question: "In my opinion, the level of reproducibility or replicability in my field is..."

level_rep_perc %>%
  ggplot(aes(x = level_rep, y = pct, fill = level_rep, label = scales::percent(pct))) +
  geom_col(position = 'dodge', show.legend = FALSE, color = "black") + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 5) + 
  scale_y_continuous(name = "Percentage (%)", labels = scales::percent, expand = expansion(mult = c(0, .1))) +
  scale_x_discrete(name = "",
                   labels = c("Same", "Better" , "Worse", "Unsure", "Unanswered")) +
  theme(axis.text.x = element_text(hjust = 0.5, size=12, face="bold"),
        axis.title.y = element_text(vjust = 2, size=12, face="bold"),
        axis.text.y = element_text(size=10, face="bold"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(color = "grey", fill = NA, size = 2)
) +
  scale_fill_viridis_d(option = "E")

ggsave("levelrep.png",
       plot = last_plot(),
       device = "png",
       units = "mm",
       width = 200,
       height = 160,
       dpi = 300,
       limitsize = TRUE
)

# Major problem in my field/all fields ----------------------------------------
# Survey question: "I think that the failure to reproduce or replicate scientific studies is a major problem in my field"
# Survey question: "I think that the failure to reproduce or replicate scientific studies is a major problem for all fields"

major_dat <- 
  overalldata %>%
  select(major_problem_allfields, major_problem_myfield) %>%
  rename("My field" =  major_problem_myfield,
         "All fields" = major_problem_allfields) %>%
  drop_na()

major_dat$id <- 1:nrow(major_dat)
major_dat <- melt(major_dat,id.vars = "id")

major_data_perc <- 
  major_dat %>%
  group_by(variable) %>%
  count(value = factor(value)) %>% 
  mutate(pct = prop.table(n))

major_data_responses <- c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree")

# barchart

major_data_perc %>% 
  ggplot(aes(x = value, y = pct, fill = variable, label = scales::percent(pct))) + 
  geom_col(position = 'dodge', color = "black") + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 5) + 
  scale_y_continuous(name = "Percentage (%)", labels = scales::percent) +
  scale_x_discrete(name = "", limits = major_data_responses, labels = wrap_format(10)) +
  labs(fill = "") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(hjust = 0.5, size=12, face="bold"),
        axis.title.y = element_text(vjust = 2, size=12, face="bold"),
        axis.text.y = element_text(size=10, face="bold"),
        legend.justification = c(1, 1), 
        legend.position = c(0.95, 0.95),
        legend.text = element_text(size = 12),
        panel.border = element_rect(color = "grey", fill = NA, size = 2)
) +
  scale_fill_viridis_d(option = "C")

ggsave("majorproblem.png",
       plot = last_plot(),
       device = "png",
       units = "mm",
       width = 250,
       height = 210,
       dpi = 300,
       limitsize = TRUE
)

# Funder efforts plot --------------------------------------------------------------------------------------------
# Prepare the data

funder_dat <- 
  renameddata %>%
  select(funder_efforts_helpful,funder_positive_changes,funder_encourage) %>%
  rename("Efforts from funding agencies\nare helpful" = funder_efforts_helpful,
          "Efforts from funding agencies \nwill lead to positive changes" = funder_positive_changes,
        "Funding agencies should encourage \nreproducibility and replicability" = funder_encourage)

funder_dat$id <- 1:nrow(funder_dat)
funder_dat <- melt(funder_dat,id.vars = "id")

funder_dat_perc <- 
  funder_dat %>%
  group_by(variable) %>%
  count(value = factor(value)) %>% 
  mutate(pct = prop.table(n))

funder_dat_responses <- c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree")

# Plot data

funder_dat_perc %>% 
  ggplot(aes(x = value, y = pct, fill = variable, label = scales::percent(pct))) + 
  geom_col(position = 'dodge', colour = "black") + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  scale_y_continuous(name = "Percentage (%)", labels = scales::percent) +
  scale_x_discrete(name = "", limits = funder_dat_responses, labels = wrap_format(10)) +
  labs(fill = "", labels = wrap_format(10)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position= "bottom",
        legend.text = element_text(size = 9),
        axis.text.x = element_text(hjust = 0.5, size=12, face="bold"),
        axis.title.y = element_text(vjust = 2, size=12, face="bold"),
        axis.text.y = element_text(size=10, face="bold"),
        legend.margin = margin(t=0),
        panel.border = element_rect(color = "grey", fill = NA, size = 2)
) +
  scale_fill_viridis_d(option = "C") 

ggsave("fundereffort.png",
       plot = last_plot(),
       device = "png",
       units = "mm",
       width = 200,
       height = 160,
       dpi = 300,
       limitsize = TRUE
)

# Journal efforts plot --------------------------------------------------------------------------------------------
# Prepare the data

journal_dat <- 
  renameddata %>%
  select(journal_efforts_positive, journal_efforts_helpful, journal_encourage) %>%
  rename("Efforts from journal publishers \nhave had a positive effect" = journal_efforts_positive,
         "Efforts from journal publishers \nare helpful" = journal_efforts_helpful,
         "Journal publishers should encourage \nreproducibility and replicability" = journal_encourage)

journal_dat$id <- 1:nrow(journal_dat)
journal_dat <- melt(journal_dat,id.vars = "id")

journal_dat_perc <- 
  journal_dat %>%
  group_by(variable) %>%
  count(value = factor(value)) %>% 
  mutate(pct = prop.table(n))

journal_dat_responses <- c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree")

# Plot data
journal_dat_perc %>% 
  ggplot(aes(x = value, y = pct, fill = variable, label = scales::percent(pct))) + 
  geom_col(position = 'dodge', colour = "black") + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  scale_y_continuous(name = "Percentage (%)", labels = scales::percent) +
  scale_x_discrete(name = "", limits = journal_dat_responses, labels = wrap_format(10)) +
  labs(fill = "", labels = wrap_format(10)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position= "bottom",
        legend.text = element_text(size = 9),
        axis.text.x = element_text(hjust = 0.5, size=12, face="bold"),
        axis.title.y = element_text(vjust = 2, size=12, face="bold"),
        axis.text.y = element_text(size=10, face="bold"),
        legend.margin = margin(t=0),
        plot.title = element_text(face="bold", hjust = 0.5, size = 16),
        panel.border = element_rect(color = "grey", fill = NA, size = 2)) +
  scale_fill_viridis_d(option = "C") 

ggsave("journaleffort.png",
       plot = last_plot(),
       device = "png",
       units = "mm",
       width = 200,
       height = 160,
       dpi = 300,
       limitsize = TRUE
)

# Failed replication plot ---------------------------------------------------------------------------------------------------

fail_data <- 
  renameddata %>%
  select(failure_rep_wrong, failure_detracts_validity) %>%
  rename("A failure to reproduce or replicate a result most \noften means that the original finding is wrong" = failure_rep_wrong,
        "A failure to reproduce or replicate rarely \ndetracts from the validity of the original finding" = failure_detracts_validity) %>% 
  drop_na()

fail_data$id <- 1:nrow(fail_data)
fail_data <- melt(fail_data,id.vars = "id")

fail_data_perc <- 
  fail_data %>%
  group_by(variable) %>%
  count(value = factor(value)) %>% 
  mutate(pct = prop.table(n))

fail_data_responses <- c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree")

# barchart

fail_data_perc %>% 
  ggplot(aes(x = value, y = pct, fill = variable, label = scales::percent(pct))) + 
  geom_col(position = 'dodge', colour = "black") + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 5) + 
  scale_y_continuous(name = "Percentage (%)", labels = scales::percent) +
  scale_x_discrete(name = "", limits = fail_data_responses, labels = wrap_format(10)) +
  labs(fill = "", labels = wrap_format(10)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(hjust = 0.5, size=12, face="bold"),
        axis.title.y = element_text(vjust = 2, size=12, face="bold"),
        axis.text.y = element_text(size=10, face="bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        panel.border = element_rect(color = "grey", fill = NA, size = 2)
  ) +
  scale_fill_viridis_d(option = "C")

ggsave("failedmeaning.png",
       plot = last_plot(),
       device = "png",
       units = "mm",
       width = 250,
       height = 210,
       dpi = 300,
       limitsize = TRUE
)
