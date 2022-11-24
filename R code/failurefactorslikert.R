if(!require(tidyverse))install.packages("tidyverse")
if(!require(likert))install.packages("likert")
if(!require(ggplot2))install.packages("ggplot2")
if(!require(dplyr))install.packages("dplyr")
if(!require(scales))install.packages("scales")
library(sjlabelled)


# Data loading --------------------------------------------------------------------------------------------------
# only use this if data has not been loaded in using surveydata.R

dat <- read_csv ("survey.csv")

str(dat)

# Rename variables --------------------------------------------------------------------------------------------------

renameddata <- dat %>%
  rename(fraud =`Fraud (i.e. fabricated or falsified results)`,
         publish_pressure =`Pressure to publish for career advancement`,
         insufficient_mentor =`Insufficient oversight/mentoring by lab principal investigator (e.g. reviewing raw data)`,
         insufficient_peer_review =`Insufficient peer review of research`,
         selective_reporting =`Selective reporting of results`,
         not_robust =`Original findings not robust enough because not replicated enough in the lab publishing the work`,
         low_power =`Original findings obtained with low statistical power/poor statistical analysis`,
         mistakes =`Mistakes or inadequate expertise in reproduction efforts`,
         raw_data_unavailable =`Raw data not available from original lab`,
         protocol_code_unavailable =`Protocols and computer code insufficient or not available from original lab`,
         specific_technical_expertise =`Methods need specific technical expertise that is difficult for others to reproduce`,
         poor_exp_design =`Poor experimental design`,
         bad_luck =`Bad luck`,
         job_title =`Which of the following job titles best applies to you?`
)

# Drop data --------------------------------------------------------------------------------------------------
# removing respondent who did not fit inclusion criteria (sports and exercise science researcher)

renameddata <- renameddata[ !(renameddata$job_title %in% c("Veterinarian")), ]

# Failure factors data --------------------------------------------------------------------------------------------------
# grouping variables based on survey question factors contributing to failure to replicate

failure_data <- renameddata %>%
  select(fraud,publish_pressure,insufficient_mentor,insufficient_peer_review, selective_reporting,
         not_robust,low_power, mistakes, raw_data_unavailable,
         protocol_code_unavailable,specific_technical_expertise,poor_exp_design,bad_luck
  )

# only needs to use the following code if an ID variable was added
#failure_data <- renameddata %>%
#  select(-(ID)
#  )

# Failure factors likert -----------------------------------------------------------------------------------------------------
## Prepare the data structure ----------------------------------------------------------------------------------------------------------

# convert data from tibble to dataframe

likdata <- as.data.frame(failure_data)
str(likdata)

# reorder the factors

levels= c("Always contributes", "Very often contributes", "Sometimes contributes", "I don't know", "Rarely contributes", "Never contributes")

# convert each variable to factors

likdata$fraud = factor(likdata$fraud, levels, ordered = TRUE)
likdata$publish_pressure = factor(likdata$publish_pressure, levels, ordered = TRUE)
likdata$insufficient_mentor = factor(likdata$insufficient_mentor, levels, ordered = TRUE)
likdata$insufficient_peer_review = factor(likdata$insufficient_peer_review, levels, ordered = TRUE)
likdata$selective_reporting = factor(likdata$selective_reporting, levels, ordered = TRUE)
likdata$not_robust = factor(likdata$not_robust, levels, ordered = TRUE)
likdata$low_power = factor(likdata$low_power, levels, ordered = TRUE)
likdata$mistakes = factor(likdata$mistakes, levels, ordered = TRUE)
likdata$raw_data_unavailable = factor(likdata$raw_data_unavailable, levels, ordered = TRUE)
likdata$protocol_code_unavailable = factor(likdata$protocol_code_unavailable, levels, ordered = TRUE)
likdata$specific_technical_expertise = factor(likdata$specific_technical_expertise, levels, ordered = TRUE)
likdata$poor_exp_design = factor(likdata$poor_exp_design, levels, ordered = TRUE)
likdata$bad_luck = factor(likdata$bad_luck, levels, ordered = TRUE)

## Rename the data -------------------------------------------------------------------------------------------------------------

likdata2 <- 
  likdata %>%
  rename("Bad luck" = bad_luck,
         "Original protocol code unavailability" = protocol_code_unavailable,
         "Specific technical expertise" = specific_technical_expertise,
         "Low statistical power" = low_power,
         "Original findings not robust" = not_robust,
         "Original raw data unavailability" = raw_data_unavailable,
         "Insufficient peer review" = insufficient_peer_review,
         "Poor experimental design" = poor_exp_design,
         "Insufficient mentoring" = insufficient_mentor,
         "Pressure to publish" = publish_pressure,
         "Selective reporting of results" = selective_reporting,
         "Mistakes" = mistakes,
         "Fraud" = fraud)

# note to self - have to rename after stating them as factors for the likert plot otherwise they are characters which doesn't work

## Create likert plot ----------------------------------------------------------------------------------------------------------

results <- likert(as.data.frame(likdata2))
#likert.options(plot.percent.neutral = TRUE)

plot(results, center = 4, wrap = 20) +
#  ggtitle("Factors contributing to a failure to replicate") +
  theme(
    axis.text.y = element_text(size=10, face="bold"),
    axis.title.x = element_text(face="bold"),
) +
scale_x_discrete(labels = wrap_format(30))

## Save plot ------------------------------------------------------------------------------------------------------------------

ggsave("failurefactors14July.png",
       plot = last_plot(),
       device = "png",
       width = NA,
       height = NA,
       dpi = 300,
       limitsize = TRUE
)