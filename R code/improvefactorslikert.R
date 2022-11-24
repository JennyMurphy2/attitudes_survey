if(!require(tidyverse))install.packages("tidyverse")
if(!require(likert))install.packages("likert")
if(!require(ggplot2))install.packages("ggplot2")
if(!require(dplyr))install.packages("dplyr")
if(!require(scales))install.packages("scales")

# Data loading --------------------------------------------------------------------------------------------------
# only use this if data has not been loaded in using surveydata.R

dat <- read_csv ("survey.csv")

str(dat)

# Rename variables --------------------------------------------------------------------------------------------------

improve_data <- dat %>%
  rename(professional_incentives_practices = `Professional incentives (e.g. funding or credit towards tenure) for adopting practices that enhance reproducibility or replicability`,
         professional_incentives_replication =`Professional incentives (e.g. funding or publications) for formally reproducing or replicating the work of others`,
         better_teaching =`Better teaching of science students`,
         better_mentoring =`Better mentoring/supervision of students, postdocs and other trainees in the lab`,
         better_stat_understanding =`Better understanding of statistics`,
         robust_exp_design =`More robust experimental design e.g. larger sample sizes with statistical power analysis, use of control groups, randomisation of participants etc.`,
         within_lab_validation =`More emphasis on independent validation within the lab`,
         independent_replication =`More emphasis on independent replication via outside labs`,
         journal_standards =`Journal editors enforcing standards to enhance reproducibility or replicability (e.g. through checklists)`,
         time_mentoring =`More time to teach and mentor students and other workers in the lab`,
         time_lab_checks =`More time checking lab notebooks and raw data`,
         job_title =`Which of the following job titles best applies to you?`
  )

# Drop data --------------------------------------------------------------------------------------------------
# removing respondent who did not fit inclusion criteria (sports and exercise science researcher)

improve_data <- improve_data[ !(improve_data$job_title %in% c("Veterinarian")), ]


# Improving factors likert--------------------------------------------------------------------------------------------------

improve_data <- improve_data %>%
  select(professional_incentives_practices, professional_incentives_replication, better_teaching,
         better_mentoring, better_stat_understanding, robust_exp_design, within_lab_validation,
         independent_replication,journal_standards,time_mentoring, time_lab_checks 
  )

## Prepare the data ----------------------------------------------------------------------------------------------------------

improve_likdata <- as.data.frame(improve_data)
str(improve_likdata)

improve_levels = c("Very likely", "Fairly likely", "Somewhat likely", "Neither likely nor unlikely", "Somewhat unlikely", "Fairly unlikely", "Very unlikely")

# convert each variable to factors

improve_likdata$professional_incentives_practices = factor(improve_likdata$professional_incentives_practices, improve_levels, ordered = TRUE)
improve_likdata$professional_incentives_replication = factor(improve_likdata$professional_incentives_replication, improve_levels, ordered = TRUE)
improve_likdata$better_teaching = factor(improve_likdata$better_teaching, improve_levels, ordered = TRUE)
improve_likdata$better_mentoring = factor(improve_likdata$better_mentoring, improve_levels, ordered = TRUE)
improve_likdata$better_stat_understanding = factor(improve_likdata$better_stat_understanding, improve_levels, ordered = TRUE)
improve_likdata$robust_exp_design = factor(improve_likdata$robust_exp_design, improve_levels, ordered = TRUE)
improve_likdata$within_lab_validation = factor(improve_likdata$within_lab_validation, improve_levels, ordered = TRUE)
improve_likdata$independent_replication = factor(improve_likdata$independent_replication, improve_levels, ordered = TRUE)
improve_likdata$journal_standards = factor(improve_likdata$journal_standards, improve_levels, ordered = TRUE)
improve_likdata$time_mentoring = factor(improve_likdata$time_mentoring, improve_levels, ordered = TRUE)
improve_likdata$time_lab_checks = factor(improve_likdata$time_lab_checks, improve_levels, ordered = TRUE)

# Rename data -------------------------------------------------------------------------------------------------------------

improve_likdata <- 
  improve_likdata %>%
  rename("More time checking lab notebooks" = time_lab_checks,
         "Professional incentives for replication" = professional_incentives_replication,
         "Better teaching" = better_teaching,
         "Journal standards for reproducibility or replicability" = journal_standards,
         "Professional incentives for adopting open practices" = professional_incentives_practices,
         "More mentoring time" = time_mentoring,
         "More emphasis on independent validation within the lab" = within_lab_validation,
         "More emphasis on independent replication via outside labs" = independent_replication,
         "Better mentoring/supervision" = better_mentoring,
         "More robust experimental design" = robust_exp_design,
         "Better understanding of statistics" = better_stat_understanding)

# note to self - have to rename after stating them as factors for the likert plot otherwise they are characters which doesn't work

## Create likert plot ----------------------------------------------------------------------------------------------------------

results <- likert(as.data.frame(improve_likdata))
plot(results) +
#  ggtitle("How likely are these factors to improve reproducibility/replicabilty") +
  theme(
    plot.title = element_text(face="bold", hjust = 0.5),
    axis.title.x = element_text(face="bold"),
    axis.text.y = element_text(size=10, face="bold")
    ) +
  scale_x_discrete(labels = wrap_format(30))

## Save plot ------------------------------------------------------------------------------------------------------------------

ggsave("likelyfactors14July.png",
       plot = last_plot(),
       device = "png",
       width = NA,
       height = NA,
       dpi = 300,
       limitsize = TRUE
)
