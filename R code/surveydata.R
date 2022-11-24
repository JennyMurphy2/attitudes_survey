
if(!require(tidyverse))install.packages("tidyverse")

# Data loading --------------------------------------------------------------------------------------------------
dat <- read_csv ("survey.csv")

summary(dat)
spec(dat)
str(dat)

# Rename variables --------------------------------------------------------------------------------------------------

renameddata <- dat %>%
  rename(familiar_repro = `Column`,
         familiar_rep = `2`,
         replication_rate = `3`,
         crisis =`Which of the following statement regarding a “reproducibility crisis” or “replication crisis” within the sports and exercise science community do you agree with?`,
         flagged_crisis = `To what extent do you feel that the "reproducibility crisis" or "replication crisis" is suitably flagged?`,
         published_results =`3`,
         level_rep = `Please complete the following sentence: "In my opinion, the level of reproducibility or replicability in my field is...`,
         major_problem_myfield = `"I think that the failure to reproduce or replicate scientific studies is a major problem in my field"`,
         major_problem_allfields = `"I think that the failure to reproduce or replicate scientific studies is a major problem for all fields"`,
        often_think_rep = `Think about reproducibility or replicability of your research`,
        talk_colleagues_rep =`Speak to your colleagues about reproducibility or replicability`,
        question_rep =`Question the reproducibility or replicability of other scientists’ work`,
        funder_efforts =`Have you encountered efforts or directives from funding agencies designed to improve reproducibility and replicability of the work you do?`,
        funder_efforts_helpful =`“I have found efforts from funding agencies helpful for my work.”`,
        funder_positive_changes =`“Efforts from funding agencies will lead to positive changes in my field.”`,
        funder_encourage =`“Funding agencies should do more to encourage or enforce better reproducibility and replicability.”`,
        journal_efforts =`Have you encountered efforts from journal publishers designed to enhance or ensure the reproducibility and replicability of your work (e.g. using checklists to ensure standards for describing rese...`,
        journal_efforts_positive = `“Efforts made by journal publishers have had a positive effect on my field.”`,
        journal_efforts_helpful =`“Efforts made by journal publishers have been helpful to my work.”`,
        journal_encourage =`“Journal publishers should do more to enforce or encourage reproducibility and replicability in my field.”`,
        labgroup_procedures =`Have you and/or your lab group established any procedures to ensure reproducibility and replicability in your work?`,
        labgroup_establish_procedures =`When did you and/or your lab group establish these procedures?`,
        labgroup_procedures_changes =`Do you think the quality of your research changed after these changes were introduced?`,
        labgroup_procedures_impact = `What kind of an impact, overall, have the changes you made to ensure reproducibility and replicability had on your lab?`,
        labgroup_further_changes =`Do you think that you and/or your lab should implement any further changes?`,
        barriers_changes =`Have you identified any barriers to implementing changes that would improve reproducibility and replicability of research in your lab?`,
        volunteer_replication = `Large projects such as the Reproducibility Project: Psychology, Many Labs and the Reproducibility Project: Cancer Biology have investigated the reproducibility and replicability of their fields. I...`,
        failure_rep_wrong =`“I think that a failure to reproduce or replicate a result most often means that the original finding is wrong.”`,
        failure_detracts_validity =`“I think that a failure to reproduce or
replicate rarely detracts from the validity of the original finding.”`,
        fraud =`Fraud (i.e. fabricated or falsified results)`,
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
        professional_incentives_practices = `Professional incentives (e.g. funding or credit towards tenure) for adopting practices that enhance reproducibility or replicability`,
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
        failed_own =`Tried and failed to reproduce or replicate one of your own experiments`,
        failed_another =`Tried and failed to reproduce or replicate someone else's experiment`,
        published_successful_another =`Published a successful attempt to reproduce or replicate someone else's work`,
        published_failed_another =`Published a failed attempt to reproduce or replicate someone else's work`,
        failed_publish_successful =`Tried and failed to publish a successful reproduction or replication`,
        failed_publish_unsuccesful =`Tried and failed to publish an unsuccessful reproduction or replication`,
        told_about_own_failed =`Has anyone ever told you that they could not reproduce or replicate results from one of your own experiments?`,
        job_title =`Which of the following job titles best applies to you?`,
        research_area =`Which of the following best describes your area of interest? Tick all that apply.`,
        age = `What is your age?`,
        continent =`In which continent do you live?`
)

# Drop data --------------------------------------------------------------------------------------------------
# removing respondent who did not fit inclusion criteria (sports and exercise science researcher)

renameddata <- renameddata[ !(renameddata$job_title %in% c("Veterinarian")), ]

# Overall data --------------------------------------------------------------------------------------------------
# grouping variables based on survey question factors contributing to failure to replicate
overalldata <- renameddata %>%
  select(ID, familiar_repro, familiar_rep, crisis, flagged_crisis, published_results,level_rep, 
         major_problem_myfield, major_problem_allfields, often_think_rep, talk_colleagues_rep,
         question_rep
  )


# Failure factors data --------------------------------------------------------------------------------------------------
# grouping variables based on survey question factors contributing to failure to replicate

failure_data <- renameddata %>%
  select(fraud,publish_pressure,insufficient_mentor,insufficient_peer_review, selective_reporting,
         not_robust,low_power, mistakes, raw_data_unavailable,
         protocol_code_unavailable,specific_technical_expertise,poor_exp_design,bad_luck
  )

# Funder data --------------------------------------------------------------------------------------------------
# grouping variables based on survey question funder efforts

funder_data <- renameddata %>%
  select(funder_efforts, funder_efforts_helpful, funder_positive_changes, funder_encourage)

# Journal data --------------------------------------------------------------------------------------------------
# grouping variables based on survey question journal efforts

journal_data <- renameddata %>%
  select(journal_efforts, journal_efforts_positive, journal_efforts_helpful,journal_encourage)

# Labgroup data --------------------------------------------------------------------------------------------------
# grouping variables based on survey question lab group efforts

labgroup_data <- renameddata %>%
  select(labgroup_procedures,labgroup_establish_procedures, labgroup_procedures_changes, 
         labgroup_procedures_impact,labgroup_further_changes)

# Improve data --------------------------------------------------------------------------------------------------
# grouping variables based on survey question factors that would improve reproducibility or replicability

improve_data <- renameddata %>%
  select(professional_incentives_practices,
         professional_incentives_replication,better_teaching,better_mentoring,better_stat_understanding,robust_exp_design,
         within_lab_validation,independent_replication,journal_standards,time_mentoring,time_lab_checks 
  )

# Experiment data --------------------------------------------------------------------------------------------------
# grouping variables based on survey questions about attempting replications

experiment_data <- renameddata %>%
  select(ID, failed_own,failed_another,published_successful_another,published_failed_another,
         failed_publish_successful,failed_publish_unsuccesful,told_about_own_failed 
  )




