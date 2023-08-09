###########################
#  Download RPE Responses #
###########################


library(qualtRics)
library(tidyverse)


surveys <- all_surveys()

qs <- paste0("Item",seq(1:14))

rpe <- fetch_survey(surveyID = 
                      surveys$id[surveys$name == "Harm Reduction Scales - Response Process Evaluation"],
                    force_request = TRUE)

items <- survey_questions(surveys$id[surveys$name == "Harm Reduction Scales - Response Process Evaluation"])

items %>% 
  filter(grepl("<h1 style=", question)) %>% 
  select(question) %>% 
  
