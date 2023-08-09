###########################
#  Download RPE Responses #
###########################


library(qualtRics)
library(tidyverse)

View(
all_surveys()
)

surveys <- all_surveys()

qs <- paste0("Item",seq(1:14))

rpe <- fetch_survey(surveyID = surveys$id[surveys$name == "Harm Reduction Scales - Response Process Evaluation"],
                    include_questions = qs,
                    force_request = TRUE)

items <- survey_questions(surveys$id[surveys$name == "Harm Reduction Scales - Response Process Evaluation"])

items %>% 
  filter(grepl("<h1 style=", question)) %>% 
  select(question) %>% 
  
