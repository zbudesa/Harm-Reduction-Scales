#####################################
#     Download Study 3 Responses    #
#####################################

# Packages
library(qualtRics)
library(tidyverse)

# Get all survey IDs
surveys <- all_surveys()

# Create Codebook ----
## Download Items
item <- survey_questions(surveys$id[surveys$name == "Harm Reduction Scales - Study 3 - Confirmation pt.2"]) 

item <- item %>% 
  filter(qname != "consent") %>% 
  select(scale = NULL, var_label = qname, item = question,
         values = NULL, missing_values = NULL)

write.csv(item, "references/codebooks/20231025_hr-scale-codebook.csv")

# Download Responses & Clean ----
resp <- fetch_survey(surveyID = 
                       surveys$id[surveys$name == "Harm Reduction Scales - Study 3 - Confirmation pt.2"],
                    force_request = TRUE,
                    include_display_order = FALSE,
                    include_embedded = c("PROLIFIC_PID", "Q_RecaptchaScore"),
                    label = TRUE, convert = FALSE)

df <- resp %>% 
  select(ResponseId, PROLIFIC_PID, 
         Q_RecaptchaScore, `Duration (in seconds)`, 
         q1:q45, age:sud_hx)


# Write File
write.csv(df, "data/raw/20231025_hr-scale-confirmatory-data2.csv")


