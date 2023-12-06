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
item_resp <- fetch_survey(surveyID = 
                            surveys$id[surveys$name == "Harm Reduction Scales - Study 3 - Confirmation pt.2"],
                          force_request = TRUE,
                          include_display_order = FALSE,
                          include_embedded = c("PROLIFIC_PID", "Q_RecaptchaScore"),
                          label = FALSE, convert = FALSE)

item_resp <- item_resp %>% 
  select(ResponseId, PROLIFIC_PID, 
         Q_RecaptchaScore, `Duration (in seconds)`, 
         attn, q1:q12, q13:q45) 

demo_resp <- fetch_survey(surveyID = 
                            surveys$id[surveys$name == "Harm Reduction Scales - Study 3 - Confirmation pt.2"],
                          force_request = TRUE,
                          include_display_order = FALSE,
                          include_embedded = c("PROLIFIC_PID", "Q_RecaptchaScore"),
                          label = TRUE, convert = FALSE)

demo_resp <- demo_resp %>% 
  select(ResponseId,
         age:sud_hx)

df <- item_resp %>% 
  left_join(demo_resp,
            by = "ResponseId")


# Write File
write.csv(df, "data/raw/03_study3_data.csv")


