#####################################
#     Download Study 2 Responses    #
#####################################

# Packages
library(qualtRics)
library(tidyverse)

# Get all survey IDs
surveys <- all_surveys()

# Create Codebook ----
## Download Items
item <- survey_questions(surveys$id[surveys$name == "Harm Reduction Scales - Study 2 - Confirmation"]) 

item <- item %>% 
  filter(qname != "consent") %>% 
  select(scale = NULL, var_label = qname, item = question,
         values = NULL, missing_values = NULL)

write.csv(item, "references/codebooks/20231011_hr-scale-codebook.csv")

# Download Responses & Clean ----
resp <- fetch_survey(surveyID = 
                            surveys$id[surveys$name == "Harm Reduction Scales - Study 2 - Confirmation"],
                    force_request = TRUE,
                    include_display_order = FALSE,
                    include_embedded = c("PROLIFIC_PID", "Q_RecaptchaScore"),
                    label = TRUE, convert = FALSE)

df <- resp %>% 
  select(ResponseId, PROLIFIC_PID, 
         Q_RecaptchaScore, `Duration (in seconds)`, 
         q1:q45, age:sud_hx)

# Fix coding errors

df <- df %>% 
  mutate(across(c(q1:q12,q14:q45), 
                ~ case_when(
                . == "Strongly disagree" ~ 1,
                . == "Somewhat disagree" ~ 2,
                . == "Neither agree nor disagree" ~ 3,
                . == "Somewhat agree" ~ 4,
                . == "Strongly agree" ~ 5)),
         attn = case_when(
           attn == "Somewhat disagree" ~ 1,
           attn != "Somewhat disagree" ~ 0
         ),
         q13 = case_when(
           q13 == "Strongly Disagree" ~ 1,
           q13 == "Disagree" ~ 2,
           q13 == "Somewhat Disagree" ~ 3,
           q13 == "Somewhat Agree" ~ 4, 
           q13 == "Agree" ~ 5
         )
         )




# Write File
write.csv(df, "data/raw/20231011_hr-scale-exploratory-data.csv")


