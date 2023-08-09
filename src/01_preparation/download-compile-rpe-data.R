###########################
#  Download RPE Responses #
###########################

# Packages
library(qualtRics)
library(tidyverse)

# Get all survey IDs
surveys <- all_surveys()

# Actual Items
item <- survey_questions(surveys$id[surveys$name == "Harm Reduction Scales - Response Process Evaluation"])

items <- item %>% 
  filter(grepl("<h1 style=", question)) %>% 
  select(question) %>% 
  mutate(item = str_remove_all(question, '<h1 style=\"text-align: center;\">'),
         question = str_remove_all(item, '</h1>'),
         number = as.character(rep(1:46))) %>% 
  select(number, question)

# Download Responses & Clean
rpe <- fetch_survey(surveyID = 
                      surveys$id[surveys$name == "Harm Reduction Scales - Response Process Evaluation"],
                    force_request = TRUE)

## Add ID
rpe <- rpe %>% 
  mutate(id = row_number())


question <- rpe %>% 
  select(id,
         starts_with(c("resp", "exp-resp", "Comp", "Para", "Comment"))) %>% 
  select(-ResponseId) %>% 
  pivot_longer(-id) %>% 
  mutate(item = str_extract(name, "\\d+"),
         name = 
           case_when(
             grepl("exp-resp", name) ~ "Explanation for Response",
             grepl("resp", name) ~ "Response",
             grepl("Para", name) ~ "Item Paraphrase",
             grepl("Comment", name) ~ "Comments",
             grepl("Comp", name) ~ "Comprehension"
           )) %>%
  filter(!is.na(value)) %>% 
  pivot_wider(id_cols = c(item, id), names_from = name, values_from = value) %>% 
  arrange(desc(item)) %>% 
  left_join(items, by = c("item" = "number")) %>% 
  left_join(data.frame(Group = rep(1:5, length.out = 46), item = as.character(1:46))) %>% 
  select(`Participant ID` = id, 
         `Question Number` = item,
         `Question Text` = question,
         Response, `Explanation for Response`,
         `Item Paraphrase`, `Comprehension`,
         `Comments`, Group)
  


