####################################
#  Download RPE Responses - Stage 2#
####################################

# Packages
library(qualtRics)
library(tidyverse)

# Get all survey IDs
surveys <- all_surveys()

# Actual Items
item <- survey_questions(surveys$id[surveys$name == "Harm Reduction Scales - Response Process Evaluation - Stage 2"])

# Pul question text, remove html, add question numbers
items <- item %>% 
  filter(grepl("<h1 style=", question)) %>% 
  select(question) %>% 
  mutate(item = str_remove_all(question, '<h1 style=\"text-align: center;\">'),
         question = str_remove_all(item, '</h1>'),
         number = as.character(c(1,3,7,10,11,12,20,"20a",23,25,26,32,38,40,42,43,44,45))) %>% 
  select(number, question)

# Download Responses & Clean
rpe <- fetch_survey(surveyID = 
                      surveys$id[surveys$name == "Harm Reduction Scales - Response Process Evaluation - Stage 2"],
                    force_request = TRUE)

## Add ID
rpe <- rpe %>% 
  mutate(id = row_number())


question <- rpe %>% 
  # Add ID number
  mutate(id = row_number()) %>% 
  # Select relevant columns
  select(id,
         starts_with(c("resp", "exp-resp", "Comp", "Para", "Comment"))) %>% 
  select(-ResponseId) %>% 
  # Pivot all relevant columns
  pivot_longer(-id) %>% 
  # Set item number from item column
  mutate(item = str_extract(name, "\\d+"),
         # Rename so that all columns are the name and do not have item numbers
         name = 
           case_when(
             grepl("exp-resp", name) ~ "Explanation for Response",
             grepl("resp", name) ~ "Response",
             grepl("Para", name) ~ "Item Paraphrase",
             grepl("Comment", name) ~ "Comments",
             grepl("Comp", name) ~ "Comprehension"
           )) %>%
  # Remove missing rows from items respondents didn't see
  filter(!is.na(value)) %>% 
  # Pivot wide to combine columns
  pivot_wider(id_cols = c(item, id), names_from = name, values_from = value) %>% 
  arrange(desc(item)) %>% 
  # Combine with question text
  left_join(items, by = c("item" = "number")) %>% 
  # Add group assignments
  left_join(data.frame(Group = rep(1:5, length.out = 18), 
                       item = as.character(c(1,3,7,10,11,12,20,"20a",23,25,26,32,38,40,42,43,44,45)))) %>% 
  # Select & rename columns of interest
  select(`Participant ID` = id, 
         `Question Number` = item,
         `Question Text` = question,
         Response, `Explanation for Response`,
         `Item Paraphrase`, `Comprehension`,
         `Comments`, Group)

# Set sheet names for excel doc
sheet_names <- list(`Group 1` = question[question$Group == 1,],
                    `Group 2` = question[question$Group == 2,],
                    `Group 3` = question[question$Group == 3,],
                    `Group 4` = question[question$Group == 4,],
                    `Group 5` = question[question$Group == 5,])

# Write file
openxlsx::write.xlsx(sheet_names, file = "data/clean/001_RPE-Data.xlsx") 


