####################################
#  Download RPE Responses - Stage 3#
####################################

# Packages
library(qualtRics)
library(tidyverse)

# Get all survey IDs
surveys <- all_surveys()

# Actual Items
item <- survey_questions(surveys$id[surveys$name == "Harm Reduction Scales - Response Process Evaluation - Stage 3"])

# Pul question text, remove html, add question numbers
items <- item %>% 
  filter(grepl("<h1 style=", question)) %>% 
  select(qname, question) %>% 
  mutate(item = str_remove_all(question, '<h1 style=\"text-align: center;\">'),
         question = str_remove_all(item, '</h1>'),
         number = str_extract(qname, "\\d+")) %>% 
  select(number, question) %>% 
  mutate(number = case_when(number == "320" ~ "2", TRUE ~ number))

# Download Responses & Clean
rpe <- fetch_survey(surveyID = 
                      surveys$id[surveys$name == "Harm Reduction Scales - Response Process Evaluation - Stage 3"],
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
  left_join(data.frame(#Group = rep(1:5, length.out = 4), 
                       item = as.character(c(1:4)))) %>% 
  # Select & rename columns of interest
  select(`Participant ID` = id, 
         `Question Number` = item,
         `Question Text` = question,
         Response, `Explanation for Response`,
         `Item Paraphrase`, `Comprehension`,
         `Comments`)

# Set sheet names for excel doc
sheet_names <- list(`Item 1` = question[question$`Question Number` == 1,],
                    `Item 2` = question[question$`Question Number` == 2,],
                    `Item 3` = question[question$`Question Number` == 11,],
                    `Item 4` = question[question$`Question Number` == 12,])

# Write file
openxlsx::write.xlsx(sheet_names, file = "data/clean/003_RPE-Data_stage-3.xlsx") 


