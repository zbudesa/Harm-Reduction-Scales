############################
#   Study 3 Descriptives   #
############################

# Libraries
library(psych)
library(gtsummary)
library(gt)
library(gtExtras)
library(tidyverse)

# Import data
df <- read.csv("data/clean/20231025_hr-scale-confirmatory-data2.csv")

demos <- df %>% 
  select(age,gid1,race,ed,sud_hx) %>% 
  mutate(gid1 = 
           factor(gid1,
                  levels = 
                    c("Man", "Woman", "Non-Binary, Agender, or Other")),
         race = 
           factor(race,
                  levels = c("Asian or Asian American",
                  "Black or African American",
                  "Latine or Hispanic", "White",
                  "Multiracial")),
         ed = 
           case_when(
               ed == "Doctorate Degree (PhD, PsyD, etc.)" |
                 ed == "Professional Degree (J.D., M.D., etc.)" ~
                 "Doctorate or Professional Degree",
               ed == "High School" |
                 ed == "Less than High School" ~ "High School or Less",
               ed == "Other option not represented here" |
                 is.na(ed) ~ "Other",
               TRUE ~ ed),
         ed = factor(ed,
                     levels = 
                       c("High School or Less", "Associate's Degree",
                         "Bachelor's Degree", "Master's Degree",
                         "Doctorate or Professional Degree",
                         "Other")),
         sud_hx = 
           case_when(
             sud_hx == "I don’t know" ~ "No",
             sud_hx == "Prefer not to say" ~ "No",
             TRUE ~ sud_hx
           )
           )


demos %>% 
  gt_plt_summary(title = "test")


demo_tbl <- demos %>% 
  select(-ed) %>% 
  tbl_summary(label = list(age ~ "Age", gid1 ~ "Gender Identity",
                           race ~ "Race/Ethnicity", 
                           sud_hx ~ "History of SUD Diagnosis"),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(age ~ c(0,1)), 
              missing = "no") %>% 
  as_gt() %>% 
  gt::tab_header(title = "Sample Demographics")


gtsave(demo_tbl, "figs/demos.png")



  
demos %>% 
  pivot_longer(-age) %>% 
  ggplot() + aes(x = value) +
  geom_bar() +
  facet_wrap(~ name, scales = "free") +
  coord_flip()
  
