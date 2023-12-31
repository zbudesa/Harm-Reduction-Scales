#####################################
#   Study 1 Cleaning & Inspecting   #
#####################################

# Libraries
library(tidyverse)

# Import Data
df <- data.table::fread("data/raw/03_study3_data.csv")

# Inspect time & ReCaptcha Data
df %>% 
  pivot_longer(c(`Duration (in seconds)`, Q_RecaptchaScore)) %>% 
  group_by(name) %>% 
  summarize(mean = mean(value,na.rm = TRUE),
            sd = sd(value,na.rm = TRUE),
            median = median(value,na.rm = TRUE),
            IQR = IQR(value,na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE))

# Attention Check
# attn should equal 1
df %>% 
  pivot_longer(attn) %>% 
  summarize(median = median(value,na.rm = TRUE),
            IQR = IQR(value,na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE)) 

table(df$attn)

df %>% 
  pivot_longer(c(`Duration (in seconds)`, Q_RecaptchaScore)) %>% 
  group_by(attn, name) %>% 
  summarize(mean = mean(value,na.rm = TRUE),
            sd = sd(value,na.rm = TRUE),
            median = median(value,na.rm = TRUE),
            IQR = IQR(value,na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE))
  

# Based on this, I'll probably conduct analyses with and without concerning responses
# in order to identify whether these are affecting overall response quality

# Clean demographic fields
## Age
df %>% 
  summarize(mean = mean(age,na.rm = TRUE),
            sd = sd(age,na.rm = TRUE),
            median = median(age,na.rm = TRUE),
            IQR = IQR(age,na.rm = TRUE),
            min = min(age, na.rm = TRUE),
            max = max(age, na.rm = TRUE))

sum(is.na(df$age)) # 7 missing

hist(df$age)

## Gender Identity
table(df$gid1,df$gid2)

table(df$gid1_4_TEXT)

df <- df %>% 
  mutate(gid1 = 
           case_when(gid1_4_TEXT == "AGENDER" | 
                       gid1 == "Non-binary" | 
                       gid1 == "Prefer not to say" ~ "Non-Binary, Agender, or Other",
                     TRUE ~ gid1)) %>% 
  select(-gid1_4_TEXT)

table(df$gid1,df$gid2)

## Race/ethnicity
df %>% 
  mutate(
    multiracial =
      rowSums(df %>% 
                select(`race-eth-id_1`:`race-eth-id_8`) %>% 
                mutate(across(c(`race-eth-id_1`:`race-eth-id_8`),
                              ~ as.numeric(case_when(
                                !is.na(.) ~ 1,
                                is.na(.) ~ NA
                                )))), 
              na.rm = TRUE),
    .after = "race-eth-choice") %>% 
  filter(multiracial > 1)
  

df <- 
  df %>% 
  mutate(
    multiracial =
      rowSums(df %>% 
                select(`race-eth-id_1`, `race-eth-id_3`:`race-eth-id_8`, `race-eth-id_2`) %>% 
                mutate(across(c(`race-eth-id_1`, `race-eth-id_3`:`race-eth-id_8`, `race-eth-id_2`),
                              ~ as.numeric(case_when(
                                !is.na(.) ~ 1,
                                is.na(.) ~ NA
                              )))), 
              na.rm = TRUE),
    .after = "race-eth-choice") %>% 
  mutate(
    race = 
      case_when(
        multiracial == 1 &
          `race-eth-id_1` == "Asian or Asian American" ~ "Asian or Asian American",
        multiracial == 1 & 
          `race-eth-id_2` == "Arab, Middle Eastern, or North African" ~ "Other or Prefer not to say",
        multiracial == 1 & 
          `race-eth-id_3` == "Black or African American" ~ "Black or African American",
        multiracial == 1 & 
          `race-eth-id_4` == "Latine or Hispanic" ~ "Latine or Hispanic",
        multiracial == 1 & 
          `race-eth-id_5` == "Indigenous, Aboriginal, or First Nations" ~ "Indigenous, Aboriginal, or First Nations",
        multiracial == 1 & 
          `race-eth-id_6` == "White" ~ "White",
        multiracial == 1 & 
          `race-eth-id_8` == "Prefer not to say" ~ "Multiracial",
        grepl("white", `race-eth-choice`, ignore.case = TRUE) | 
          `race-eth-id_8` == "Prefer not to say" ~ "White",
        grepl("black", `race-eth-choice`, ignore.case = TRUE) ~ "Black or African American",
        grepl("Native American", `race-eth-choice`, ignore.case = TRUE) ~ "Indigenous, Aboriginal, or First Nations",
        grepl("asian", `race-eth-choice`, ignore.case = TRUE) ~ "Asian or Asian American",
    multiracial > 1 &
      is.na(`race-eth-id_4`) ~ "Multiracial",
    !is.na(`race-eth-id_4`) &
      !is.na(`race-eth-id_6`) &
      multiracial == 2 ~ "White",
    `race-eth-choice` == "Biracial" | 
      grepl("bi-racial", `race-eth-choice`, ignore.case = TRUE) |
      grepl("mixed", `race-eth-choice`, ignore.case = TRUE) |
      grepl("multi", `race-eth-choice`, ignore.case = TRUE) |
      grepl("multiracial", `race-eth-choice`, ignore.case = TRUE) |
      grepl("other", `race-eth-choice`) ~ "Multiracial"
    ),
    hispanic = case_when(
      `race-eth-id_4` == 1 ~ "Yes",
      is.na(`race-eth-id_4`) ~ "No"
    ),
    hispanic = 
      case_when(
        race == "Latine or Hispanic" ~ "Yes",
        TRUE ~ hispanic
      ),
    .keep = "unused",
    .after = "gid2"
  ) %>% 
  select(-`race-eth-id_7`)

# Write to file
write.csv(df, "data/clean/03_study3_data_clean.csv")




