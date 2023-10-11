#####################################
#   Study 1 Cleaning & Inspecting   #
#####################################

# Libraries
library(tidyverse)

# Import Data
df <- data.table::fread("data/raw/20231011_hr-scale-exploratory-data.csv")

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

sum(is.na(df$Q_RecaptchaScore))

# Attention Check
# attn should equal 3
df %>% 
  pivot_longer(attn) %>% 
  summarize(median = median(value,na.rm = TRUE),
            IQR = IQR(value,na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE)) 

table(df$attn)

df %>% 
  mutate(attn_correct = 
           ifelse(attn == 3, 1, 0)) %>% 
  pivot_longer(c(`Duration (in seconds)`, Q_RecaptchaScore)) %>% 
  group_by(attn_correct, name) %>% 
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

sum(is.na(df$age)) # 6 missing

hist(df$age)

## Gender Identity
table(df$gid1,df$gid2)

table(df$gid1_4_TEXT)

df <- df %>% 
  mutate(gid1 = 
           case_when(gid1_4_TEXT == "Fluid" | gid1 == "Non-binary" ~ "Non-Binary or Gender Fluid",
                     TRUE ~ gid1)) %>% 
  select(-gid1_4_TEXT)

table(df$gid1,df$gid2)

## Race/ethnicity
df %>% 
  mutate(
    multiracial =
      rowSums(df %>% 
                select(`race-eth-id_1`, `race-eth-id_3`:`race-eth-id_8`) %>% 
                mutate(across(c(`race-eth-id_1`, `race-eth-id_3`:`race-eth-id_8`),
                              ~ as.numeric(case_when(
                                !is.na(.) ~ 1,
                                is.na(.) ~ NA
                                )))), 
              na.rm = TRUE),
    .after = "race-eth-choice") %>% 
  filter(multiracial > 1)
  

df <- df %>% 
  mutate(
    multiracial =
      rowSums(df %>% 
                select(`race-eth-id_1`, `race-eth-id_3`:`race-eth-id_8`) %>% 
                mutate(across(c(`race-eth-id_1`, `race-eth-id_3`:`race-eth-id_8`),
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
          `race-eth-id_3` == "Black or African American" ~ "Black or African American",
        multiracial == 1 & 
          `race-eth-id_4` == "Latine or Hispanic" ~ "Latine or Hispanic",
        multiracial == 1 & 
          `race-eth-id_5` == "Indigenous, Aboriginal, or First Nations" ~ "Indigenous, Aboriginal, or First Nations",
        multiracial == 1 & 
          `race-eth-id_6` == "White" ~ "White",
        multiracial == 1 & 
          `race-eth-id_7` == "Prefer to self-describe" ~ "Prefer to self-describe",
        multiracial == 1 & 
          `race-eth-id_8` == "Prefer not to say" ~ "Prefer not to say",
        multiracial > 1 &
          (`race-eth-choice` == "Bi-racial" |
          grepl("mixed race", `race-eth-choice`, ignore.case = TRUE) |
          grepl("multi-racial", `race-eth-choice`, ignore.case = TRUE) |
          grepl("multiracial", `race-eth-choice`, ignore.case = TRUE)) ~ "Multiracial",
        grepl("white", `race-eth-choice`, ignore.case = TRUE) ~ "White",
        grepl("black", `race-eth-choice`, ignore.case = TRUE) ~ "Black or African American",
        grepl("Native American", `race-eth-choice`, ignore.case = TRUE) ~ "Indigenous, Aboriginal, or First Nations",
    multiracial > 1 &
      is.na(`race-eth-id_4`) ~ "Multiracial",
    !is.na(`race-eth-id_4`) &
      !is.na(`race-eth-id_6`) &
      multiracial == 2 ~ "White"),
    hispanic = case_when(
      `race-eth-id_4` == 1 ~ "Yes",
      is.na(`race-eth-id_4`) ~ "No"
    ),
    .keep = "unused",
    .after = "gid2"
  ) %>% 
  mutate(
    hispanic = 
      case_when(
        race == "Latine or Hispanic" ~ "Yes",
        TRUE ~ hispanic
      ) 
  ) %>% 
  select(-`race-eth-id_2`)



# Write to file
write.csv(df, "data/clean/20231011_hr-scale-exploratory-data.csv")




