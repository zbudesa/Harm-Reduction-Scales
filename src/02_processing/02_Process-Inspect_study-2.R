#####################################
#   Study 1 Cleaning & Inspecting   #
#####################################

# Libraries
library(tidyverse)

# Import Data
df <- data.table::fread("data/raw/02_study2_data.csv")

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

df %>% 
  pivot_longer(c(q1:q12, q13:q45)) %>% 
  group_by(name) %>% 
  summarize(mean = mean(value,na.rm = TRUE),
            sd = sd(value,na.rm = TRUE),
            median = median(value,na.rm = TRUE),
            IQR = IQR(value,na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE)) %>% View()
  

# Clean up responses
df <- df %>% 
  mutate(across(c(q5, q10, q11, q12, q14:q45),
                ~ . - 6))

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

df <- df %>% 
  mutate(gid1 = 
           case_when(gid1 == "Prefer not to say" | gid1 == "Non-binary" |
                       gid1 == "Prefer to self-describe" ~ "Non-Binary, Agender, or Other",
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
          `race-eth-id_5` == "Indigenous, Aboriginal, or First Nations" ~ "Other or Prefer not to say",
        multiracial == 1 & 
          `race-eth-id_6` == "White" ~ "White",
        multiracial == 1 & 
          `race-eth-id_7` == "Prefer to self-describe" &
          `race-eth-choice` == "Mixed Race" ~ "Multiracial",
        multiracial == 1 & 
          `race-eth-id_8` == "Prefer not to say" ~ "Other or Prefer not to say",
        `race-eth-choice` == "white" & 
          `race-eth-id_8` == "Prefer not to say" ~ "White",
        multiracial > 1 &
          (`race-eth-choice` == "Biracial" |
          grepl("mixed", `race-eth-choice`, ignore.case = TRUE) |
          grepl("multi", `race-eth-choice`, ignore.case = TRUE) |
          grepl("multiracial", `race-eth-choice`, ignore.case = TRUE) |
            grepl("other", `race-eth-choice`)) ~ "Multiracial",
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
    hispanic = 
      case_when(
        race == "Latine or Hispanic" ~ "Yes",
        TRUE ~ hispanic
      ),
    .keep = "unused",
    .after = "gid2"
  )


# Write to file
write.csv(df, "data/clean/02_study2_data_clean.csv")
# 



