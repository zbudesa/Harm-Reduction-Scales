#####################################
#        OSF Data Processing        #
#####################################

# Libraries
library(tidyverse)


# Study 1
df <- read.csv("data/clean/20231011_hr-scale-exploratory-data.csv")

df <- df %>% 
  select(id = X, q1:sud_hx)

write.csv(df, "data/osf/01_study1_data.csv")

# Study 2
df <- read.csv("data/clean/20231020_hr-scale-confirmatory-data.csv")

df <- df %>% 
  select(id = X, q1:sud_hx)

write.csv(df, "data/osf/02_study2_data.csv")

# Study 3
df <- read.csv("data/clean/20231025_hr-scale-confirmatory-data2.csv")

df <- df %>% 
  select(id = X, q1:sud_hx)

write.csv(df, "data/osf/03_study3_data.csv")
