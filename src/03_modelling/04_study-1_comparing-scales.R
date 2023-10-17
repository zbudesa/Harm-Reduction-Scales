########################################
# Study 1 Comparing Alternative Models #
########################################

source("src/03_modelling/02_study-1_IRT-models.R")
source("src/03_modelling/03_study-1_alternative-models.R")


item <- item %>% 
  mutate(
    scale = case_when(
      var_label %in% fascale1 ~ "Scale 1",
      var_label %in% fascale2 ~ "Scale 2",
      var_label %in% fascale3 ~ "Scale 3"
    ),
    irtscale = case_when(
      var_label %in% scale1 ~ "Scale 2",
      var_label %in% scale2 ~ "Scale 3",
      var_label %in% scale3 ~ "Scale 1"
    ),
    altscale = case_when(
      var_label %in% altscale1 ~ "Scale 1",
      var_label %in% altscale2 ~ "Scale 2",
      var_label %in% altscale3 ~ "Scale 3"
    ))


# Scale 1 Alpha Comparison
psych::alpha(df %>% 
               select(all_of(fascale1)),
             check.keys = TRUE)
psych::alpha(df %>% 
               select(all_of(scale3)),
             check.keys = TRUE)
psych::alpha(df %>% 
               select(all_of(altscale1)),
             check.keys = TRUE)


# Scale 2 Alpha Comparison
psych::alpha(df %>% 
               select(all_of(fascale2)),
             check.keys = TRUE)
psych::alpha(df %>% 
               select(all_of(scale1)),
             check.keys = TRUE)
psych::alpha(df %>% 
               select(all_of(altscale2)),
             check.keys = TRUE)


# Scale 3 Alpha Comparison
psych::alpha(df %>% 
               select(all_of(fascale3)),
             check.keys = TRUE)
psych::alpha(df %>% 
               select(all_of(scale2)),
             check.keys = TRUE)
psych::alpha(df %>% 
               select(all_of(altscale3)),
             check.keys = TRUE)


