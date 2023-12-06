##########################
# Study 1 Visualizations #
##########################

# Libraries
library(psych)
library(tidyverse)

# Import data
df <- read.csv("data/clean/20231011_hr-scale-exploratory-data.csv")

# Build Scales and Check Alphas ----
scale1 <- df %>% 
  select(q1, q6, q7, q16, q18, q19, q21, q39, q13)
scale2 <- df %>% 
  select(q36, q38, q42) 
scale3 <- df %>% 
  select(q9, q2, q23, q29, q10, q32) 

# Connect with Items
library(qualtRics)

# Get all survey IDs
surveys <- all_surveys()

# Create Codebook ----
## Download Items
item <- survey_questions(surveys$id[surveys$name == "Harm Reduction Scales - Study 1"]) 

item <- item %>% 
  mutate(scale = case_when(
      qname %in% colnames(scale1) ~ "Scale 1",
      qname %in% colnames(scale2) ~ "Scale 2",
      qname %in% colnames(scale3) ~ "Scale 3"
    ))

# Estimate alphas
alpha1 <- psych::alpha(scale1, check.keys = TRUE)
alpha2 <- psych::alpha(scale2, check.keys = TRUE)
alpha3 <- psych::alpha(scale3, check.keys = TRUE)

# Visualize Responses
## Scale 1
items.1 <- data.frame(alpha1$response.freq) %>% 
  mutate(item = rownames(.)) %>% 
  left_join(item %>% 
              select(qname, question), 
            by = c("item" = "qname")) %>% 
  mutate(question = paste(item, question, sep = " - ")) %>% 
  pivot_longer(c(X1:X6)) %>% 
  mutate(Response = 
           case_when(
             name == "X1" ~ "Strongly Disagree",
             name == "X2" ~ "Disagree",
             name == "X3" ~ "Somewhat Disagree",
             name == "X4" ~ "Somewhat Agree",
             name == "X5" ~ "Agree",
             name == "X6" ~ "Strongly Agree"
           )) %>% 
  ggplot() +
  aes(x = question, y = value, fill = Response) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(value*100,0),"%")), 
            position = position_stack(vjust = .5),
            size = 3) +
  coord_flip() +
  ggforce::facet_col(facets = vars(question), 
                     scales = "free_y", 
                     space = "free",
                     labeller = label_wrap_gen(width=80)
                     ) +
  labs(title = "Response Distribution for Scale 1 (Set 1)") + 
  theme(
    strip.background = element_blank(),
    #legend.position = "",
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    panel.background = element_blank()
  )

## Scale 2
items.2 <- data.frame(alpha2$response.freq) %>% 
  mutate(item = rownames(.)) %>% 
  left_join(item %>% 
              select(qname, question), 
            by = c("item" = "qname")) %>%
  mutate(question = paste(item, question, sep = " - ")) %>% 
  pivot_longer(c(X1:X6)) %>% 
  mutate(Response = 
           case_when(
             name == "X1" ~ "Strongly Disagree",
             name == "X2" ~ "Disagree",
             name == "X3" ~ "Somewhat Disagree",
             name == "X4" ~ "Somewhat Agree",
             name == "X5" ~ "Agree",
             name == "X6" ~ "Strongly Agree"
           )) %>% 
  ggplot() +
  aes(x = question, y = value, fill = Response) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(value*100,0),"%")), 
            position = position_stack(vjust = .5),
            size = 3) +
  coord_flip() +
  ggforce::facet_col(facets = vars(question), 
                     scales = "free_y", 
                     space = "free",
                     labeller = label_wrap_gen(width=80)
  ) +
  labs(title = "Response Distribution for Scale 2") + 
  theme(
    strip.background = element_blank(),
    #legend.position = "",
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.background = element_blank()
  )

## Scale 3
items.3 <- data.frame(alpha3$response.freq) %>% 
  mutate(item = rownames(.)) %>% 
  left_join(item %>% 
              select(qname, question), 
            by = c("item" = "qname")) %>%
  mutate(question = paste(item, question, sep = " - ")) %>% 
  pivot_longer(c(X1:X6)) %>% 
  mutate(Response = 
           case_when(
             name == "X1" ~ "Strongly Disagree",
             name == "X2" ~ "Disagree",
             name == "X3" ~ "Somewhat Disagree",
             name == "X4" ~ "Somewhat Agree",
             name == "X5" ~ "Agree",
             name == "X6" ~ "Strongly Agree"
           )) %>% 
  ggplot() +
  aes(x = question, y = value, fill = Response) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(value*100,0),"%")), 
            position = position_stack(vjust = .5),
            size = 3) +
  coord_flip() +
  ggforce::facet_col(facets = vars(question), 
                     scales = "free_y", 
                     space = "free",
                     labeller = label_wrap_gen(width=80)
  ) +
  labs(title = "Response Distribution for Scale 3") + 
  theme(
    strip.background = element_blank(),
    #legend.position = "",
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    panel.background = element_blank()
  )



meanscores <- df %>%
  mutate(across(c(q39, q41, q42), 
                ~ 7 - .),
         fact1 = 
           rowMeans(df %>% select(all_of(colnames(scale1))), na.rm = TRUE),
         fact2 = 
           rowMeans(df %>% select(all_of(colnames(scale2))), na.rm = TRUE),
         fact3 = 
           rowMeans(df %>% select(all_of(colnames(scale3))), na.rm = TRUE))

cor(meanscores[63:65], use = "na.or.complete")

meanscores %>% 
  pivot_longer(fact1:fact3) %>% 
  mutate(sud_hx = 
           case_when(
             sud_hx == "I don’t know" ~ "No",
             sud_hx == "Prefer not to say" ~ "No",
             TRUE ~ sud_hx
           ),
         gid1 = 
           case_when(
             gid1 == "Non-Binary or Gender Fluid" ~ "Other",
             gid1 == "Prefer not to say" ~ "Other",
             is.na(gid1) ~ "Other",
             TRUE ~ gid1
           )) %>% 
  group_by(
    name, 
    #sud_hx, 
    #gid1
    ) %>% 
  mutate(avg = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>%
  ggplot() + aes(x = name, y = value, color = name) +
  geom_violin() +
  geom_jitter(alpha = .4, height = 0.1) +
  geom_hline(aes(yintercept = avg, col = name)) 
  #facet_grid(~ sud_hx, scales = "free")

mod <- fa(df %>% select(c(
  # Factor 1
  q1, q6, q7, q14, q16, q18, q19, q21, q39, q13,
  # Factor 2
  q33, q36, q38, q42, #q35, q41, 
  # Factor 3
  q9, q2, q20, q23, q29, q12, q10, q28, q32,
  
  # Try out some final items
  
  
  # Skip:
  # q4:q5, q25, q40,
  # q31, q8, q24, q45, 
  # q22, q32
  # Tried: 2, 3, 11, 15, 17, 26, 27, 29, q31, 32, 34, 35, 39, 40, 43
  
  # Worth considering: 
  #  - ADD Q35!!!
  #  - item 15 instead of 13
  #  - item 27 instead of 28; 29 instead of 28; or 27 & 29 instead of 28
  #  - Factor 2: q9, q2, q23, q29, q12, q10, q29, q31 - slightly better alpha
  #  - Factor 2: q9, q2, q20, q23, q29, q12, q10, q28, q32
  #  - Factor 2: q9, q2, q20, q23, q29, q12, q10, q28, q32 -- Much better alpha
  #  - Consider removing q41
  #  - q
  
  
)),
nfactors = 3, rotate = "oblimin", cor = "poly")


# Reduced Model for Momparison
reduced.mod <- fa(df %>% select(c(
  # Factor 1            
  q1, q6, q7, q16, q18, q19, q21, q39, q13, # 39 reverse coded
  # Factor 2
  q36, q38, q42, # 42 reverse coded
  # Factor 3
  q9, q2, q23, q29, q10, q32)),
  nfactors = 3, rotate = "oblimin", cor = "poly")
print.psych(reduced.mod, cut = .3, sort = TRUE)






