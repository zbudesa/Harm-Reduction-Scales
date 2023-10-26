##########################
# Study 1 Visualizations #
##########################

# Libraries
library(psych)
library(tidyverse)

# Import data
df <- read.csv("data/clean/20231020_hr-scale-confirmatory-data.csv")

# Build Scales and Check Alphas ----
scale1 <- df %>% 
  select(q1, q6, q7, q16, q18, q19, q21, q39, q13)
scale2 <- df %>% 
  select(q42, q36, q33, q35, q34, q38, q41) 

# Connect with Items
library(qualtRics)

# Get all survey IDs
surveys <- all_surveys()

# Create Codebook ----
## Download Items
item <- survey_questions(surveys$id[surveys$name == "Harm Reduction Scales - Study 2 - Confirmation"]) 

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
  pivot_longer(c(X1:X5)) %>% 
  mutate(Response = 
           factor(case_when(
             name == "X1" ~ "Strongly disagree",
             name == "X2" ~ "Somewhat disagree",
             name == "X3" ~ "Neither agree nor disagree",
             name == "X4" ~ "Somewhat agree",
             name == "X5" ~ "Strongly Agree"
           ),
             levels = c("Strongly disagree", 
                        "Somewhat disagree",
                        "Neither agree nor disagree",
                        "Somewhat agree",
                        "Strongly Agree"))) %>% 
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
  pivot_longer(c(X1:X5)) %>% 
  mutate(Response = 
  factor(case_when(
    name == "X1" ~ "Strongly disagree",
  name == "X2" ~ "Somewhat disagree",
  name == "X3" ~ "Neither agree nor disagree",
  name == "X4" ~ "Somewhat agree",
  name == "X5" ~ "Strongly Agree"),
  levels = c("Strongly disagree", 
             "Somewhat disagree",
             "Neither agree nor disagree",
             "Somewhat agree",
             "Strongly Agree"))) %>% 
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


meanscores <- df %>%
  mutate(across(c(q33, q36, q39, q38), 
                ~ 6 - .),
         fact1 = 
           rowMeans(df %>% select(all_of(colnames(scale1))), na.rm = TRUE),
         fact2 = 
           rowMeans(df %>% select(all_of(colnames(scale2))), na.rm = TRUE))

cor(meanscores[60:61])

meanscores %>% 
  pivot_longer(fact1:fact2) %>% 
  mutate(sud_hx = 
           case_when(
             sud_hx == "I don’t know" ~ "No",
             sud_hx == "Prefer not to say" ~ "No",
             TRUE ~ sud_hx
           )) %>% 
  group_by(
    name, 
    sud_hx, 
    #gid1
    ) %>% 
  mutate(avg = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>%
  ggplot() + aes(x = name, y = value, color = name) +
  geom_violin() +
  geom_jitter(alpha = .4, height = 0.1) +
  geom_hline(aes(yintercept = avg, col = name)) +
  facet_grid(~ sud_hx, scales = "free")








