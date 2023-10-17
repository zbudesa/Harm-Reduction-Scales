##########################
# Study 1 Visualizations #
##########################

# Libraries
library(psych)
library(tidyverse)

# Import data
df <- read.csv("data/clean/20231011_hr-scale-exploratory-data.csv")

# Build Scales and Check Alphas ----
scale1 <- hr.ld %>% 
  select(q1, q3, q6, q7, q13, q14, q16, q18, q19, q21,q39)
scale2 <- hr.ld %>% 
  select(q33, q36, q38, q41, q42)
scale3 <- hr.ld %>% 
  select(q2, q9, q10, q23, q27, q29, q30)

# Connect with Items
library(qualtRics)

# Get all survey IDs
surveys <- all_surveys()

# Create Codebook ----
## Download Items
item <- survey_questions(surveys$id[surveys$name == "Harm Reduction Scales - Study 1"]) 


# Estimate alphas
alpha1 <- psych::alpha(scale1, check.keys = TRUE)
alpha2 <- psych::alpha(scale2, check.keys = TRUE)
alpha3 <- psych::alpha(scale3, check.keys = TRUE)

# Visualize Responses
## Scale 1
items.1a <- data.frame(alpha1$response.freq) %>% 
  mutate(item = rownames(.)) %>% 
  left_join(item %>% 
              select(qname, question), 
            by = c("item" = "qname")) %>%
  filter(item %in% c("q1", "q3", "q6", "q7", "q13", "q14")) %>% 
  pivot_longer(c(X1:X6)) %>% 
  ggplot() +
  aes(x = question, y = value, fill = factor(name)) +
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

items.1b <- data.frame(alpha1$response.freq) %>% 
  mutate(item = rownames(.)) %>% 
  left_join(item %>% 
              select(qname, question), 
            by = c("item" = "qname")) %>%
  filter(!(item %in% c("q1", "q3", "q6", "q7", "q13", "q14"))) %>% 
  pivot_longer(c(X1:X6)) %>% 
  ggplot() +
  aes(x = question, y = value, fill = factor(name)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(value*100,0),"%")), 
            position = position_stack(vjust = .5),
            size = 3) +
  coord_flip() +
  ggforce::facet_col(facets = vars(question), 
                     scales = "free_y", 
                     space = "free",
                     labeller = label_wrap_gen(width=75)
  ) +
  labs(title = "Response Distribution for Scale 1 (Set 2)") + 
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
  pivot_longer(c(X1:X6)) %>% 
  ggplot() +
  aes(x = question, y = value, fill = factor(name)) +
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

## Scale 3
items.3 <- data.frame(alpha3$response.freq) %>% 
  mutate(item = rownames(.)) %>% 
  left_join(item %>% 
              select(qname, question), 
            by = c("item" = "qname")) %>%
  pivot_longer(c(X1:X6)) %>% 
  ggplot() +
  aes(x = question, y = value, fill = factor(name)) +
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


