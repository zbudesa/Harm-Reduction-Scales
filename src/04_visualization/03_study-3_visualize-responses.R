##########################
# Study 1 Visualizations #
##########################

# Libraries
library(psych)
library(tidyverse)

# Import data
df <- read.csv("data/clean/20231025_hr-scale-confirmatory-data2.csv")

# Build Scales and Check Alphas ----
scale1 <- df %>% 
  select(q1, q6, q7, q16, q18, q19, q21, q13) # Drop item 39 due to loading and low alpha
scale2 <- df %>% 
  select(q42, q36, q33, q35, q34, q38, q41) 

# Connect with Items
library(qualtRics)

# Get all survey IDs
surveys <- all_surveys()

# Create Codebook ----
## Download Items
# item <- survey_questions(surveys$id[surveys$name == "Harm Reduction Scales - Study 2 - Confirmation"]) 
# 
# item <- item %>% 
#   mutate(scale = case_when(
#       qname %in% colnames(scale1) ~ "Scale 1",
#       qname %in% colnames(scale2) ~ "Scale 2"
#     ))
# 
# write.csv(item, "references/docs/items.csv")

item <- read.csv("references/docs/items.csv")

# Estimate alphas
alpha1 <- psych::alpha(scale1, check.keys = TRUE)
alpha2 <- psych::alpha(scale2, check.keys = TRUE)

# Visualize Responses
## Scale 1
(items.1 <- data.frame(alpha1$response.freq) %>% 
  mutate(item = rownames(.)) %>% 
  left_join(item %>% 
              select(qname, question), 
            by = c("item" = "qname")) %>% 
  #mutate(question = paste(item, question, sep = " - ")) %>% 
  pivot_longer(c(X1:X5)) %>% 
  mutate(Response = 
           factor(case_when(
             name == "X1" ~ "Strongly disagree",
             name == "X2" ~ "Somewhat disagree",
             name == "X3" ~ "Neither agree nor disagree",
             name == "X4" ~ "Somewhat agree",
             name == "X5" ~ "Strongly agree"
           ),
             levels = c("Strongly agree",
                        "Somewhat agree",
                        "Neither agree nor disagree",
                        "Somewhat disagree",
                        "Strongly disagree"))) %>% 
  ggplot() +
  aes(x = question, y = value, fill = Response) +
  geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = paste0(round(value*100,0),"%")), 
              position = position_stack(vjust = .5),
              size = 8, color = "white") +
    ggokabeito::scale_fill_okabe_ito(
      breaks = c("Strongly disagree","Somewhat disagree",
                 "Neither agree nor disagree","Somewhat agree",
                 "Strongly agree"
      )) +
    coord_flip() +
    ggforce::facet_col(facets = vars(question), 
                       scales = "free_y", 
                       space = "free",
                       #labeller = label_wrap_gen(width = 150)
    ) +
    labs(title = "Harm Reduction Strategies") +
    theme(
      axis.title = element_blank(),
      strip.background = element_blank(),
      legend.position = "top",
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.background = element_blank(),
      strip.text = element_text(size = 18, 
                                hjust = 0,
                                vjust = -1,
                                margin = 
                                  margin(.6, 0, -.3, 2.1, "cm")),
      panel.spacing = unit(-1.5, "lines"),
      legend.title = element_blank(),
      strip.clip = "off",
      plot.title = element_text(hjust = 0.5, size = 30),
      legend.text = element_text(size = 19, vjust = .2)
    ))

 ggsave("figs/scale1.png", items.1, width = 1500, height = 1500, units = "px")


## Scale 2
(items.2 <- data.frame(alpha2$response.freq) %>% 
  mutate(item = rownames(.)) %>% 
  left_join(item %>% 
              select(qname, question), 
            by = c("item" = "qname")) %>%
  pivot_longer(c(X1:X5)) %>% 
  mutate(Response = 
  factor(case_when(
    name == "X1" ~ "Strongly disagree",
  name == "X2" ~ "Somewhat disagree",
  name == "X3" ~ "Neither agree nor disagree",
  name == "X4" ~ "Somewhat agree",
  name == "X5" ~ "Strongly agree"),
  levels = c("Strongly agree",
             "Somewhat agree",
             "Neither agree nor disagree",
             "Somewhat disagree",
             "Strongly disagree"
             ))) %>% 
  ggplot() +
  aes(x = question, y = value, fill = Response) +
  geom_bar(stat = "identity", position = "stack", width = .6) +
  geom_text(aes(label = paste0(round(value*100,0),"%")), 
            position = position_stack(vjust = .5),
            size = 8, color = "white") +
    ggokabeito::scale_fill_okabe_ito(
      breaks = c("Strongly disagree","Somewhat disagree",
                                                "Neither agree nor disagree","Somewhat agree",
                                                "Strongly agree"
    )) +
    coord_flip() +
    ggforce::facet_col(facets = vars(question), 
                       scales = "free_y", 
                       space = "free",
                       labeller = label_wrap_gen(width = 200)
    ) +
    labs(title = "Harm Reduction Principles") +
    theme(
      axis.title = element_blank(),
      strip.background = element_blank(),
      legend.position = "top",
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.background = element_blank(),
      strip.text = element_text(size = 18, 
                                hjust = 0,
                                vjust = -1,
                                margin = 
                                  margin(0, 0, -.3, 2.1, "cm")),
      panel.spacing = unit(-1.5, "lines"),
      legend.title = element_blank(),
      strip.clip = "off",
      plot.title = element_text(hjust = 0.5, size = 30),
      legend.text = element_text(size = 19, vjust = .2)
    ) )

ggsave("figs/scale2.png", items.2)


meanscores <- df %>%
  mutate(across(c(q39, q42, q35, q34, q41), 
                ~ 6 - .),
         Strategies = 
           rowMeans(df %>% select(all_of(colnames(scale1))), na.rm = TRUE),
         Principles = 
           rowMeans(df %>% select(all_of(colnames(scale2))), na.rm = TRUE))

cor(meanscores[60:61])

(scores <- meanscores %>% 
  pivot_longer(Strategies:Principles) %>% 
  mutate(sud_hx = 
           case_when(
             sud_hx == "I don’t know" ~ "No",
             sud_hx == "Prefer not to say" ~ "No",
             TRUE ~ sud_hx
           )) %>% 
  group_by(
    name, 
    #race
    #sud_hx, 
    #gid1
    ) %>% 
  mutate(avg = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>%
  ggplot() + aes(x = name, y = value, fill = name) +
  geom_violin() +
  geom_jitter(alpha = .3, height = 0.1) +
  geom_hline(aes(yintercept = avg, col = name),
             size = 2, alpha = .5) +
    labs(title = "Harm Reduction Subscale Scores")+
    ggokabeito::scale_fill_okabe_ito() +
    ggokabeito::scale_color_okabe_ito() +
  cowplot::theme_half_open() +
    theme(
      axis.title = element_blank(),
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.line.y = element_line(),
      axis.line.y.right = element_line(),
      panel.background = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 25),
      strip.clip = "off"
    )  +
  facet_wrap(~ name, scales = "free_x",
             strip.position = "bottom"))


ggsave("figs/scores.png", scores)


 Demos
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
  select(-age, -ed) %>% 
  pivot_longer(everything()) %>% 
  group_by(name, value) %>% 
  summarise(count = n()) %>%
  ungroup() %>%
  
  ggplot() + 
  aes(x = name, y = count, fill = value) +
  geom_bar(stat = "identity") +
  facet_wrap(name ~ ., scales = "free",
             labeller = labeller(c(gid1 = "Gender Identity"))) 













