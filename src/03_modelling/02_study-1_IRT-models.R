#####################################
#    Study 1 Checking IRT Models    #
#####################################

# Libraries
library(psych)
library(GPArotation)
library(EFA.dimensions)
library(tidyverse)

# Import data
df <- read.csv("data/clean/20231011_hr-scale-exploratory-data.csv")




# Current Model for Comparison ----
hr_new <- df %>% 
  select(starts_with("q") & -Q_RecaptchaScore) %>% 
      select(-c(
        # Items with Local Dependence (& Closely related Item)
        q4, q5, # q3
        q24, q25, # q23
        q40, #39
        q31, # q30
        q8,  # q7
        q45, # q12
        q22, # q27
        q32, # q29
        # Items Removed from Final Scales
        #q17, q12, q43, q15, q35, q37, q26, q34, q44, q20, q11, q28
        ))


hrMatrix <- 
  POLYCHORIC_R(hr_new)

hr <- df %>% 
  select(starts_with("q") & -Q_RecaptchaScore)

fa.current <- fa(hrMatrix, n.obs = 301, nfactors = 3, rotate = "oblimin", cor = "poly")
psych::print.psych(fa.current, sort = "TRUE", cut = .3)

fascale1 <- c("q1", "q7", "q18", "q19", "q21", "q6", "q13", "q3", "q16", "q14", "q39")
fascale2 <- c("q42", "q36", "q38", "q33", "q41")
fascale3 <- c("q9","q23", "q2", "q10", "q29","q27", "q30")

check_factorstructure(hr.ld %>% 
                        select(
                          -c(q17, q12, q43, q15, q35, q37, q26, q34, q44, q20, q11, q28)))

# IRT - Graded Response Model ----
library(mirt)

mod <- mirt(hr_new,
            model = 5, 
            itemtype = "graded",
            method = "QMCEM",
            technical =
              list(NCYCLES = 2000))

saveRDS(mod, file = "model/irt-grm.rds")

irt.fact <- summary(mod, rotate = "oblimin")
irt.fact <- data.frame(irt.fact$rotF)

fact1 <- rownames(irt.fact[abs(irt.fact$F1) >= .4,])
fact2 <- rownames(irt.fact[abs(irt.fact$F2) >= .4,])
fact3 <- rownames(irt.fact[abs(irt.fact$F3) >= .4,])
fact4 <- rownames(irt.fact[abs(irt.fact$F4) >= .4,])
fact5 <- rownames(irt.fact[abs(irt.fact$F5) >= .4,])

psych::alpha(hr_new %>% 
               select(all_of(fact2)),
             check.keys = TRUE)

psych::alpha(hr_new %>% 
               select(all_of(fact3)),
             check.keys = TRUE)

psych::alpha(hr_new %>% 
               select(all_of(fact1)),
             check.keys = TRUE)

psych::alpha(hr_new %>% 
               select(all_of(fact5)),
             check.keys = TRUE)

## IRT Model 2
mod <- mirt(hr_new,
            model = 4, 
            itemtype = "graded",
            method = "QMCEM",
            technical =
              list(NCYCLES = 2000))

saveRDS(mod, file = "model/irt-grm-2.rds")

irt.fact <- summary(mod, rotate = "oblimin")
irt.fact <- data.frame(irt.fact$rotF)

fact1 <- rownames(irt.fact[abs(irt.fact$F1) >= .3,])
fact2 <- rownames(irt.fact[abs(irt.fact$F2) >= .3,])
fact3 <- rownames(irt.fact[abs(irt.fact$F3) >= .3,])
fact4 <- rownames(irt.fact[abs(irt.fact$F4) >= .3,])

fact1 ; fact2 ; fact3 ; fact4

psych::alpha(hr_new %>% 
               select(all_of(fact1)),
             check.keys = TRUE)

psych::alpha(hr_new %>% 
               select(all_of(fact2)),
             check.keys = TRUE)

psych::alpha(hr_new %>% 
               select(all_of(fact3)),
             check.keys = TRUE)

psych::alpha(hr_new %>% 
               select(all_of(fact4)),
             check.keys = TRUE)


## IRT Model 2
mod <- mirt(hr_new,
            model = 3, 
            itemtype = "graded",
            method = "QMCEM",
            technical =
              list(NCYCLES = 2000))

saveRDS(mod, file = "model/irt-grm-3.rds")

irt.fact <- summary(mod, rotate = "oblimin")
irt.fact <- data.frame(irt.fact$rotF)

fact1 <- rownames(irt.fact[abs(irt.fact$F1) >= .4,])
fact2 <- rownames(irt.fact[abs(irt.fact$F2) >= .4,])
fact3 <- rownames(irt.fact[abs(irt.fact$F3) >= .4,])

fact1 ; fact2 ; fact3

psych::alpha(hr_new %>% 
               select(all_of(fact1)),
             check.keys = TRUE)

psych::alpha(hr_new %>% 
               select(all_of(fact2)),
             check.keys = TRUE)

psych::alpha(hr_new %>% 
               select(all_of(fact3)),
             check.keys = TRUE)


## IRT Model 2
mod <- mirt(hr_new,
            model = 4, 
            itemtype = "graded",
            method = "QMCEM",
            technical =
              list(NCYCLES = 2000))

saveRDS(mod, file = "model/irt-grm-2.rds")

irt.fact <- summary(mod, rotate = "oblimin")
irt.fact <- data.frame(irt.fact$rotF)

fact1 <- rownames(irt.fact[abs(irt.fact$F1) >= .3,])
fact2 <- rownames(irt.fact[abs(irt.fact$F2) >= .3,])
fact3 <- rownames(irt.fact[abs(irt.fact$F3) >= .3,])
fact4 <- rownames(irt.fact[abs(irt.fact$F4) >= .3,])

fact1 ; fact2 ; fact3 ; fact4

psych::alpha(hr_new %>% 
               select(all_of(fact1)),
             check.keys = TRUE)

psych::alpha(hr_new %>% 
               select(all_of(fact2)),
             check.keys = TRUE)

psych::alpha(hr_new %>% 
               select(all_of(fact3)),
             check.keys = TRUE)

psych::alpha(hr_new %>% 
               select(all_of(fact4)),
             check.keys = TRUE)


## IRT Model - Full
mod <- mirt(hr,
            model = 5, 
            itemtype = "graded",
            method = "QMCEM",
            technical =
              list(NCYCLES = 2000))

saveRDS(mod, file = "model/irt-grm-full-items.rds")

irt.fact <- summary(mod5, rotate = "oblimin")
irt.fact <- data.frame(irt.fact$rotF)

fact1 <- rownames(irt.fact[abs(irt.fact$F1) >= .4,])
fact2 <- rownames(irt.fact[abs(irt.fact$F2) >= .4,])
fact3 <- rownames(irt.fact[abs(irt.fact$F3) >= .4,])
fact4 <- rownames(irt.fact[abs(irt.fact$F4) >= .4,])
fact5 <- rownames(irt.fact[abs(irt.fact$F5) >= .4,])


fact1 ; fact2 ; fact3 ; fact4 ; fact5

psych::alpha(hr %>% 
               select(all_of(fact1)),
             check.keys = TRUE)

psych::alpha(hr %>% 
               select(all_of(fact2)),
             check.keys = TRUE)

psych::alpha(hr %>% 
               select(all_of(fact3)),
             check.keys = TRUE)

psych::alpha(hr %>% 
               select(all_of(fact4)),
             check.keys = TRUE)

psych::alpha(hr %>% 
               select(all_of(fact5)),
             check.keys = TRUE)

mod5 <- readRDS("model/irt-grm-full-items.rds")

## IRT Model - Full - 4 Factors
mod <- mirt(hr,
            model = 4, 
            itemtype = "graded",
            method = "QMCEM",
            technical =
              list(NCYCLES = 2000))

saveRDS(mod, file = "model/irt-grm-full-items-4-factors.rds")

irt.fact <- summary(mod, rotate = "oblimin")
irt.fact <- data.frame(irt.fact$rotF)

fact1 <- rownames(irt.fact[abs(irt.fact$F1) >= .4,])
fact2 <- rownames(irt.fact[abs(irt.fact$F2) >= .4,])
fact3 <- rownames(irt.fact[abs(irt.fact$F3) >= .4,])
fact4 <- rownames(irt.fact[abs(irt.fact$F4) >= .4,])


fact1 ; fact2 ; fact3 ; fact4 

psych::alpha(hr %>% 
               select(all_of(fact1)),
             check.keys = TRUE)

psych::alpha(hr %>% 
               select(all_of(fact2)),
             check.keys = TRUE)

psych::alpha(hr %>% 
               select(all_of(fact3)),
             check.keys = TRUE)

psych::alpha(hr %>% 
               select(all_of(fact4)),
             check.keys = TRUE)

## IRT Model - Full - 3 Factors - Minus q39 & q40
mod <- mirt(hr_new,
            model = 3, 
            itemtype = "graded",
            method = "QMCEM",
            technical =
              list(NCYCLES = 2000))

saveRDS(mod, file = "model/irt-grm-full-items-3-factors.rds")

irt.fact <- summary(mod, rotate = "oblimin")
irt.fact <- data.frame(irt.fact$rotF)

fact1 <- rownames(irt.fact[abs(irt.fact$F1) >= .4,])
fact2 <- rownames(irt.fact[abs(irt.fact$F2) >= .4,])
fact3 <- rownames(irt.fact[abs(irt.fact$F3) >= .4,])

fact1 ; fact2 ; fact3

psych::alpha(hr %>% 
               select(all_of(fact1)),
             check.keys = TRUE)

psych::alpha(hr %>% 
               select(all_of(fact2)),
             check.keys = TRUE)

psych::alpha(hr %>% 
               select(all_of(fact3)),
             check.keys = TRUE)


psych::alpha(
  hr %>% 
    select(
      rownames(irt.fact[(abs(irt.fact$F1) >= .4 &
                           ((irt.fact$F1 - irt.fact$F3) > abs(.2) &
                              (irt.fact$F1 - irt.fact$F2) > abs(.2))),])),
  check.keys = TRUE)

psych::alpha(
  hr %>% 
    select(
      rownames(irt.fact[(abs(irt.fact$F2) >= .4 &
                           ((irt.fact$F2 - irt.fact$F3) > abs(.2) &
                              (irt.fact$F2 - irt.fact$F1) > abs(.2))),])),
  check.keys = TRUE)

psych::alpha(
  hr %>% 
    select(
      rownames(irt.fact[(abs(irt.fact$F3) >= .4 &
                           ((irt.fact$F3 - irt.fact$F1) > abs(.2) &
                              (irt.fact$F3 - irt.fact$F2) > abs(.2))),])),
  check.keys = TRUE)



# Connect with Items
library(qualtRics)

# Get all survey IDs
surveys <- all_surveys()

# Create Codebook ----
## Download Items
item <- survey_questions(surveys$id[surveys$name == "Harm Reduction Scales - Study 1"]) 

item <- item %>% 
  filter(qname != "consent") %>% 
  select(scale = NULL, var_label = qname, item = question,
         values = NULL, missing_values = NULL)

scale1 <- rownames(irt.fact[(abs(irt.fact$F1) >= .4 &
                     ((irt.fact$F1 - irt.fact$F3) > abs(.2) &
                        (irt.fact$F1 - irt.fact$F2) > abs(.2))),])

scale2 <- rownames(irt.fact[(abs(irt.fact$F2) >= .4 &
                     ((irt.fact$F2 - irt.fact$F3) > abs(.2) &
                        (irt.fact$F2 - irt.fact$F1) > abs(.2))),])

scale3 <- rownames(irt.fact[(abs(irt.fact$F3) >= .4 &
                               ((irt.fact$F3 - irt.fact$F1) > abs(.2) &
                                  (irt.fact$F3 - irt.fact$F2) > abs(.2))),])

item <- item %>% 
  mutate(
    scale = case_when(
      var_label %in% fascale1 ~ "Scale 1",
      var_label %in% fascale2 ~ "Scale 2",
      var_label %in% fascale3 ~ "Scale 3"
    ),
    alt_scale = case_when(
     var_label %in% scale1 ~ "Scale 2",
     var_label %in% scale2 ~ "Scale 3",
     var_label %in% scale3 ~ "Scale 1"
  ))

# Scale 1s
psych::alpha(
  hr %>% 
    select(all_of(scale3)), 
  check.keys = TRUE
)

psych::alpha(
  hr %>% 
    select(all_of(fascale1)), 
  check.keys = TRUE
)

# Scale 2s
psych::alpha(
  hr %>% 
    select(all_of(scale1)), 
  check.keys = TRUE
)

psych::alpha(
  hr %>% 
    select(all_of(fascale2)), 
  check.keys = TRUE
)

# Scale 3s
psych::alpha(
  hr %>% 
    select(all_of(scale2)), 
  check.keys = TRUE
)

psych::alpha(
  hr %>% 
    select(all_of(fascale3)), 
  check.keys = TRUE
)

