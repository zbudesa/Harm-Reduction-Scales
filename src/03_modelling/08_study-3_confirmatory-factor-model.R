############################################
#   Study 3 Confirmatory Factor Analysis   #
############################################

# Libraries
library(psych)
library(lavaan)
library(tidyverse)

# Import data
df <- read.csv("data/clean/20231025_hr-scale-confirmatory-data2.csv")

# Select items
hr <- df %>% # N = 301
  select(starts_with("q"), -Q_RecaptchaScore)

# Study 1 Model Fit
model.reduced <-  'F1 =~ q7 + q1 + q6 + q16 + q18 + q19 + q21 + q39 + q13
                   F2 =~ q38 + q36 + q42
                   F3 =~ q2 + q9 + q10 + q32

                   F1 ~~ F2
                   F1 ~~ F3' 

# Have to remove q23 & q29 due to restricted range of responses

fit.reduced <- cfa(
  model.reduced,
  data = df,
  ordered = TRUE,
  estimator = "WLS",
  std.lv = TRUE
)      

fitmeasures(fit.reduced,
            c("cfi", "tli", "srmr", "rmsea", "chisq", "df", "pvalue"),
            output = "matrix")

# Study 2 Model Fit
model <-  'F1 =~ q21 + q1 + q6 + q7 + q16 + q18 + q19 + q13
           F2 =~ q36 + q42 + q33 + q35 + q34 + q38 + q41' 

fit <- cfa(
  model,
  data = df,
  ordered = TRUE,
  estimator = "WLS",
  std.lv = TRUE
)      

fitmeasures(fit,
            c("cfi", "tli", "srmr", "rmsea", "chisq", "df", "pvalue"),
            output = "matrix")

summary(fit)

# Dynamic Model Fit
manual.model <-  'F1 =~ .946*q1 + .865*q6 + .957*q7 + .817*q16 + .909*q18 + .925*q19 + .963*q21 + -.720*q39 + .791*q13
                  F2 =~ -.820*q42 + .877*q36 + .771*q33 + -.822*q35 + -.674*q34 + .806*q38 + -.745*q41

                  F1 ~~ .801*F2' 

library(dynamic)
cfaHB(manual.model, plot = TRUE, manual = TRUE, n = 329)

