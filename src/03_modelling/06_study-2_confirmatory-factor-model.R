######################################
#Study 2 Confirmatory Factor Analysis#
######################################

# Libraries
library(psych)
library(lavaan)
library(tidyverse)

# Import data
df <- read.csv("data/clean/20231020_hr-scale-confirmatory-data.csv")

# Select items
hr <- df %>% # N = 301
  select(starts_with("q"), -Q_RecaptchaScore)

# Initial Model Fit
model <-  'F1 =~ q7 + q1 + q6 + q14 + q16 + q18 + q19 + q21 + q39 + q13
           F2 =~ q38 + q33 + q36 + q42
           F3 =~ q2 + q9 + q20 + q23 + q29 + q12 + q10 + q28 + q32

           F1 ~~ F2
           F1 ~~ F3' 

fit <- cfa(
  model,
  data = df,
  ordered = TRUE,
  estimator = "DWLS",
  std.lv = TRUE
)      

fitmeasures(fit,
            c("cfi", "tli", "srmr", "rmsea", "chisq", "df", "pvalue"),
            output = "matrix")

# Reduced Model Fit
model.reduced <-  'F1 =~ q7 + q1 + q6 + q16 + q18 + q19 + q21 + q39 + q13
                   F2 =~ q38 + q36 + q42
                   F3 =~ q2 + q9 + q23 + q29 + q10 + q32

                   F1 ~~ F2
                   F1 ~~ F3' 

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

# Dynamic Fit Indicies
library(dynamic)

# Full Model
manual.model <-  'F1 =~ .908*q7 + .888*q1 + .780*q6 + .734*q14 + .746*q16 + .849*q18 + .840*q19 + .891*q21 + -.524*q39 + .765*q13
                  F2 =~ .883*q38 + .721*q33 + .709*q36 + -.763*q42
                  F3 =~ .812*q2 + .685*q9 + .177*q20 + .565*q23 + .690*q29 + .620*q12 + .697*q10 + .464*q28 + .621*q32

                  F1 ~~ .764*F2
                  F1 ~~ .679*F3
                  F2 ~~ .261*F3' 
cfaHB(manual.model, plot = TRUE, n = 375, manual = TRUE)

# Reduced Model
manual.model.reduced <-  
                 'F1 =~ .909*q7 + .894*q1 + .779*q6 + .734*q16 + .846*q18 + .842*q19 + .891*q21 + -.523*q39 + .762*q13
                  F2 =~ .873*q38 + .682*q36 + -.750*q42
                  F3 =~ .815*q2 + .681*q9 + .575*q23 + .710*q29 + .697*q10 + .604*q32

                  F1 ~~ .792*F2
                  F1 ~~ .661*F3
                  F2 ~~ .293*F3' 
cfaHB(manual.model.reduced, plot = TRUE, n = 380, manual = TRUE)







