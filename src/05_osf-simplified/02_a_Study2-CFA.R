# Install any needed packages
# packages <- c("psych", "lavaan", "tidyverse")
# install.packages(packages)


# Libraries
library(psych)
library(lavaan)
library(tidyverse)

# Data
data <- read.csv(file.choose())
items <- data %>% select(starts_with("q"), -Q_RecaptchaScore)

# Initial Model
model <-  'F1 =~ q1 + q18 + q7 + q19 + q21 + q6 + q16 + q39
           F2 =~ q42 + q36 + q38 + q33 + q41
           F3 =~ q9 + q2 + q23 + q10 + q29

           F1 ~~ F2
           F1 ~~ F3' 

fit <- cfa(
  model,
  data = items,
  ordered = TRUE,
  estimator = "WLS",
  std.lv = TRUE)   


fitmeasures(fit,
            c("cfi", "tli", "srmr", "rmsea", "chisq", "df", "pvalue"),
            output = "matrix")

summary(fit)

## Full Model Scale Reliability
scale1 <- data %>% 
  select(q1, q18, q7, q19, q21, q6, q16, q39)
scale2 <- data %>% 
  select(q42, q36, q38, q33, q41)
scale3 <- data %>% 
  select(q9, q2, q23, q10, q29)

## Alpha
psych::alpha(scale1, check.keys = TRUE)
psych::alpha(scale2, check.keys = TRUE)
psych::alpha(scale3, check.keys = TRUE)

## Greatest Lower Bound
psych::glb.fa(scale1)
psych::glb.fa(scale2)
psych::glb.fa(scale3)

## Omega
psych::omega(items2, poly = TRUE, nfactors = 3)


