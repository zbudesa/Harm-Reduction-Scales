# Install any needed packages
# packages <- c("psych", "lavaan", "tidyverse")
# install.packages(packages)


# Libraries
library(psych)
library(lavaan)
library(tidyverse)

# Data
data <- read.csv("data/osf/03_study3_data.csv")
items <- data %>% select(starts_with("q"), -Q_RecaptchaScore)

# Initial Model
model <-  'F1 =~ q7 + q1 + q6 + q18 + q19 + q21 + q24 + q14 + q13
           F2 =~ q33 + q42 + q36 + q35 + q38 + q34 + q41

           F1 ~~ F2' 

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
  select(q7, q1, q6, q24, q14, q18, q19, q13, q21)
scale2 <- data %>% 
  select(q42, q36, q33, q35, q38, q34, q41)

## Alpha
psych::alpha(scale1, check.keys = TRUE)
psych::alpha(scale2, check.keys = TRUE)

## Greatest Lower Bound
psych::glb.fa(scale1)
psych::glb.fa(scale2)

## Omega
items2 <- items %>% 
  select(q7, q1, q6, q24, q14, q18, q19, q13, q21,
         q42, q36, q33, q35, q38, q34, q41)

psych::omega(items2, poly = TRUE, nfactors = 2)


