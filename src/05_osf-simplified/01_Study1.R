# Install any needed packages
# packages <- c("psych", "tidyverse")
# install.packages(packages)


# Libraries
library(psych)
library(tidyverse)

# Data
data <- read.csv(file.choose())
items <- data %>% select(starts_with("q"), -Q_RecaptchaScore)
# Remove Locally Dependent Items
items1 <- items %>% select(-c(q4, q5, q8, q22, q24, q25, q32, q31, q40, q45))

# Bartlett's Sphericity Test
cortest.bartlett(items1)

# Kaiser-Meyer-Olkin
KMO(items1)

# Parallel Analysis
fa.parallel(items, cor = "poly", fa = "fa", fm = "ml")

# Factor Analysis
## Initial Model
fa1 <- fa(items1, nfactors = 5, rotate = "oblimin", cor = "poly", fm = "mle")
psych::print.psych(fa1, sort = "TRUE", cut = .3)

## Items removed in the order they were removed
items2 <- items1 %>% 
  select(-c(q17, q12, q43, q15, q35, q37, q26, q34, q44, q20, q11, q28, q14, q27, q30, q3, q13))

## Final Model
fa2 <- fa(items2, nfactors = 3, rotate = "oblimin", cor = "poly", fm = "mle")
psych::print.psych(fa2, sort = "TRUE", cut = .3)

# Scale Reliability
scale1 <- data %>% 
  select(q1, q18, q7, q19, q21, q6, q16, q39)
scale2 <- data %>% 
  select(q36, q42, q38, q33, q41)
scale3 <- data %>% 
  select(q9, q2, q23, q10, q29)

# Alpha
psych::alpha(scale1, check.keys = TRUE)
psych::alpha(scale2, check.keys = TRUE)
psych::alpha(scale3, check.keys = TRUE)

# Greatest Lower Bound
psych::glb.fa(scale1)
psych::glb.fa(scale2)
psych::glb.fa(scale3)

# Omega
psych::omega(items2, poly = TRUE, nfactors = 3) # Problem
