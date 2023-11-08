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

# Locally Dependent Items
items2 <- items %>% select(-c(q4:q5, q25, q31, q40, q45, q8))

# Bartlett's Sphericity Test
cortest.bartlett(items2)

# Kaiser-Meyer-Olkin
KMO(items2)

# Parallel Analysis
fa.parallel(items2, cor = "poly", fa = "fa", fm = "ml")

# Factor Analysis
## Initial Model
fa <- fa(items2, nfactors = 5, rotate = "oblimin", cor = "poly", fm = "mle")
psych::print.psych(fa, sort = "TRUE", cut = .3)

## Items removed in the order they were removed
items3 <- items2 %>% select(-c(q12, q30, q17, q3, q43, q11, q22, q27, q23, q26, q44, q20))

## Final 3 Factor Model
fa2 <- fa(items3, nfactors = 3, rotate = "oblimin", cor = "poly", fm = "mle")
psych::print.psych(fa2, sort = "TRUE", cut = .3)

## Final 2 Factor Model, Remove Third Factor & Remove Low Loadings
items4 <- items3 %>% 
  select(-c(q2, q9, q10, q28,
            # Remove from Scale 1
            q37, q32, q29, q39, q15, q16))

fa3 <- fa(items4, nfactors = 2, rotate = "oblimin", cor = "poly", fm = "mle")
psych::print.psych(fa3, sort = "TRUE", cut = .3)

# Scale Reliability
scale1 <- data %>% 
  select(q7, q1, q6, q18, q19, q21, q24, q14, q13)
scale2 <- data %>% 
  select(q33, q42, q36, q35, q38, q34, q41)

# Alpha
psych::alpha(scale1, check.keys = TRUE)
psych::alpha(scale2, check.keys = TRUE)

# Greatest Lower Bound
psych::glb.fa(scale1)
psych::glb.fa(scale2)

# Omega
psych::omega(items4, poly = TRUE, nfactors = 2)
