#####################################
# Study 1 Checking Alternate Models #
#####################################

# Libraries
library(psych)
library(GPArotation)
library(EFA.dimensions)
library(tidyverse)

# Import data
df <- read.csv("data/clean/20231011_hr-scale-exploratory-data.csv")

# Current Model for Comparison ----
hr_current <- df %>% 
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
    q17, q12, q43, q15, q35, q37, q26, q34, q44, q20, q11, q28
  ))

hrMatrix_current <- 
  POLYCHORIC_R(hr_current)

fa.current <- fa(hrMatrix_current, n.obs = 301, nfactors = 3, rotate = "oblimin", cor = "poly")
psych::print.psych(fa.current, sort = "TRUE", cut = .3)
# Replacing Locally dependent items
## Alternative Models - Items 23, 24, 25----
hr_new <- df %>% 
  select(starts_with("q") & -Q_RecaptchaScore) %>% 
  select(-c(
    # Items with Local Dependence (& Closely related Item)
    q5, q4, # q3
    q23, q25, # q24 Replace here
    q40, #39
    q31, # q30
    q8,  # q7
    q45, # q12
    q22, # q27
    q32, # q29
    # Items Removed from Final Scales
    q17, q12, q43, q15, q35, q37, q26, q34, q44, q20, q11, q28
  ))

hrMatrix_new <- 
  POLYCHORIC_R(hr_new)

fa.new <- fa(hrMatrix_new, n.obs = 301, nfactors = 3, rotate = "oblimin", cor = "poly")
psych::print.psych(fa.new, sort = "TRUE", cut = .3)
# Fit is slightly worse

hr_new <- df %>% 
  select(starts_with("q") & -Q_RecaptchaScore) %>% 
  select(-c(
    # Items with Local Dependence (& Closely related Item)
    q5, q4, # q3
    q23, q24, # q25 Replace here
    q40, #39
    q31, # q30
    q8,  # q7
    q45, # q12
    q22, # q27
    q32, # q29
    # Items Removed from Final Scales
    q17, q12, q43, q15, q35, q37, q26, q34, q44, q20, q11, q28
  ))

hrMatrix_new <- 
  POLYCHORIC_R(hr_new)

fa.new <- fa(hrMatrix_new, n.obs = 301, nfactors = 3, rotate = "oblimin", cor = "poly")
psych::print.psych(fa.new, sort = "TRUE", cut = .3)
# Fit is slightly worse

## Alternative Models - Items 39 & 40 ----
hr_new <- df %>% 
  select(starts_with("q") & -Q_RecaptchaScore) %>% 
  select(-c(
    # Items with Local Dependence (& Closely related Item)
    q5, q4, # q3
    q24, q25, # q23 
    q39, #q40, #Replace here
    q31, # q30
    q8,  # q7
    q45, # q12
    q22, # q27
    q32, # q29
    # Items Removed from Final Scales
    q17, q12, q43, q15, q35, q37, q26, q34, q44, q20, q11, q28
  ))

hrMatrix_new <- 
  POLYCHORIC_R(hr_new)

fa.new <- fa(hrMatrix_new, n.obs = 301, nfactors = 3, rotate = "oblimin", cor = "poly")
psych::print.psych(fa.new, sort = "TRUE", cut = .3)
# Fit is slightly worse with 40, much worse with both.

## Alternative Models - Items 39 & 40 ----
hr_new <- df %>% 
  select(starts_with("q") & -Q_RecaptchaScore) %>% 
  select(-c(
    # Items with Local Dependence (& Closely related Item)
    q5, q4, # q3
    q24, q25, # q23 
    q40, #q39, 
    q30, # q31 #Replace here
    q8,  # q7
    q45, # q12
    q22, # q27
    q32, # q29
    # Items Removed from Final Scales
    q17, q12, q43, q15, q35, q37, q26, q34, q44, q20, q11, q28
  ))

hrMatrix_new <- 
  POLYCHORIC_R(hr_new)

fa.new <- fa(hrMatrix_new, n.obs = 301, nfactors = 3, rotate = "oblimin", cor = "poly")
psych::print.psych(fa.new, sort = "TRUE", cut = .3)
# Fit is slightly better with 30, but I think it is worth maintaining

## Alternative Models - Items 7 & 8 ----
hr_new <- df %>% 
  select(starts_with("q") & -Q_RecaptchaScore) %>% 
  select(-c(
    # Items with Local Dependence (& Closely related Item)
    q5, q4, # q3
    q24, q25, # q23 
    q40, #q39, 
    q30, # q31 
    q7,  #q8, #Replace here
    q45, # q12
    q22, # q27
    q32, # q29
    # Items Removed from Final Scales
    q17, q12, q43, q15, q35, q37, q26, q34, q44, q20, q11, q28
  ))

hrMatrix_new <- 
  POLYCHORIC_R(hr_new)

fa.new <- fa(hrMatrix_new, n.obs = 301, nfactors = 3, rotate = "oblimin", cor = "poly")
psych::print.psych(fa.new, sort = "TRUE", cut = .3)
# Fit is slightly better with 8, but I think it is worth maintaining

## Alternative Models - Items 45 & 12 ----
hr_current <- df %>% 
  select(starts_with("q") & -Q_RecaptchaScore) %>% 
  select(-c(
    # Items with Local Dependence (& Closely related Item)
    q4, q5, # q3
    q24, q25, # q23
    q40, #39
    q31, # q30
    q8,  # q7
    q12, # q45
    q22, # q27
    q32, # q29
    # Items Removed from Final Scales
    q17, q12, q43, q15, q35, q37, q26, q34, q44, q20, q11, q28
  ))

hrMatrix_current <- 
  POLYCHORIC_R(hr_current)

fa.current <- fa(hrMatrix_current, n.obs = 301, nfactors = 3, rotate = "oblimin", cor = "poly")
psych::print.psych(fa.current, sort = "TRUE", cut = .3)
# Item 45 cross loads (people), and would be dropped

## Alternative Models - Items 22 & 27 INVESTIGATE ALTERATIVE MODEL HERE ----
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
    q27, # q22
    q32, # q29
    # Items Removed from Final Scales
    q17, q12, q43, q15, q35, q37, q26, q34, q44, q20, q11, q28
  ))

hrMatrix_new <- 
  POLYCHORIC_R(hr_new)

fa.new <- fa(hrMatrix_new, n.obs = 301, nfactors = 3, rotate = "oblimin", cor = "poly")
psych::print.psych(fa.new, sort = "TRUE", cut = .3)
# Induces additional fit problems for this model

## Alternative Models - Items 32 & 29 ----
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
    q29, # q32
    # Items Removed from Final Scales
    q17, q12, q43, q15, q35, q37, q26, q34, q44, q20, q11, q28
  ))

hrMatrix_new <- 
  POLYCHORIC_R(hr_new)

fa.new <- fa(hrMatrix_new, n.obs = 301, nfactors = 3, rotate = "oblimin", cor = "poly")
psych::print.psych(fa.new, sort = "TRUE", cut = .3)
# Basically no change, better fit with 29 instead of 32

# Trying a completely different structure by flipping 22 & 27 ----
hr_alt <- df %>% 
  select(starts_with("q") & -Q_RecaptchaScore) %>% 
  select(-c(
    # Items with Local Dependence (& Closely related Item)
    q4, q5, # q3
    q24, q25, # q23
    q40, #39
    q31, # q30
    q8,  # q7
    q45, # q12
    q27, # q22
    q32, # q29
    # Items to Drop
    
  ))

hrMatrix_alt <- 
  POLYCHORIC_R(hr_alt)

fa.alt <- fa(hrMatrix_alt, n.obs = 301, nfactors = 5, rotate = "oblimin", cor = "poly")
psych::print.psych(fa.alt, sort = "TRUE", cut = .3)
# Drop 26, no loading > .3

hr_alt1 <- df %>% 
  select(starts_with("q") & -Q_RecaptchaScore) %>% 
  select(-c(
    # Items with Local Dependence (& Closely related Item)
    q4, q5, # q3
    q24, q25, # q23
    q40, #39
    q31, # q30
    q8,  # q7
    q45, # q12
    q27, # q22
    q32, # q29
    # Items to Drop
    q26
  ))

hrMatrix_alt1 <- 
  POLYCHORIC_R(hr_alt1)

fa.alt1 <- fa(hrMatrix_alt1, n.obs = 301, nfactors = 5, rotate = "oblimin", cor = "poly")
psych::print.psych(fa.alt1, sort = "TRUE", cut = .3)
# q37 has cross loadings


hr_alt2 <- df %>% 
  select(starts_with("q") & -Q_RecaptchaScore) %>% 
  select(-c(
    # Items with Local Dependence (& Closely related Item)
    q4, q5, # q3
    q24, q25, # q23
    q40, #39
    q31, # q30
    q8,  # q7
    q45, # q12
    q27, # q22
    q32, # q29
    # Items to Drop
    q26, q37
  ))

hrMatrix_alt2 <- 
  POLYCHORIC_R(hr_alt2)

fa.alt2 <- fa(hrMatrix_alt2, n.obs = 301, nfactors = 5, rotate = "oblimin", cor = "poly")
psych::print.psych(fa.alt2, sort = "TRUE", cut = .3)
# q43 only one to end up with loading on factor 5

hr_alt3 <- df %>% 
  select(starts_with("q") & -Q_RecaptchaScore) %>% 
  select(-c(
    # Items with Local Dependence (& Closely related Item)
    q4, q5, # q3
    q24, q25, # q23
    q40, #39
    q31, # q30
    q8,  # q7
    q45, # q12
    q27, # q22
    q32, # q29
    # Items to Drop
    q26, q37, q43
  ))

hrMatrix_alt3 <- 
  POLYCHORIC_R(hr_alt3)

fa.alt3 <- fa(hrMatrix_alt3, n.obs = 301, nfactors = 4, rotate = "oblimin", cor = "poly")
psych::print.psych(fa.alt3, sort = "TRUE", cut = .3)
# q28 no loading

hr_alt4 <- df %>% 
  select(starts_with("q") & -Q_RecaptchaScore) %>% 
  select(-c(
    # Items with Local Dependence (& Closely related Item)
    q4, q5, # q3
    q24, q25, # q23
    q40, #39
    q31, # q30
    q8,  # q7
    q45, # q12
    q27, # q22
    q32, # q29
    # Items to Drop
    q26, q37, q43, q28
  ))

hrMatrix_alt4 <- 
  POLYCHORIC_R(hr_alt4)

fa.alt4 <- fa(hrMatrix_alt4, n.obs = 301, nfactors = 4, rotate = "oblimin", cor = "poly")
psych::print.psych(fa.alt4, sort = "TRUE", cut = .3)
# q35 cross loading

hr_alt5 <- df %>% 
  select(starts_with("q") & -Q_RecaptchaScore) %>% 
  select(-c(
    # Items with Local Dependence (& Closely related Item)
    q4, q5, # q3
    q24, q25, # q23
    q40, #39
    q31, # q30
    q8,  # q7
    q45, # q12
    q27, # q22
    q32, # q29
    # Items to Drop
    q26, q37, q43, q28, q35
  ))

hrMatrix_alt5 <- 
  POLYCHORIC_R(hr_alt5)

fa.alt5 <- fa(hrMatrix_alt5, n.obs = 301, nfactors = 4, rotate = "oblimin", cor = "poly")
psych::print.psych(fa.alt5, sort = "TRUE", cut = .3)
# q44 cross loading

hr_alt6 <- df %>% 
  select(starts_with("q") & -Q_RecaptchaScore) %>% 
  select(-c(
    # Items with Local Dependence (& Closely related Item)
    q4, q5, # q3
    q24, q25, # q23
    q40, #39
    q31, # q30
    q8,  # q7
    q45, # q12
    q27, # q22
    q32, # q29
    # Items to Drop
    q26, q37, q43, q28, q35, q44
  ))

hrMatrix_alt6 <- 
  POLYCHORIC_R(hr_alt6)

fa.alt6 <- fa(hrMatrix_alt6, n.obs = 301, nfactors = 4, rotate = "oblimin", cor = "poly")
psych::print.psych(fa.alt6, sort = "TRUE", cut = .3)
# q14 multiple loadings

hr_alt7 <- df %>% 
  select(starts_with("q") & -Q_RecaptchaScore) %>% 
  select(-c(
    # Items with Local Dependence (& Closely related Item)
    q4, q5, # q3
    q24, q25, # q23
    q40, #39
    q31, # q30
    q8,  # q7
    q45, # q12
    q27, # q22
    q32, # q29
    # Items to Drop
    q26, q37, q43, q28, q35, q44, q14
  ))

hrMatrix_alt7 <- 
  POLYCHORIC_R(hr_alt7)

fa.alt7 <- fa(hrMatrix_alt7, n.obs = 301, nfactors = 4, rotate = "oblimin", cor = "poly")
psych::print.psych(fa.alt7, sort = "TRUE", cut = .3)
# q17 - cross loadings

hr_alt8 <- df %>% 
  select(starts_with("q") & -Q_RecaptchaScore) %>% 
  select(-c(
    # Items with Local Dependence (& Closely related Item)
    q4, q5, # q3
    q24, q25, # q23
    q40, #39
    q31, # q30
    q8,  # q7
    q45, # q12
    q27, # q22
    q32, # q29
    # Items to Drop
    q26, q37, q43, q28, q35, q44, q14, q17
  ))

hrMatrix_alt8 <- 
  POLYCHORIC_R(hr_alt8)

fa.alt8 <- fa(hrMatrix_alt8, n.obs = 301, nfactors = 4, rotate = "oblimin", cor = "poly")
psych::print.psych(fa.alt8, sort = "TRUE", cut = .3)
# q20 cross loadings

hr_alt9 <- df %>% 
  select(starts_with("q") & -Q_RecaptchaScore) %>% 
  select(-c(
    # Items with Local Dependence (& Closely related Item)
    q4, q5, # q3
    q24, q25, # q23
    q40, #39
    q31, # q30
    q8,  # q7
    q45, # q12
    q27, # q22
    q32, # q29
    # Items to Drop
    q26, q37, q43, q28, q35, q44, q14, q17, q20
  ))

hrMatrix_alt9 <- 
  POLYCHORIC_R(hr_alt9)

# 4 factor model has onlhy 3 eigenvalues > 1
fa.alt9 <- fa(hrMatrix_alt9, n.obs = 301, nfactors = 3, rotate = "oblimin", cor = "poly")
psych::print.psych(fa.alt9, sort = "TRUE", cut = .3)
# q12 closest cross loading

hr_alt10 <- df %>% 
  select(starts_with("q") & -Q_RecaptchaScore) %>% 
  select(-c(
    # Items with Local Dependence (& Closely related Item)
    q4, q5, # q3
    q24, q25, # q23
    q40, #39
    q31, # q30
    q8,  # q7
    q45, # q12
    q27, # q22
    q32, # q29
    # Items to Drop
    q26, q37, q43, q28, q35, q44, q14, q17, q20,
    q12
  ))

hrMatrix_alt10 <- 
  POLYCHORIC_R(hr_alt10)

fa.alt10 <- fa(hrMatrix_alt10, n.obs = 301, nfactors = 3, rotate = "oblimin", cor = "poly")
psych::print.psych(fa.alt10, sort = "TRUE", cut = .3)
# q2 cross loading

hr_alt11 <- df %>% 
  select(starts_with("q") & -Q_RecaptchaScore) %>% 
  select(-c(
    # Items with Local Dependence (& Closely related Item)
    q4, q5, q24, q25, q40, q31, q8, q45, q27, q32,  
    # Items to Drop
    q26, q37, q43, q28, q35, q44, q14, q17, q20,
    q12, q2
  ))

hrMatrix_alt11 <- 
  POLYCHORIC_R(hr_alt11)

fa.alt11 <- fa(hrMatrix_alt11, n.obs = 301, nfactors = 3, rotate = "oblimin", cor = "poly")
psych::print.psych(fa.alt11, sort = "TRUE", cut = .3)
# q11

hr_alt12 <- df %>% 
  select(starts_with("q") & -Q_RecaptchaScore) %>% 
  select(-c(
    # Items with Local Dependence (& Closely related Item)
    q4, q5, q24, q25, q40, q31, q8, q45, q27, q32,  
    # Items to Drop
    q26, q37, q43, q28, q35, q44, q14, q17, q20,
    q12, q2, q11
  ))

hrMatrix_alt12 <- 
  POLYCHORIC_R(hr_alt12)

fa.alt12 <- fa(hrMatrix_alt12, n.obs = 301, nfactors = 3, rotate = "oblimin", cor = "poly")
psych::print.psych(fa.alt12, sort = "TRUE", cut = .3)
# Success! How different is this from the existing model?

## Build Scales 
altscale1 <- c("q1", "q7", "q18", "q19", "q6", "q21", "q3", "q13", "q16", "q39")
altscale2 <- c("q42", "q36", "q38", "q33", "q15", "q41", "q34")
altscale3 <- c("q9", "q10", "q22", "q29", "q30", "q23")

### Alphas
psych::alpha(df %>% select(all_of(altscale1)),
             check.keys = TRUE)

psych::alpha(df %>% select(all_of(altscale2)),
             check.keys = TRUE)

psych::alpha(df %>% select(all_of(altscale3)),
             check.keys = TRUE)












