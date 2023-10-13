#####################################
#Study 1 Checking Alternate Models  #
#####################################

# Libraries
library(psych)
library(GPArotation)
library(correlation)
library(corrplot)
library(tidyverse)

# Import data
df <- read.csv("data/clean/20231011_hr-scale-exploratory-data.csv")




# Current Model for Comparison ----
hr_new <- df %>% 
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
        q17, q12, q43, q15, q35, q37, q26, q34, q44, q20, q11, q28))


hrMatrix12 <- 
  POLYCHORIC_R(
   ))

fa13 <- fa(hrMatrix12, n.obs = 301, nfactors = 3, rotate = "oblimin", cor = "poly")
psych::print.psych(fa13, sort = "TRUE", cut = .3)

check_factorstructure(hr.ld %>% 
                        select(-c(q17, q12, q43, q15, q35, q37, q26, q34, q44, q20, q11, q28)))