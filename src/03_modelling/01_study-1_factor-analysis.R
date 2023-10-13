#####################################
#Study 1 Exploratory Factor Analysis#
#####################################

# Libraries
library(psych)
library(GPArotation)
library(correlation)
library(corrplot)
library(tidyverse)

# Import data
df <- read.csv("data/clean/20231011_hr-scale-exploratory-data.csv")

# Select items
hr <- df %>% # N = 301
  select(starts_with("q"), -Q_RecaptchaScore)

# Removed Concerning Cases
hr2 <- df %>% 
  filter(attn == 3 &
           Duration..in.seconds. > 200 &
           Q_RecaptchaScore > .5) %>% # N = 248
  select(starts_with("q"), -Q_RecaptchaScore)

# Correlation Matrix
cor.plot(hr2, 
         sort = TRUE, 
         upper = FALSE, 
         pval = FALSE,
         gr = colorRampPalette(c("#B52127", "white", "#2171B5")))
hrMatrix <- cor(hr, use = "na.or.complete")
View(round(hrMatrix, 2))

# Inspecting Highly Correlated Items
hr %>% 
  pivot_longer(c(q3, q4, q7, q8)) %>% 
  group_by(name) %>% 
  summarize(
    mean = mean(value, na.rm = TRUE), 
    sd = sd(value, na.rm = TRUE))

df %>% 
  pivot_longer(c(q3,q4)) %>% 
  ggplot() + 
  aes(x = name, y = value) + 
  geom_jitter() + 
  facet_wrap(~name)

df %>% 
  pivot_longer(c(q7,q8)) %>%
  ggplot() + 
  aes(x = name, y = value) + 
  geom_jitter() + 
  facet_wrap(~name, scales = "free")

inspect <- df %>% 
  select(q3,q4,q7,q8) %>% 
  psych::alpha()

inspect$response.freq %>% 
  as_tibble() %>% 
  mutate(item = c(3,4,7,8)) %>% 
  pivot_longer(c(`1`:`6`))  %>% 
  ggplot() +
  aes(fill = name, y = value, x = item) + 
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  facet_wrap(~ item, scales = "free")

# Assessing for Local Item Dependence
library(EFA.dimensions)

ld <- LOCALDEP(hr)

# Building Dataset with Removed Local Dependence Items
hr.ld <-
  hr %>% 
  select(-c(q4:q5, q25, q40,
            q31, q8, q24, q45, 
            q22, q32))

hrMatrix <- POLYCHORIC_R(hr.ld)

# Bartlett & KMO
library(performance)

check_factorstructure(hrMatrix, n = 301)

cortest.bartlett(hrMatrix, n = 301) # 8619.69, p < .001
cortest.bartlett(hr) # 8275.10, p < .001

# KMO factor Adequacy
KMO(hrMatrix) 
# Overall MSA = 0.94

# What is the determinant of the matrix?
det(hrMatrix) # 3.719035e-12
det(hrMatrix) > .00001 # No. Not the greatest

# Parallel Analysis
fa.parallel(hr.ld) # 4 factors

# Very Simple Structure
vss(hr.ld, n = length(hr.ld), fm = "mle") # MAP suggests 4, BIC 3, SABIC 5


## Initial Factor Analysis ----
fa1 <- fa(hrMatrix, n.obs = 301, nfactors = 5, rotate = "none", cor = "poly")
psych::print.psych(fa1, sort = "TRUE", cut = .3)

fa1$e.values
# 4 - 5 based on eigenvalues
# [1] 15.0655223  3.3523919  1.4946177  1.2704324  1.0747913  0.9763345  0.8369580  0.7953042  0.7431390  0.6956115  0.6744081  0.6386878
# [13]  0.6094020  0.5693275  0.5466554  0.5215616  0.4955943  0.4547034  0.4132554  0.3912640  0.3672601  0.3404331  0.3188535  0.2799967
# [25]  0.2638317  0.2545245  0.2463219  0.2347694  0.2147308  0.1955085  0.1658685  0.1561116  0.1252800  0.1103726  0.1061746

# Inspect residuals
residuals <- factor.residuals(hrMatrix, fa1$loadings)
residuals <- as.matrix(residuals[upper.tri(residuals)])
large.resid <- abs(residuals) > .05
sum(large.resid) ; sum(large.resid)/nrow(residuals)

sqrt(mean(residuals^2))

hist(residuals)

# Model 2 ----
fa2 <- fa(hrMatrix, n.obs = 301, nfactors = 4, rotate = "oblimin", cor = "poly")
psych::print.psych(fa2, sort = "TRUE", cut = .3)

# Model 3 ----
# new correlation matrix
hrMatrix2 <- 
  POLYCHORIC_R(
    hr.ld %>% 
      select(-c(q17)))

# EFA
fa3 <- fa(hrMatrix2, n.obs = 301, nfactors = 4, rotate = "oblimin", cor = "poly")
psych::print.psych(fa3, sort = "TRUE", cut = .3)

# Model 4 ----
# new correlation matrix
hrMatrix3 <- 
  POLYCHORIC_R(
    hr.ld %>% 
      select(-c(q17, q12)))

# EFA
fa4 <- fa(hrMatrix3, n.obs = 301, nfactors = 4, rotate = "oblimin", cor = "poly")
psych::print.psych(fa4, sort = "TRUE", cut = .3)

# Model 5 ----
# new correlation matrix
hrMatrix4 <- 
  POLYCHORIC_R(
    hr.ld %>% 
      select(-c(q17, q12, q43)))

# EFA
fa5 <- fa(hrMatrix4, n.obs = 301, nfactors = 4, rotate = "oblimin", cor = "poly")
psych::print.psych(fa5, sort = "TRUE", cut = .3)

# Model 6 ----
# new correlation matrix
hrMatrix5 <- 
  POLYCHORIC_R(
    hr.ld %>% 
      select(-c(q17, q12, q43, q15)))

# EFA
fa6 <- fa(hrMatrix5, n.obs = 301, nfactors = 4, rotate = "oblimin", cor = "poly")
psych::print.psych(fa6, sort = "TRUE", cut = .3)

fa6$e.values

# Model 7 ----
# new correlation matrix
hrMatrix6 <- 
  POLYCHORIC_R(
    hr.ld %>% 
      select(-c(q17, q12, q43, q15, q35)))

# EFA
fa7 <- fa(hrMatrix6, n.obs = 301, nfactors = 4, rotate = "oblimin", cor = "poly")
psych::print.psych(fa7, sort = "TRUE", cut = .3)

fa7$e.values

# Model 8 ----
# new correlation matrix
hrMatrix7 <- 
  POLYCHORIC_R(
    hr.ld %>% 
      select(-c(q17, q12, q43, q15, q35, q37, q26)))

# EFA
fa8 <- fa(hrMatrix7, n.obs = 301, nfactors = 4, rotate = "oblimin", cor = "poly")
psych::print.psych(fa8, sort = "TRUE", cut = .3)

# Model 9 ----
# new correlation matrix
hrMatrix8 <- 
  POLYCHORIC_R(
    hr.ld %>% 
      select(-c(q17, q12, q43, q15, q35, q37, q26, q34)))

# EFA
fa9 <- fa(hrMatrix8, n.obs = 301, nfactors = 4, rotate = "oblimin", cor = "poly")
psych::print.psych(fa9, sort = "TRUE", cut = .3)

# Model 10 ----
# new correlation matrix
hrMatrix9 <- 
  POLYCHORIC_R(
    hr.ld %>% 
      select(-c(q17, q12, q43, q15, q35, q37, q26, q34, q44)))

# EFA
fa10 <- fa(hrMatrix9, n.obs = 301, nfactors = 4, rotate = "oblimin", cor = "poly")
psych::print.psych(fa10, sort = "TRUE", cut = .3)

# Model 11 ----
# new correlation matrix
hrMatrix10 <- 
  POLYCHORIC_R(
    hr.ld %>% 
      select(-c(q17, q12, q43, q15, q35, q37, q26, q34, q44, q20)))

# EFA
fa11 <- fa(hrMatrix10, n.obs = 301, nfactors = 3, rotate = "oblimin", cor = "poly")
psych::print.psych(fa11, sort = "TRUE", cut = .3)

# Model 12 ----
# new correlation matrix
hrMatrix11 <- 
  POLYCHORIC_R(
    hr.ld %>% 
      select(-c(q17, q12, q43, q15, q35, q37, q26, q34, q44, q20, q11)))

# EFA
fa12 <- fa(hrMatrix11, n.obs = 301, nfactors = 3, rotate = "oblimin", cor = "poly")
psych::print.psych(fa12, sort = "TRUE", cut = .3)

# Model 13 ----
# new correlation matrix
hrMatrix12 <- 
  POLYCHORIC_R(
    hr.ld %>% 
      select(-c(q17, q12, q43, q15, q35, q37, q26, q34, q44, q20, q11, q28)))

# EFA
fa13 <- fa(hrMatrix12, n.obs = 301, nfactors = 3, rotate = "oblimin", cor = "poly")
psych::print.psych(fa13, sort = "TRUE", cut = .3)


