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
df <- read.csv("data/clean/20231020_hr-scale-confirmatory-data.csv")

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
# cor.plot(hr, 
#          upper = FALSE, 
#          pval = FALSE,
#          gr = colorRampPalette(c("#B52127", "white", "#2171B5")))
hrMatrix <- cor(hr, use = "na.or.complete")
View(round(hrMatrix, 2))

# Inspecting Highly Correlated Items
hr %>% 
  pivot_longer(c(q3, q4, q7, q8)) %>% 
  group_by(name) %>% 
  summarize(
    mean = mean(value, na.rm = TRUE), 
    sd = sd(value, na.rm = TRUE))

# df %>% 
#   pivot_longer(c(q3,q4)) %>% 
#   ggplot() + 
#   aes(x = name, y = value) + 
#   geom_jitter() + 
#   facet_wrap(~name)

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

ld <- ld$localdep_stats

View(ld[abs(ld$Q3) > .2,])

# Building Dataset with Removed Local Dependence Items
hr.ld <-
  hr %>% 
  select(-c(q4:q5, 
            q31,
            q40,
            q25,
            q45))

hrMatrix <- POLYCHORIC_R(hr.ld)

# Bartlett & KMO
library(performance)

check_factorstructure(hrMatrix, n = 301)

cortest.bartlett(hrMatrix, n = 301) # 8619.69, p < .001
cortest.bartlett(hr) # 8275.10, p < .001

# KMO factor Adequacy
KMO(hrMatrix) 
# Overall MSA = 0.93

# Parallel Analysis
fa.parallel(hr.ld) # 3 factors

# Very Simple Structure
vss(hr.ld, n = length(hr.ld), fm = "mle") # MAP suggests 3, BIC 3, SABIC 7


## Initial Factor Analysis ----
fa1 <- fa(hrMatrix, n.obs = 301, nfactors = 5, rotate = "none", cor = "poly")
psych::print.psych(fa1, sort = "TRUE", cut = .3)

fa1$e.values
# 4 - 5 based on eigenvalues
# [1] 16.37246021  3.63176763  1.58375151  1.34144144  1.12552875  0.99702094  0.94389545  0.90219197  0.84977120  0.74605713
# [11]  0.73836259  0.69584954  0.66679792  0.60639336  0.58950337  0.53129198  0.51686222  0.50392217  0.48077815  0.45345073
# [21]  0.41913748  0.39793462  0.38458103  0.36056798  0.33382266  0.31555734  0.29973781  0.28876964  0.26706846  0.24678510
# [31]  0.21789708  0.20279603  0.19368633  0.18408851  0.17333516  0.15409720  0.12139128  0.09145913  0.07018890

# Inspect residuals
residuals <- factor.residuals(hrMatrix, fa1$loadings)
residuals <- as.matrix(residuals[upper.tri(residuals)])
large.resid <- abs(residuals) > .05
sum(large.resid) ; sum(large.resid)/nrow(residuals)

sqrt(mean(residuals^2))

hist(residuals)

# Model 2 ----
fa2 <- fa(hrMatrix, n.obs = 301, nfactors = 4, 
          rotate = "oblimin", cor = "poly",
          fm = "ml")
psych::print.psych(fa2, sort = "TRUE", cut = .3)

# Model 3 ----
# new correlation matrix
hrMatrix2 <- 
  POLYCHORIC_R(
    hr.ld %>% 
      select(-c(q12)))

# EFA
fa3 <- fa(hrMatrix2, n.obs = 301, nfactors = 3, 
          rotate = "oblimin", cor = "poly",
          fm = "ml")
psych::print.psych(fa3, sort = "TRUE", cut = .3)

# Model 4 ----
# new correlation matrix
hrMatrix3 <- 
  POLYCHORIC_R(
    hr.ld %>% 
      select(-c(q12, q30)))

# EFA
fa4 <- fa(hrMatrix3, n.obs = 301, nfactors = 3, 
          rotate = "oblimin", cor = "poly",
          fm = "ml")
psych::print.psych(fa4, sort = "TRUE", cut = .3)

# Model 5 ----
# new correlation matrix
hrMatrix4 <- 
  POLYCHORIC_R(
    hr.ld %>% 
      select(-c(q12, q30, q17, q3,
                q43, q11, q22,
                q27, q23, q26, q44,
                q20, 
                # < .4 loading
                q41,
                # Remove 3rd scale
                q2, q9, q32, q28, q29,
                # Remove from Scale 1
                q15, q21, q37, q39)))

# EFA
fa5 <- fa(hrMatrix4, n.obs = 301, nfactors = 2, 
          rotate = "oblimin", cor = "poly",
          fm = "ml", impute = "mean")
psych::print.psych(fa5, sort = "TRUE", cut = .3)


# Build Scales and Check Alphas ----
scale1 <- hr.ld %>% 
  select(q1, q6, q7, q16, q18, q19, q21, q39, q13)
scale2 <- hr.ld %>% 
  select(q42, q36, q33, q35, q34, q38, q41)


psych::alpha(scale1, check.keys = TRUE)
psych::alpha(scale2, check.keys = TRUE)


# Model 5 ----
# new correlation matrix
hrMatrix4 <- 
  POLYCHORIC_R(
    hr.ld %>% 
      select(q1, q6, q7, q16, q18, q19, q21, q13,
             q42, q36, q33, q35, q34, q38, q41))

# EFA
fa5 <- fa(hrMatrix4, n.obs = 301, nfactors = 2, 
          rotate = "oblimin", cor = "poly",
          fm = "ml", impute = "mean")
psych::print.psych(fa5, sort = "TRUE", cut = .3)

splitHalf(scale1)



