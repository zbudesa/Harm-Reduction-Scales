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


POLYCHORIC_R(hr) -> polymax







# It looks like Items 4 & 8 are better distributed
# I'm going to drop 3 & 7 for now, but will run 
# everything with them later. 

hr.4.8 <- hr %>% 
  select(-c(q3,q7))

hrMatrix_short <- cor(hr.4.8, use = "na.or.complete")


# Bartlett & KMO
library(performance)

check_factorstructure(hr.4.8)

cortest.bartlett(polymax, n = 301) # 8619.69, p < .001
cortest.bartlett(hr) # 8275.10, p < .001

# KMO factor Adequacy
KMO(hr.4.8) 
# Overall MSA = 0.94
# Lowest item: q20 = .78
# Highest: q1, q14, q18, q19, q21, q34 = .97

# What is the determinant of the matrix?
det(polymax) # 6.705847e-14
det(hrMatrix_short) > .00001 # No. Not the greatest

# Parallel Analysis
fa.parallel(hr.4.8) # 5 factors
fa.parallel(hr.4.8, cor = "poly") # 5 factors

# Very Simple Structure
vss(hr.4.8, n = length(hr.4.8), fm = "mle") # MAP suggests 5, BIC 4, SABIC 11

# Principal Components Analysis ----
pc1 <- principal(hrMatrix_short, nfactors = length(hrMatrix_short[,1]), rotate = "none")
pc1
plot(pc1$values, type = "b")

pc2 <- principal(hrMatrix_short, nfactors = 5, rotate = "none")
pc2
plot(pc2$values, type = "b")

# Inspect residuals
residuals <- factor.residuals(hrMatrix_short, pc2$loadings)
residuals <- as.matrix(residuals[upper.tri(residuals)])
large.resid <- abs(residuals) > .05
sum(large.resid) ; sum(large.resid)/nrow(residuals)

sqrt(mean(residuals^2))

hist(residuals)

DIMTESTS(hr, corkind = "polychoric")

## Initial Factor Analysis ----
fa1 <- fa(polymax, nfactors = 5, rotate = "none")
psych::print.psych(fa1, sort = "TRUE", cut = .3)

efa <- EFA(polymax, corkind = "polychoric", rotation = "oblimin", Ncases = 301, Nfactors = 3, extraction = "wls")

# Inspect residuals
residuals <- factor.residuals(hrMatrix_short, fa1$loadings)
residuals <- as.matrix(residuals[upper.tri(residuals)])
large.resid <- abs(residuals) > .05
sum(large.resid) ; sum(large.resid)/nrow(residuals)

sqrt(mean(residuals^2))

hist(residuals)

# Now with the matrix
fa1matrix <- fa(hrMatrix_short, n.obs = 301, nfactors = 5, rotate = "none")
psych::print.psych(fa1matrix, sort = "TRUE", cut = .3)

# Inspect residuals
residuals <- factor.residuals(hrMatrix_short, fa1matrix$loadings)
residuals <- as.matrix(residuals[upper.tri(residuals)])
large.resid <- abs(residuals) > .05
sum(large.resid) ; sum(large.resid)/nrow(residuals)

sqrt(mean(residuals^2))

hist(residuals)


# Model 2 ----
fa2 <- fa(hrMatrix_short, n.obs = 301, nfactors = 4, rotate = "oblimin")
psych::print.psych(fa2, sort = "TRUE", cut = .3)

# Model 3 ----
# new correlation matrix
hr.3 <- hr %>% 
  select(-c(q3,q7,q29))

hrMatrix_3 <- cor(hr.3, use = "na.or.complete")

fa.parallel(hr.3)

fa3 <- fa(hrMatrix_3, n.obs = 301, nfactors = 5, rotate = "oblimin")
psych::print.psych(fa3, sort = "TRUE", cut = .3)

# Model 4 ----
# new correlation matrix
hr.4 <- hr %>% 
  select(-c(q3,q7,q29,q44))

hrMatrix_4 <- cor(hr.4, use = "na.or.complete")

fa.parallel(hr.4)

fa4 <- fa(hrMatrix_4, n.obs = 301, nfactors = 5, rotate = "oblimin")
psych::print.psych(fa4, sort = "TRUE", cut = .3)

# Model 5 ----
# new correlation matrix
hr.5 <- hr %>% 
  select(-c(q3,q7,q29,q44,q11))

hrMatrix_5 <- cor(hr.5, use = "na.or.complete")

fa.parallel(hrMatrix_5, n.obs = 301, n.iter = 500)

fa5 <- fa(hrMatrix_5, n.obs = 301, nfactors = 5, rotate = "oblimin")
psych::print.psych(fa5, sort = "TRUE", cut = .3)


