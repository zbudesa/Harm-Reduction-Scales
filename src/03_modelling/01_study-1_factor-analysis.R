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

# Bartlett
cortest.bartlett(hrMatrix, n = 301) # 8619.69, p < .001
cortest.bartlett(hr) # 8275.10, p < .001

# KMO factor Adequacy
KMO(hr) 
# Overall MSA = 0.94
# Lowest item: q20 = .78
# Highest: q1, q14, q18, q19, q21, q34 = .97

# What is the determinant of the matrix?
det(hrMatrix) # 6.705847e-14
det(hrMatrix) > .00001 # No. Not the greatest

# Parallel Analysis
fa.parallel(hr) # 6 factors
fa.parallel(hr, cor = "poly") # 5 factors

# Very Simple Structure
vss(hr, n = length(hr), fm = "mle")

mod <- mirt::mirt(hr, itemtype = "graded")

library(mirt)

coef(mod, simplified = TRUE, IRTpars = TRUE)

itemfit(mod, na.rm = TRUE)


# Principal Components Analysis ----
pc1 <- principal(hrMatrix, nfactors = length(hrMatrix[,1]), rotate = "none")
pc1
plot(pc1$values, type = "b")

pc2 <- principal(hrMatrix, nfactors = 8, rotate = "none")
pc2
plot(pc2$values, type = "b")

# Inspect residuals
residuals <- factor.residuals(hrMatrix, pc2$loadings)
residuals <- as.matrix(residuals[upper.tri(residuals)])
large.resid <- abs(residuals) > .05
sum(large.resid) ; sum(large.resid)/nrow(residuals)

sqrt(mean(residuals^2))

hist(residuals)


## Initial Factor Analysis ----
fa1 <- fa(hr, nfactors = 7, rotate = "none")
psych::print.psych(fa1, sort = "TRUE", cut = .3)

# Inspect residuals
residuals <- factor.residuals(hrMatrix, fa1$loadings)
residuals <- as.matrix(residuals[upper.tri(residuals)])
large.resid <- abs(residuals) > .05
sum(large.resid) ; sum(large.resid)/nrow(residuals)

sqrt(mean(residuals^2))

hist(residuals)

# Now with the matrix
fa1matrix <- fa(hrMatrix, n.obs = 301, nfactors = 8, rotate = "none")
psych::print.psych(fa1matrix, sort = "TRUE", cut = .3)

# Inspect residuals
residuals <- factor.residuals(hrMatrix, fa1matrix$loadings)
residuals <- as.matrix(residuals[upper.tri(residuals)])
large.resid <- abs(residuals) > .05
sum(large.resid) ; sum(large.resid)/nrow(residuals)

sqrt(mean(residuals^2))

hist(residuals)


# Model 2 ----
fa2 <- fa(hrMatrix, n.obs = 301, nfactors = 8, rotate = "oblimin")
psych::print.psych(fa2, sort = "TRUE", cut = .3)


