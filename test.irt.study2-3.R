library(mirt)
library(tidyverse)

items <- df %>% 
  select(q7, q1, q6, q24, q14, q18, q19, q13, q21,
         q42, q36, q33, q35, q38, q34, q41)

model <- "F1 = 1-9
          F2 = 10-16"

mod1 <- mirt(items[1:9], model = 1, type = "graded")

summary(mod1)

plot(mod1, type = "infotrace")

mod1.short <- mirt(items1[c(1:3,6,7,9)], model = 1, type = "graded")

summary(mod1.short, rotate = oblimin)
plot(mod1.short, rotate = "oblimin")

plot(mod1.short, type = "infotrace")

coef(mod1.short, IRTpars = TRUE, simplify = TRUE)

mod2 <- mirt(items[c(10:12)], model = 1, type = "graded")

summary(mod2)

plot(mod2, type = "infotrace")

cor(fscores(mod1), fscores(mod2), use = "na.or.complete")

library(mokken)

items1 <- items[1:9]

aisp(twoway(data.frame(items1)), lowerbound = c(.3, .35, .4, .45, .5, .55, .6), search = "ga")

items2 <- items[10:16]

aisp(twoway(data.frame(items2)), lowerbound = c(.3, .35, .4, .45, .5, .55, .6), search = "ga")


items3 <- items[c(1:3,5:9,11:12,14)]

psych::alpha(items3)
mod3 <- mirt(items3, model = 1, type = "graded")
coef(mod3, IRTpars = TRUE, simplify = TRUE)

items4 <- items[c(1:3,5:9)]

psych::alpha(items4)
mod4 <- mirt(items4, model = 1, type = "graded")
coef(mod4, IRTpars = TRUE, simplify = TRUE)

cor(fscores(mod3), fscores(mod4), use = "na.or.complete")

View(data.frame(fscores(mod3), fscores(mod4)))

sd(fscores(mod3), na.rm = TRUE)
sd(fscores(mod4), na.rm = TRUE)


tibble(mean1 = rowMeans(items1 - mean(rowMeans(items1), na.rm = TRUE)), 
       mean2 = rowMeans(items2 - mean(rowMeans(items2), na.rm = TRUE)),
       mean.total = rowMeans(items3 - mean(rowMeans(items3), na.rm = TRUE)),
       mean.total.short = rowMeans(items4 - mean(rowMeans(items4), na.rm = TRUE)),
       scale1 = fscores(mod1), scale2 = fscores(mod2),
        total = fscores(mod3), total.short = fscores(mod4)) %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(-id) %>% 
  
  ggplot() +
  aes(x = value, y = name) +
  geom_jitter()










