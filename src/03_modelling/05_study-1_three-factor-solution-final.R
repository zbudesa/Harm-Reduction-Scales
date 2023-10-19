

hr.ld <-
  df  %>% # N = 301
  select(starts_with("q"), -Q_RecaptchaScore) %>% 
  select(-c(q4:q5, q25, q40,
            q31, q8, q24, q45, 
            q22, q32))


mod <- fa(hr.ld, n.obs = 301, nfactors = 5, rotate = "promax", cor = "poly")
psych::print.psych(mod, sort = "TRUE", cut = .3)

mod1 <- fa(hr.ld %>% 
             # Factor 1 + some
             select(-c(q1, q3, q6, q7, q13, q14, q16, q18, q19, q21,q39, q35, q34,
                       # Now remove
                       q15, q43, q44, q26, q2, q10, q23, q41,
                       q9, q27, q30,q12,q28,q20
                       )),
           nfactors = 1, rotate = "oblimin", cor = "poly")
psych::print.psych(mod1, sort = "TRUE", cut = .3)

mod2 <- fa(hr.ld %>% 
             select(c(
               # Factor 1
               q1, q6, q7, q13, q14, q16, q18, q19, q21,q39, 
                # Factor 2
               q33, q36, q38, q41, q42,
               # Factor 3
               q9, q2, q20, q23, q28, q29, q12, q10,
               
               # Try out some final items
               q2, q3
               
             )),
           nfactors = 3, rotate = "oblimin", cor = "poly")
psych::print.psych(mod2, sort = "TRUE", cut = .3)

psych::print.psych(
  fa(df %>% select(c(
                          # Factor 1
                          q1, q6, q7, q14, q16, q18, q19, q21, q39, q13,
                          # Factor 2
                          q33, q36, q38, q42, #q35, q41, 
                          # Factor 3
                          q9, q2, q20, q23, q29, q12, q10, q28, q32,
                          
                          # Try out some final items
                         
                          
                          # Skip:
                          # q4:q5, q25, q40,
                          # q31, q8, q24, q45, 
                          # q22, q32
                          # Tried: 2, 3, 11, 15, 17, 26, 27, 29, q31, 32, 34, 35, 39, 40, 43
                          
                          # Worth considering: 
                          #  - ADD Q35!!!
                          #  - item 15 instead of 13
                          #  - item 27 instead of 28; 29 instead of 28; or 27 & 29 instead of 28
                          #  - Factor 2: q9, q2, q23, q29, q12, q10, q29, q31 - slightly better alpha
                          #  - Factor 2: q9, q2, q20, q23, q29, q12, q10, q28, q32
                          #  - Factor 2: q9, q2, q20, q23, q29, q12, q10, q28, q32 -- Much better alpha
                          #  - Consider removing q41
                          #  - q
                          
                          
                        )),
                      nfactors = 3, rotate = "oblimin", cor = "poly"), 
  sort = "TRUE", cut = .3)


psych::alpha(df %>% 
               select(q9, q2, q20, q23, q29, q12, q10, q28, q32),
             check.keys = TRUE)


library(GPArotation)

d <- EFA.dimensions::POLYCHORIC_R(df %>% select(c(
  # Factor 1
  q1, q6, q7, q14, q16, q18, q19, q21, q39, q13,
  # Factor 2
  q33, q36, q38, q42, #q35, q41, 
  # Factor 3
  q9, q2, q20, q23, q29, q12, q10, q28, q32)))

library(EFAtools)

hr.ld <-
  hr %>% 
  select(-c(q4:q5, q25, q40,
            q31, q8, q24, q45, 
            q22, q29))

mod_avg <- EFA_AVERAGE(hr.ld, n_factors = 5, rotation = "oblimin",
                       method = "ML")

mod_avg$fit_indices

mod_avg$loadings$average


mod_avg <- 
  EFA_AVERAGE(hr.ld %>% 
                select(
                  -c(q44, q27, q43, q35,
                     q37,  q11, q30,  #q28,
                     q26, q3, q17, q13)
                ), 
              n_factors = 3, rotation = "oblimin",
              method = "ML")

mod_avg$fit_indices

mod_avg$loadings$average

f1 <- df %>% 
  select(q1,q6,q7,q14,q16,q18,q19,q21,q39)
f2 <- df %>% 
  select(q15,q33,q34,q36,q38,q41,q42)
f3 <- df %>% 
  select(q2, q9,q10,q12,q20,q23,q28,q32)

psych::alpha(f1, check.keys = TRUE)
psych::alpha(f2, check.keys = TRUE)
psych::alpha(f3, check.keys = TRUE)
# It's the same solution!










