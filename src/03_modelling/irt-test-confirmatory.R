library(mirt)

model <-  'F1 = 1-9
           F2 = 10-16' 

mirt.model(model)

mod <- mirt(cbind(scale1,scale2),
            model = mirt.model(model), 
            itemtype = "graded")

summary(mod, rotate = "oblimin")

coef(mod, IRTpars = TRUE, simplify = TRUE)

plot(mod, type = "trace")
