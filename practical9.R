load("/Users/charliejones/desktop/miss_data/example1.Rdata")

fit1 <- lm(y~x1 + x2, data = example1)
summary(fit1)
plot(fit1$res ~ fit1$fitted.values)
qqnorm(fit1$res)
qqline(fit1$res)

library(MASS)
boxcox(fit1)

ytrans <- log(example1$y)
example1a <- example1
example1a[,3] <- ytrans
names(example1a)[3] <- "logy"

fit2 <- lm(logy~x1 + x2, data = example1a)
plot(fit2$res ~ fit2$fitted.values)
qqnorm(fit2$res)
qqline(fit2$res)

#must be careful and make sure imputations are not bad
#otherwise the entire analysis is not going to be accurate
library(mice)
example1.imp <- mice(example1, method = "norm")
example1.imp.fit <- with(example1.imp, lm(log(y) ~ x1 + x2))
example1.imp.pool <- pool(example1.imp.fit)
#aim for a value around (-+)10 in the intecept
summary(example1.imp.pool)
#with(complete(example.imp)) and such
load("/Users/charliejones/desktop/miss_data/example2.Rdata")

fit2 <- lm(y~x1 + x2, data = example2)
summary(fit2)
plot(fit2$res ~ fit1$fitted.values)
qqnorm(fit2$res)
qqline(fit2$res)

library(MASS)
boxcox(fit2)


