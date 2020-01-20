
y=c(6.5, 5.4, 8.4, 6.2, 6.5, NA, 6.2, NA, 7.4, NA)
x=c(6.9, 4.5, 12.2, 5.3, 6.6, 2.6, 3.4, 11.0, 10.2, 9.7)
example=data.frame(y=y, x=x)

ex.reg = lm(y~x)
summary(ex.reg)
#regression imputation for the y values
yimp = ifelse(is.na(y), 4.65 + 0.287*x, y)
ex.imp = data.frame(y=yimp, x=x)

is.na(y)
#applying the coefficients to the x values to get the expected predictions
#these two lines compare the imputed predictions vs the actual predictions made by model
#we can tell from this ... that the estimates are wrong for the imputed model
4.65 + 0.287*x
yimp

#correlation and covariance are inflated if we used an imputation to generate the values
#more accurate to use imputation + noise
mean(yimp)
var(yimp)
cor(example)
cor(ex.imp) #imputed allows this to run more
cor(ex.imp$x,ex.imp$y)
cor(example$x,example$y) #it is missing for the values which were not imputed

#Ex 2
summary(ex.reg)$sigma^2

#stochastic imputation model based on results from the first model
#rnorm is used just to generate noise in the model
yimp2 = ifelse(is.na(y), 4.65 + 0.287*x + sqrt(0.147)*rnorm(10), y)
ex.imp2 = data.frame(y=yimp2, x=x)


#Ex 3
d = ifelse(is.na(y), 1, 0)
ystar = ifelse(is.na(y), 6.657, y)

dummy.reg = lm(x~ystar+d)

#Ex 4

library(mice)
library(lattice)
data(mammalsleep)
mammalsleep[,c(2:3,7:8)]=log(mammalsleep[,c(2:3,7:8)])
names(mammalsleep)=c("species", "lbw", "lbrw", "sws", "ps",
                     "ts", "lmls", "lgt", "pi", "sei", "odi")
M2=as.data.frame(mammalsleep[,2:9])
Mmi<- mice(M2, method = "norm",m=20)

xyplot(Mmi, lgt~ lbw,pch=1:2,col=1:2,main="norm")
xyplot(Mmi, lgt~ lbw|.imp,pch=1:2,col=1:2,main="norm")

Mpm<- mice(M2, method = "pmm",m=20)
xyplot(Mpm, lgt~ lbw,pch=1:2,col=1:2,main="pmm")

Mm<- mice(M2, method = "mean",m=20)
xyplot(Mm, lgt~ lbw,pch=1:2,col=1:2,main="mean")

MN<- mice(M2, method = "norm.nob",m=20,main="norm.obs")
xyplot(MN, lgt~ lbw,pch=1:2,col=1:2)

MP<- mice(M2, method = "norm.predict",m=20,main="norm.predict")
xyplot(MP, lgt~ lbw,pch=1:2,col=1:2)

MB<- mice(M2, method = "norm.boot",m=20)
xyplot(MB, lgt~ lbw,pch=1:2,col=1:2,main="norm.boot")

imp <- mice(M2, maxit=1,method="mean")

