#Ex 0
chol2 <- c(270, 236, 210, 142, 280, 272, 160, 220, 226, 242, 186, 266, 206,
           318, 294, 282, 234, 224, 276, 282, 360,  310, 280, 278, 288, 288, 244, 236)
chol4 <- c(218, 234, 214, 116, 200, 276, 146, 182, 238, 288, 190, 236, 244,
           258, 240, 294, 220, 200, 220, 186, 352, 202, 218, 248, 278, 248, 270, 242)
chol14 <- c(156, NA, 242, NA, NA, 256, 142, 216, 248, NA, 168, 236, NA, 200,
            264, NA, 264, NA, 188, 182, 294, 214, NA, 198, NA, 256, 280, 204)
chol = data.frame(chol2=chol2, chol4=chol4, chol14=chol14)
cholB = as.matrix(chol)
chol.imp = mice(chol, method="norm", maxit=500, seed=935)
chol.imp$imp
with(chol.imp, summary(chol14))
with(chol.imp, cor(cbind(chol2, chol4, chol14)))

#Ex 1
ex.imp = mice(example, m=3, method="norm")
lm.imp = with(ex.imp, lm(y~x))
lm.imp
summary(lm.imp)
pool(lm.imp)
summary(pool(lm.imp))

#Ex 2
iq = c(107, 100, 104, 85, 90, 99, 122, 115, 112, 91, 83, 
       78, 81, 70, 77, 94, 118, 89, 87, 110)
pre = c(51, 60, 33, 52, NA, 45, 81, 75, 57, 43, 41, 47, 
        28, NA, 36, 63, 54, 58, 45, 66)
post = c(66, 72, 75, NA, NA, 72, 93, 78, 69, NA, 52, 56, 59, 
         NA, 56, 73, NA, 61, 54, NA)
ex2 = data.frame(iq=iq, pre=pre, post=post)

pred.mat = matrix(c(0,0,0,
                    1,0,0,
                    1,1,0), byrow=TRUE, nrow=3, ncol=3)


ex2.imp = mice(ex2, method="norm", predictorMatrix=pred.mat,m=10)
model1=with(ex2.imp, lm(post~iq+pre))
model1
summary(model1)
pool(model1)
summary(pool(model1))

#Defining a simple mode model2
model2=with(ex2.imp, lm(post~iq))
model2
summary(model2)
pool(model2)
summary(pool(model2))

#Using the Wald test to compare models
pool.compare(model1,model2,method="Wald")

#Exercise 3

ex3.imp = mice(ex2, method="pmm", predictorMatrix=pred.mat,m=10)

ex3.imp=with(ex3.imp, lm(post~pre+iq))
ex3.imp
summary(ex3.imp)
pool(ex3.imp)
summary(pool(ex3.imp))

#Exercise 4
oxygen = c(44.609, 45.313, 54.297, 59.571, 49.874, 44.811, NA, NA, 39.442, 60.055,
           50.541, 37.388, 44.754, 47.273, 51.855, 49.156, 40.836, 46.672, 46.774, 50.388,
           39.407, 46.08, 45.441, NA, 45.118, 39.203, 45.79, 50.545, 48.673, 47.92, 47.467)
run.time = c(11.37, 10.07, 8.65, NA, 9.22, 11.63, 11.95, 10.85, 13.08, 8.63, NA,
             14.03, 11.12, NA, 10.33, 8.95, 10.95, 10, 10.25, 10.08, 12.63, 11.17, 9.63, 8.92,
             11.08, 12.88, 10.47, 9.93, 9.4, 11.5, 10.5)
run.pulse = c(178, 185, 156, NA, NA, 176, 176, NA, 174, 170, NA, 186, 176, NA, 166, 
              180, 168, NA, NA, 168, 174, 156, 164, NA, NA, 168, 186, 148, 186, 170, 170)
fit = data.frame(oxygen=oxygen, run.time=run.time, run.pulse=run.pulse)

fit.imp = mice(fit, method="pmm")

#Exrecise 5
library(corrplot)
library(RColorBrewer)
library(VIM)
data(tao)
matrixplot(tao)
C=cor(tao,use="pairwise")
corrplot(C, type="upper", order="hclust") 

library(mice)
quickpred(tao,mincor=.3)
imp.tao=mice(tao,m=20)
fullmodel=with(imp.tao, lm(Humidity~factor(Year)+Latitude+Longitude+Sea.Surface.Temp+Air.Temp+UWind+VWind))
pool(fullmodel)
summary(pool(fullmodel))


#Exercise 6
library(missMDA)
data(snorena)
names(snorena)

#Make 20 imputations using default imputation methods used in  mice
imp.snore=mice(snorena,m=20)
fullmodel=with(imp.snore, glm(as.numeric(snore)~age+weight+sex+size+alcohol+tobacco))
pool(fullmodel)
summary(pool(fullmodel))

