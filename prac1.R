#--------------
# for the interactive plots in Q3 and such, run these in R terminal
# There is a problem in R studio where the plots are not interactive 
#
#

#NMAR (not missing at random) - feature missing with __differing__ probabilities
#but somehow relates to other features
#e.g income is missing in a survey, but more women aged over 40 don't answer this
#or some describable mechanism

#MAR (missing at random) - missing with __same__ porbability on a (single feature),
#e.g missing for male participants at 0.4 and females at 0.35

#MCAR (missing completely at random) - missing with the _same_ probability for each individual
#regardless of male/female, age etc

#Practical 1
#Ex 1
height = c(106, 104, 110, 107, 98)
weight = c(18.9, 18.0, 19.5, 18.5, 17.8)
read = c(4.5, 5.5, 5, 4, 6)
children = data.frame(height=height, weight=weight, read=read)
summary(children)

height
weight
read

save(height, weight, read, children, file="exercise1.Rdata")

#Ex 2
mean(height)
median(height)
sd(height)
min(height)
max(height)
summary(height)

colMeans(children)
summary(children)




#Ex 3 (R just ignores the missing values when performing summary)
height = c(106, 104, 110, 107, 98, NA, NA, NA)
weight = c(18.9, 18.0, 19.5, 18.5, 17.8, NA, NA, NA)
read = c(4.5, 5.5, 5, 4, 6, 7, 4, 5.5)
children = data.frame(height=height, weight=weight, read=read)
summary(children)

#Ex 4
mother = c(158, NA, 163, NA, 156, 165, 149, NA, 154, 152, NA)
child = c(3.1, 4.2, 3.5, 2.9, 3.0, NA, 2.7, 4.1, 2.8, 2.9, 4.5)
day = c(274, 279, NA, NA, 270, 262, NA, NA, 266, 269, 277)
births = data.frame(mother=mother, child=child, day=day)

summary(births)

#multiple imputation through chained equations
library(mice)
md.pattern(births)

mean(mother)
mean(child)

mean(mother, na.rm=TRUE)
mean(child, na.rm=TRUE)

na.omit(mother)
mean(na.omit(mother))

#Pairwise: missing point not used, but rest for this row is
#listwise: entire point/row not used
mean(mother,na.rm=TRUE)
mean(na.omit(mother))
colMeans(births)
?colMeans

#doing nothing
colMeans(births)
#pairwise deletion
colMeans(births, na.rm=TRUE)
colMeans(na.omit(births))

#identity matrix
cor(births) 

#complete only -- listwise
cor(births, use="complete.obs")

#use available data
cor(births, use="pairwise.complete.obs")

#listwise again
cor(na.omit(births))

#Ex 2
library(mice)
data(mammalsleep)
?mammalsleep


#Ex 3
library(VIM)
#x11()
aggr(mammalsleep,numbers=TRUE)

md.pattern(sleep)
md.pairs(sleep)

#Ex 4
mammalsleep[,c(2:8)]=log(mammalsleep[,c(2:8)])
names(mammalsleep)=c("species", "lbw", "lbrw", "lsws", "lps",
                     "lts", "lmls", "lgt", "pi", "sei", "odi")
scattmatrixMiss(mammalsleep[,2:8])

scattMiss(mammalsleep[,c(7,8)],col=1:3)
scattMiss(mammalsleep[,c(8,7)],col=1:3)

marginplot(mammalsleep[,c(8,7)])

