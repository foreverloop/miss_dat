
#ex 1

library(mice)
data(mammalsleep)
?mammalsleep
#Species = nominal, body weight = continuous, brain weight = continuous
#Slow wave = continous, paradox = continuous, total sleep = continous
#max lifespan = continous, gestation time = continuous
#preditation index = ordinal, sleep exposure index = ordinal
#overall danger index = ordinal
View(mammalsleep)

library(VIM)
#x11()
aggr(mammalsleep,numbers=TRUE)
nrow(sleep)

#first column (non binary content) is the count frequency pattern, so 42 have nothing missing
#for example 9 have dreaming and non dreamining missing, 
#2 have lifespan and dream/non dream missing
#1 has total sleep and dreaming/non dreaming missing

#bottom row (non binary content) is the total number within feature missing
#e.g 12 dreaming points missing, 14 non dreaming points missing

#final column (non binary content) is the number of entries
#for this missingness pattern
md.pattern(sleep)
md.pattern(sleep,plot=FALSE)

?md.pairs
#shows where pairs exist, e.g non dreaming and dreaming share 12 pairs of missing data
md.pairs(sleep)

#Ex 2 

mammalsleep[,c(2:3,7:8)]=log(mammalsleep[,c(2:3,7:8)])
names(mammalsleep)=c("species", "lbw", "lbrw", "sws", "ps",
                     "ts", "lmls", "lgt", "pi", "sei", "odi")
#x11()

#will show clusters of missing data (red crosses), perhaps there is some pattern in this?
scattmatrixMiss(mammalsleep[,2:8])

#shows where the variable values for the missing lifespan data is
#also shows the distribution when the data is missing and when it is not
scattMiss(mammalsleep[,c(7,8)],col=1:3)

#shows the variable value for where the missing gestation data is
scattMiss(mammalsleep[,c(8,7)],col=1:3)

?marginplot
#missing data shown in red by the small boxplots?
marginplot(mammalsleep[,c(8,7)])

#unsure why
C=cor(mammalsleep[,2:11],use="pairwise",method="spearman")
round(C,3)

library(corrplot)
library(RColorBrewer)
corrplot(C, order="hclust") #type="upper"
corrplot(C,type="upper" ,order="hclust")

#or plotted in another way
corrplot.mixed(C,order="hclust")

#Ex 3
#select all rows, only for columns 2 till 11 (species not included)

matrixplot(mammalsleep[,2:11])

#flip through plots in the viewer. Unsure how we can determine how these are
#'causing' missingness however

for(i in 1:11){ #this needs to be cumulative, it is only ok for i == 1
  barMiss(mammalsleep[,i:11])
}


#Ex 4
data(tao)
?tao
View(tao)
names(tao)
matrixplot(tao)
aggr(tao)
md.pattern(tao)
md.pairs(tao)
scattmatrixMiss(tao[,1:8])
marginplot(tao[,c(5,6)])

