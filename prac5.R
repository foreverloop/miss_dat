#library(BRugs)
library(rjags) #mac OS X compatible MCMC software
library(mice)
library(coda)

data(mammalsleep)
mammalsleep[,c(2:3,7:8)]=log(mammalsleep[,c(2:3,7:8)])
names(mammalsleep)=c("species", "lbw", "lbrw", "sws", "ps",
                     "ts", "lmls", "lgt", "pi", "sei", "odi")

M2=as.data.frame(mammalsleep[,2:9])

x=M2$lbw
y=M2$ps
z=M2$lbrw
N=length(x)

#n.chains is why there is a black and red output
#since black is one chain, red is another, this can be used to check consistency of the results
model0 <- jags.model("/Users/charliejones/desktop/miss_data/bmodel0.txt", data=list(N=N,X=x,Y=y,Z=z), 
           #inits=list("b","sigma","I","X","Y","sigma2"),
           n.chains = 2)

jsam <- jags.samples(model0,variable.names =  c("b","sigma","Y"), n.iter = 10000)
cod_params <-c("b","sigma","Y")

samps <- coda.samples( model0, cod_params, n.iter=10000)
#par("mar")
par(mar=c(1,1,1,1))
plot(samps)
summary(samps)
# OK Above



BRugsFit("bmodel0.txt", data=list(N=N,X=x,Y=y,Z=z),
         para=c("b","sigma","Y"),numChains =2)

S=samplesStats("*")
S
S[,4:6]

samplesHistory("b")
# plot the chain and check convergence
samplesDensity("b")



#Ex 1b
x=M2$lmls[-13]
y=M2$lgt[-13]
z=M2$lbrw[-13]
N=length(x)
#?jags.model

#jags.model("bmodel2.txt", data=list(N=N,X=x,Y=y,Z=z), 
#                  inits=c("b","sigma","I","X","Y","sigma2"),
#                  n.chains = 2)

model2 <- jags.model("/Users/charliejones/desktop/miss_data/bmodel2.txt", data=list(N=N,X=x,Y=y,Z=z), 
                     #inits=list("b","sigma","I","X","Y","sigma2"),
                     n.chains = 2)

jsam <- jags.samples(model2,variable.names =  c("b","sigma","I","X","Y","sigma2"), n.iter = 10000)
cod_params <-c("b","sigma","I","X","Y","sigma2")

samps <- coda.samples( model1, cod_params, n.iter=10000)
#par("mar")
par(mar=c(1,1,1,1))
plot(samps)

BRugsFit("bmodel2.txt", data=list(N=N,X=x,Y=y,Z=z),
         para=c("b","sigma","I","X","Y","sigma2"),numChains =2)


S=samplesStats("*")
S
S[,4:6]
samplesHistory("b")
# plot the chain and check convergence
samplesDensity("b")
samplesDensity("I")


#Example 2
y = c(6.5, 5.4, 8.4, 6.2, 6.5, NA, 6.2, NA, 7.4, NA)
x = c(6.9, 4.5, 12.2, 5.3, 6.6, 2.6, 3.4, 11.0, 10.2, 9.7)
example = data.frame(y=y, x=x)
example2 = as.matrix(example)
library(norm)
s = prelim.norm(example2)
thetahat = em.norm(s)
rngseed(1234567)
theta = da.norm(s,thetahat,steps=1000,return.ymis=TRUE)
theta$ymis
getparam.norm(s,theta$parameter)

#Example 3
library(mice)
#m is the number of chains to generate
ex.imp = mice(example, m=2, method="norm", maxit=500)
ex.imp$imp
with(ex.imp, summary(y))
plot(ex.imp)
