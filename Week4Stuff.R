#########

# Fit all possible bayesian regressions


library(BAS)
library(foreign)
library(MASS)

cog = read.dta("http://www.stat.columbia.edu/~gelman/arm/examples/child.iq/kidiq.dta")

m1<-bas.lm(kid_score~mom_hs+mom_iq+mom_work+mom_age, 
               prior="BIC", modelprior=uniform(), data=cog)

round(summary(cog_bas),4)

image(m1, rotate = F)

coef(m1)


#############

# Crime and punishment R demo

# load data
data(UScrime)

# log transform numeric variables

UScrime[,-2]<-log(UScrime[,-2])

mcrime<-bas.lm(y~., data=UScrime, prior="ZS-null",
               modelprior = uniform(), method = "MCMC")

diagnostics(mcrime)

plot(mcrime, which = 1)
plot(mcrime, which = 2)
plot(mcrime, which = 3)
plot(mcrime, which = 4)

image(mcrime, rotate = F)

# PO1 and PO2 correlated

cor(UScrime$Po1, UScrime$Po2)

coef<-coef(mcrime)

plot(coef, subset=5:6)


