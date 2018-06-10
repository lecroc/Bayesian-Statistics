# Frequentist approach inference for a proportion

# 40 women
# 20 get RU486
# 20 get standard treatment

# 4 pregnancies from RU486
# 16 from standard group

# H0 = standard treatment and RU486 are the same
# HA - RU486 is better

# Categorical distribution with two levels is binomial distribution

# r code

sum(dbinom(0:4, size=20, p=.5))

# probability of 0-4 pregnancies from sample size of 20 if it is effectiveness
# of both treatments is the same

# Reject H0 as data provide convincing evidence that RU486 is more effective


##############################


#  Bayesian approach to same question

# Set up models

# assume p to be .1 or .2 or .3 or .4 or .5 or .6 or .7 or .8 or .9

# Specify prior probabilities
# Equivalent to state of beleif at the time of the experiment ~ H0

# calculate P(data | model) for each model (likelyhood)

# P(data | model ) = P(k=4 | n=20, p)

# R code:

p<- seq(from=.1, to=.9, by=.1)
prior<-c(rep(.06,4), .52, rep(.06, 4))
likelyhood<-dbinom(4, size=20, prob=p)

likelyhood


# CAlculating posterior

numerator<-prior*likelyhood
denominator<-sum(numerator)
posterior<-numerator/denominator
sum(posterior)
posterior

# Calculate probability that RU486 is more effective than control
# Sum of posterieors where p<.5

options(scipen = 999)

posterior

prob<-sum(posterior[1:4])*100

# Probability that RU486 is more effective:

prob











