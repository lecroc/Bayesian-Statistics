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


# Calculating posterior

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


# same example with larger sample (40 subjects, 8 pregnancies)

p<- seq(from=.1, to=.9, by=.1)
prior<-c(rep(.06,4), .52, rep(.06, 4))
likelyhood<-dbinom(8, size=40, prob=p)

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


# Even bigger sample size

p<- seq(from=.1, to=.9, by=.1)
prior<-c(rep(.06,4), .52, rep(.06, 4))
likelyhood<-dbinom(40, size=200, prob=p)

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

#########################

# Practice quiz question 5


p<- c(.2, .4, .5, .6, .8)
prior<-c(rep(.125,2), .5, rep(.125, 2))
likelyhood<-dbinom(3, size=3, prob=p)

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


# Quiz question 3

# You go to Las Vegas and sit down at a slot machine. 
# You are told by a highly reliable source that, for each spin, 
# the probability of hitting the jackpot is either 1 in 1,000 or 
# 1 in 1,000,000, but you have no prior information to tell you 
# which of the two it is. You play ten times, but do not win the 
# jackpot. What is the posterior probability that the true odds of 
# hitting the jackpot are 1 in 1,000?


# 0.269
# 0.475
# 0.498
# 0.500

p<- c(1/1000, 1/1000000)
prior<-c(.5, .5)
likelyhood<-dbinom(0, size=10, prob=p)
likelyhood


# CAlculating posterior

numerator<-prior*likelyhood
denominator<-sum(numerator)
posterior<-numerator/denominator
sum(posterior)
posterior

# Question 7

#Hearing about your brilliant success in working with M&Ms, 
# Mars Inc. transfers you over to the Skittles department. 
# They recently have had complaints about tropical Skittles 
# being mixed in with original Skittles. You decide to conduct a
# frequentist analysis. If the findings suggest that more than 1% 
# of sampled supposedly original skittles are actually tropical, 
# you will recommend action to be taken and the production process 
# to be corrected. You will use a significance level of α=0.1. 
# You randomly sample 300 supposedly original skittles, and you find 
# that five of them are tropical. What should be the conclusion of your 
# hypothesis test? 

dbinom(5, size=300, p=.01)


# Question 8

# In the NFL, a professional American football league, there are 32 teams,
# of which 12 make the playoffs. In a typical season, 20 teams (the ones 
# that don’t make the playoffs) play 16 games, 4 teams play 17 games, 
# 6 teams play 18 games, and 2 teams play 19 games. At the beginning of 
# each game, a coin is flipped to determine who gets the football first. 
# You are told that an unknown team won ten of its coin flips last season. 
# Given this information, what is the posterior probability that the team 
# did not make the playoffs (i.e. played 16 games)?

games<-c(16,17,18,19)
p<-dbinom(10, size=games, p=.5)
prior<-c(20/32,4/32,6/32,2/32)

num<-p[1]*prior[1]
denom<-sum(p*prior)

# Question 9

# You are a professor and assign your TAs to type up a very important 
# homework assignment. You have three TAs: David makes an average of one 
# typo per page, Amy makes an average of two typos per page, and Joe makes 
# an average of three typos per page. A one-page typed homework assignment 
# is turned into your box that has ten typos! Assuming that typos follow a 
# Poisson distribution and you have no prior knowledge about which TA typed 
# the assignment, what is the posterior probability that the TA who typed 
# the homework assignment was Joe?

rate<-c(1,2,3)

p<-ppois(10, lambda = rate, lower.tail = F)

prior<-c(1/3, 1/3, 1/3)

num<-p[3]*prior[1]
denom<-sum(p*prior)

num/denom




