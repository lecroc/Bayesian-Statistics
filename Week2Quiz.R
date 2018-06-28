# Week 2 Quiz

# 1 

# Which of the following statements is true of a probability mass 
# function but not a probability density function?
  
  
# The probability that a random variable X is equal to a specific value x can be greater than zero.


# 2

# Below are plots of the prior distribution for a parameter $\theta$ and the 
# likelihood as a function of $\theta$ based on 10 observed data points.

x<-seq(0,10,by=0.01)
prior<-dgamma(x,2,1)
likelihood<-dnorm(x,4,0.5)/1e6
posterior<-prior*likelihood
plot(x,likelihood,type="l",col="red",ylim=c(0,.000001))
lines(x,posterior,col="black")
lines(x,prior,col="blue")


# 3

# Which of the following distributions would be a good choice of prior to 
# use if you wanted to determine if a coin is fair when you have a strong 
# belief that the coin is biased towards heads? (Assume a model where we call
# heads a success and tails a failure).


Beta(90, 10)

# 4

# If John is trying to perform a Bayesian analysis to make inferences 
# about the proportion of defective electric toothbrushes, which of the 
# following distributions represents the a conjugate prior for the proportion p ?


# Beta


# 5

# You are hired as a data analyst by politician A. She wants to know the 
# proportion of people in Metrocity who favor her over politician B. 
# From previous poll numbers, you place a Beta(40,60) prior on the proportion. 
# From polling 200 randomly sampled people in Metrocity, you find that 103 people
# prefer politician A to politician B. What is the posterior probability that 
# the majority of people prefer politician A to politican B (i.e. P(p>0.5|data))?
  
1-pbeta(.5, 143, 157) 


# 6

# An engineer has just finished building a new production line for manufacturing 
# widgets. They have no idea how likely this process is to produce defective 
# widgets so they plan to run two separate runs of 15 widgets each. The first 
# run produces 3 defective widgets and the second 5 defective widgets.

# We represent our lack of apriori knowledge of the probability of producing a 
# defective widgets, p, using a flat, uninformative prior -Beta(1,1). 
# What should the posterior distribution of p be after the first run is finished? 
# And after the second?
  
  
# After the first run, Beta(4,13). After the second run, Beta(9,23).

# 7

# Suppose that the number of fish that Hans catches in an hour follows a Poisson 
# distribution with rate \lambdaλ. If the prior on \lambdaλ is Gamma(1,1) and 
# Hans catches no fish in five hours, what is the posterior distribution for 
# \lambdaλ?
  
  
# Gamma(k = 1, \theta = 1/6)Gamma(k=1,θ=1/6)


# 8

# Suppose that a miner finds a gold nugget and wants to know the weight of 
# the nugget in order to assess its value. The miner believes the nugget to 
# be roughly 200 grams, although she is uncertain about this quantity, so she 
# puts a standard deviation of 50 grams on her estimate. She weighs the nugget 
# on a scale which is known to weigh items with standard deviation 2 grams. 
# The scale measures the nugget at 149.3 grams. What distribution summarizes 
# the posterior beliefs of the miner?
  
  mu<-200 #prior mean
  tau<-50 #prior sd
  sigma<-2 # data sd
  xmean<-149.3 # data mean
  n<-1 # number of measurements
  
  muStar<-(mu*sigma^2+n*xmean*tau^2)/(sigma^2+n*tau^2) #posterior mean
  tauStar<-sqrt((sigma^2*tau^2)/(sigma^2+n*tau^2)) # posterior sd
  c(muStar,tauStar)
  
# 9

# A scientist is interested in estimating the average weight of male golden 
# hamsters. They decide to use a Bayesian approach to estimate \muμ by creating 
# a credible interval using a weakly informative prior. The posterior 
# distribution gives a 95% credible interval spanning 3.3 - 4.0 oz. 
# According to this model, what is the probability that \muμ does not fall 
# within this range?

  
# 5%
  
# 10

# Suppose you are given a coin and told that the die is either biased towards 
# heads (p = 0.75 ) or biased towards tails (p = 0.25 ). Since you have no prior
# knowledge abou the bias of the coin, you place a prior probability of 0.5 on 
# the outcome that the coin is biased towards heads. You flip the coin twice and 
# it comes up tails both times. What is the posterior probability that your next 
# flip will be heads?
    
    
ph1<-dbinom(0, 2, .75)
ph2<-dbinom(0, 2, .25)
denom<-ph1+ph2
posth<-ph1/denom
postt<-1-posth
postprobhead<-(.75*posth)+(.25*postt)
postprobhead


#10 a

# Suppose you are given a die and told that the die is either fair or is loaded 
# (it always comes up as a 6). Since most dice are not loaded, you place a prior 
# probability of 0.8 on the outcome that the die is fair. You roll a die and it 
# comes up as a 6. What is the posterior probability that your next roll will 
# also be a 6?
  
pstar<-(1/6)*(8/10)/((1/6)*(8/10)+1*2/10) #post probability that coin is fair
p6post<-pstar*1/6+(1-pstar)*1
p6post