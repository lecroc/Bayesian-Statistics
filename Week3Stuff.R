############################
# Week 3 Review

# Suppose we have a bag of 5 M&M's. The general percentage of yellow M&M's 
# in a bag is 10%,i.e., 0.1. Let X be the number of yellow M&M's in this bag. 
# What is the expected number of yellow M&M's we would have?

# E(X) = summ(k) * p(k=x)

p<-.1    # probablility k=x or of getting yellow M&M
size<-5     # number of M&Ms in bag
vals<-0:5   # possible number of yellow M&Ms in bag
probs<-dbinom(vals, size=size, prob=p)
EX<-sum(vals*probs)
EX

size*p

p<-.1    # probablility k=x or of getting yellow M&M
size<-10000     # number of M&Ms in bag
vals<-0:10000   # possible number of yellow M&Ms in bag
probs<-dbinom(vals, size=size, prob=p)
EX<-sum(vals*probs)
EX

size*p

#####################

x<-(1:15)
mu_0=4

t.test(x, alternative = "two.sided", mu=mu_0, conf.level = .95 )


#########################

# Tap Water example
# Normal / Gamma conjugate

library(statsr)

m_0<-35       # prior mean
n_0<-25       # prior sample size
s2_0<-156.25  # prior sd
v_0<-n_0-1    # prior degrees of freedom

data(tapwater)

Y<-tapwater$tthm

ybar<-mean(Y)
s2<-var(Y)
n<-length(Y)

n_n<-n_0+n
m_n<-(n*ybar + n_0*m_0)/n_n
v_n<-v_0+n
s2_n<-((n-1)*s2 +v_0*s2_0+n_0*n*(m_0-ybar)^2/n_n)/v_n

n_n
m_n
v_n
s2_n

# Confidence Interval

L<-qt(0.025, v_n)*sqrt(s2_n/n_n)+ m_n
U<-qt(0.975, v_n)*sqrt(s2_n/n_n)+ m_n

c(L,U)

#####################

# Monte Carlo example using tap water data posterior distribution

set.seed(8675309)

phi<-rgamma(1000, shape=v_n/2, rate=s2_n*v_n/2)

hist(phi)

sigma<-1/sqrt(phi)

mean(sigma)

quantile(sigma, c(.025, .975))


###################################

# Predictive Distributions and Prior Choice - using tapwater data

m_0<-(60+10)/2
s2_0<-((60-10)/4)^2
n_0<-2
v_0<-n_0-1
phi<-rgamma(1000, shape=v_0/2, rate=s2_0*v_0/2)
sigma<-1/sqrt(phi)
mu<-rnorm(1000, mean=m_0, sd=sigma/sqrt(n_0))
y<-rnorm(1000, mean=mu, sd=sigma)
quantile(y, c(.025, .975))

# quantiles return negative which is impossible for parts per billion, need to
# play with n_0 value to get reasonable numbers

m_0<-(60+10)/2
s2_0<-((60-10)/4)^2
n_0<-25
v_0<-n_0-1
phi<-rgamma(1000, shape=v_0/2, rate=s2_0*v_0/2)
sigma<-1/sqrt(phi)
mu<-rnorm(1000, mean=m_0, sd=sigma/sqrt(n_0))
y<-rnorm(1000, mean=mu, sd=sigma)
quantile(y, c(.025, .975))


######

# probablility y is negative in simulated data set

pyneg<-sum(y<0)/length(y)

pyneg


######

# predictive distribution of Yn+1 given Y1.......Yn

# use posterior hyper paramaters as new prior

phi<-rgamma(1000, shape=v_n/2, rate=s2_n*v_n/2)
hist(phi)
sigma<-1/sqrt(phi)
post_mu<-rnorm(1000, mean = m_n, sd=sigma/sqrt(n_n))
pred_y<-rnorm(1000, mean=post_mu, sd=sigma)
quantile(pred_y, c(.025, 0.975))

#######

# predict probability that ppb will exceed the legal limit of 80

pilgl<-sum(pred_y>80)/length(pred_y)
pilgl


##########################

# Reference priors

# Bayesian without priors

phi<-rgamma(10000, shape=(n-1)/2, rate=s2*(n-1)/2)
sigma<-1/sqrt(phi)
post_mu<-rnorm(10000, mean=ybar, sd=sigma/sqrt(n))
pred_y<-rnorm(10000, mean=post_mu, sd=sigma)

quantile(pred_y, c(.025, .975))


# probability ppb will exceed legal limit of 80

pilgl2<-sum(pred_y>80)/length(pred_y)
pilgl2


################################
  
# simmulating using MCMC Markov Chain Monte Carlo

# Cauchy distribution

# Gibbs sampler or Markov Chain Monte Carlo

tthmmodel<-bayes_inference(y=tthm, data=tapwater, statistic = "mean", mu_0=35,
                rscale=1, prior="JZS", type="ci", method = "sim")


  
post_mu<-rnorm(10000, mean=mean(tapwater$tthm), sd=tthmmodel$post_sd)
pred_y<-rnorm(10000, mean=post_mu, sd=sigma)

quantile(pred_y, c(.025, .975))

# probability of illegal ppb

pilgl3<-sum(pred_y>80)/length(pred_y)
pilgl3

###################################

# Bayesian test of the difference between two paired means using zinc concentration data

# Normal mean with known variance 

bayes_inference(difference, data=zinc, statistic="mean", type="ht",
                prior = "JZS",mu_0=0, method="theo", alt="twosided")



##############################

# Bayesian test of two means using weight gain data

# Comparing two paired means

# H1 = weight gain for younger mom and older mom the same
# H2 = weight gain for younger mom and older mom is not the same

data(nc)
bayes_inference(y=gained, x=mature, data=nc, type="ht",
                statistic="mean", alternative = "twosided",
                null=0, prior="JZS", r=1, method="theo", show_sum=T)


#################################

# Comparing two independent means 

bayes_inference(y=gained, x=mature, data = nc, type="ht",
                statistic = "mean", alternative = "twosided",
                null=0, prior="JZS", method="theo", show_summ = F)


######################3

# Comparing two means, what to report?

sm<-bayes_inference(y=weight, x=habit, data=nc, type="ht", null=0,
                    statistic = "mean", alternative = "twosided",
                    prior="JZS", r=.5, method = "sim", show_summ = F)


smci<-bayes_inference(y=weight, x=habit, data=nc, type="ci",
                      statistic = "mean", mu_0=0, prior="JZS",
                      r=.5, method = "sim", verbose = F)

print(smci$summary, digits=2)
                


               


