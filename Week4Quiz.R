#######


# Week 4 Quiz

# Q1 In a Bayesian simple linear regression 
#     y \sim N(\alpha + x\beta, \sigma^2)y~N(a+xﬂ,s)
#     Suppose our priors on the parameters \alpha,\beta,\sigma^2a,ﬂ,s2
#     are independent and that the prior on \betaﬂ is N(0,1)N(0,1).

#     Then the posterior mean of \betaﬂ will be closer to zero than the 
#     least squares estimate. True or False?

#     True


# Q1A True or False: The mean and standard deviation of the posterior distribution 
#     of a slope or intercept parameter in Bayesian linear regression is equal to the 
#     least squares estimate and corresponding standard error if the reference prior 
#     is used and normally distributed errors are assumed.

#     True


# Q2 A simple linear model (either Bayesian or frequentist) that tries 
#    to predict an individual's height from his/her age is unlikely to perform well, 
#    since human growth rates are non-linear with regard to age. Specifically, humans
#    tend to grow quickly early in life, stop growing at through most of adulthood, 
#    and sometimes shrink somewhat when they get old. Which of the following 
#    modifications to a simple linear regression model should you prefer?

#    Including terms of age^2 and or \log(age)log(age) as covariates in the model.



# Q3 You fit a linear model on 1000 data points and identify a point that lies 
#    3 standard deviations above its predicted value. Should you worry about this 
#    potential outlier? Why or why not?

#    No, because the probability that all 1000 points will be within 3 standard 
#    deviations of their predicted values is 0.070.07, so it is unsurprising to 
#    observe a point 3 standard deviations away from its predicted value.


(prob_outlier<-pnorm(-3) + pnorm(3, lower.tail = F))

# probability of a single case not being an outlier is complement

(prob_not_outlier<-1-prob_outlier)


# probability of no outliers in sample n

n<-1000

(prob_no_outliers<-prob_not_outlier^n)

# probability of at least one outler in the sample is compliment

(prob_outlier_n<-1-prob_no_outliers)


# Q3A Suppose we want to set a level k such that if we observe a data point more 
#     than k standard deviations away from the mean, we deem it an outlier. If the 
#     number of observations is 1000, what is the probability that we observe an 
#     outlier at least 4 standard deviations away from its prediction value?

# prob of outlier at 4 sd

(prob_outlier<-pnorm(-4) + pnorm(4, lower.tail = F))

# probability of a single case not being an outlier is complement

(prob_not_outlier<-1-prob_outlier)


# probability of no outliers in sample n

n<-1000

(prob_no_outliers<-prob_not_outlier^n)

# probability of at least one outler in the sample is compliment

(prob_outlier_n<-1-prob_no_outliers)


# Q4 Suppose a researcher is using Bayesian multiple regression to quantify the 
#    effect of vitamin C on cancer patient mortality. The central 95% posterior 
#    credible interval of the coefficient of vitamin C dosage is (-0.19, -0.07). 
#    Assuming the model assumptions are valid, what can we say about the effect 
#    of vitamin C on cancer patient mortality?

#   The posterior probability that the coefficient of vitamin C is greater than 
#   zero is low, so there is a high posterior probability of a negative association 
#   between vitamin C and cancer patient mortality.


# Q5 Which of the following goes into the calculation of the Bayesian Information 
#    Criterion (BIC)?


#    The maximum value of the log-likelihood under the current model, the sample 
#    size, and the number of parameters in the model


# Q6 In a linear model with an intercept term (that is always included) and 3 potential 
#    predictors, how many possible models are there?

# 2^k possible models - k is number of predictors

2^3

# Q7 Suppose that a MCMC sampler is currently visiting model B. Model A has a higher 
#   posterior probability than model B and Model C has a lower posterior probability than 
#   model B. Which of the following statements is true in the MCMC algorithm?

#  If a jump to Model A is proposed, this jump is always accepted.

# Q8 Which of the following is not an assumption made in Bayesian multiple regression?

# The errors follow a t-distribution.


# Q9 Why is the Zellner gg-prior useful in Bayesian model averaging?

#    It simplifies prior elicitation down to two components, the prior mean and g

# Q10 When selecting a single model from an ensemble of models in the case of Bayesian 
#     model averaging, which of the following selection procedures corresponds to 
#     choosing the "highest probability model"?

#     Selecting the model with the highest posterior model probability.


