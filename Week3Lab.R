######

# Week 3 Lab

# Insall packages

library(PairedData)
library(tidyverse)
library(statsr)


# Load data

data(nc)

# EDA

str(nc)

# Question 1

# 7 categorical variables

# Question 2

# slight left skew

hist(nc$weight)

###

# Filter to full term babies only

nc_fullterm<-filter(nc, premie=="full term")

hist(nc_fullterm$weight)

###

# Bayesian inference on mean weight of full-term babies

bayes_inference(y=weight, data=nc_fullterm,
                statistic = "mean", type="ci",
                prior_family = "JZS", mu_0=7.7,
                rscale=1, method = "simulation",
                cred_level=.95)


# get bayes factor

bayes_inference(y=weight, data=nc_fullterm,
                statistic = "mean", type="ht", alternative="twosided",
                prior_family = "JZS", mu_0=7.7,
                rscale=1, method = "theo",
                cred_level=.95, show_plot = F)

############

# Prediction using MCMC

weight_post<-bayes_inference(y=weight, data=nc_fullterm,
                             statistic = "mean", type="ci",
                             prior_family = "JZS", mu_0=7.7,
                             rscale = 1, method = "simulation",
                             cred_level = .95)

# convert samples to data frame

samples<-as.data.frame(weight_post$samples)
nsim<-nrow(samples)
samples<-mutate(samples, y_pred=rnorm(nsim, mu, sqrt(sig2)))

# smoothed plot of simulated y_pred

ggplot(data=samples, aes(x=y_pred))+
  geom_histogram(aes(y=..density..), bins=100)+
  geom_density() +
  xlab(expression(y[new]))

select(samples, mu, y_pred) %>%
  map(quantile, probs=c(.025, .5, .975))

### Predict same for premies

nc_premie<-filter(nc, premie=="premie")

weight_post_premie<-bayes_inference(y=weight, data=nc_premie,
                             statistic = "mean", type="ci",
                             prior_family = "JZS", mu_0=7.7,
                             rscale = 1, method = "simulation",
                             cred_level = .95)

samples_premie<-as.data.frame(weight_post_premie$samples)
nsim<-nrow(samples_premie)
samples_premie<-mutate(samples_premie, y_pred=rnorm(nsim, mu, sqrt(sig2)))

select(samples_premie, mu, y_pred) %>%
  map(quantile, probs=c(.025, .5, .975))

#########

# side by side boxplots of weight for smokers and non-smokers

ggplot(nc, aes(x=habit, y=weight))+
  geom_boxplot()

#######

# Evaluate means of weight for smokers and non-smokers

nc_fullterm %>%
  group_by(habit) %>%
  summarise(mean_weight = mean(weight))


# bayesian inference on weights

bayes_inference(y=weight, x=habit, data=nc_fullterm,
                statistic = "mean", type="ht", alternative="twosided",
                null=0, prior="JZS", rscale=1, method = "theo", show_plot = F)


###

# shift prior

bayes_inference(y=weight, x=habit, data=nc_fullterm,
                statistic = "mean", type="ht", alternative="twosided",
                null=0, hypothesis_prior=c(.25, .75),prior="JZS", rscale=1, method = "theo", show_plot = F)


# shift rscale

bayes_inference(y=weight, x=habit, data=nc_fullterm,
                statistic = "mean", type="ht", alternative="twosided",
                null=0, prior="JZS", rscale=sqrt(2)/22, method = "theo", show_plot = F)


####

# Credible interval for mean birthweight smoker vs. non-smoker

bayes_inference(y=weight, x=habit, data=nc_fullterm,
                statistic = "mean", type="ci", mu_0=0, 
                prior="JZS", rscale=1, method = "simulation", show_plot = F)


