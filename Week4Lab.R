#######

# Week 4 lab

library(tidyverse)
library(dplyr)
library(statsr)
library(MASS)
library(BAS)
library(ggplot2)
library(broom)

# load data

data(wage)

# Q1 Answer = Observational Study

# set seed
set.seed(5150)

#######

# EDA

ggplot(data=wage, aes(x=wage))+
  geom_histogram(binwidth = 100)

summary(wage$wage)

# Q2 answer = 10 of the respondents make strictly less than 300 dollars per week
# Actual count is 6

poor<-filter(wage, wage<300)
nrow(poor)

#####

# look at wage vs. iq

ggplot(data=wage, aes(x=iq, y=wage))+    # basic scatterplot
  geom_point()

m_iq<-lm(wage~iq, data=wage)     # run linear model

tidy(m_iq)                   # model summary

ggplot(data=wage, aes(x=iq, y=wage))+      # scatterplot with regression line
  geom_point()+
  stat_smooth(method = "lm", se=T)

confint(m_iq)


# look at wage vs. educ

ggplot(data=wage, aes(x=educ, y=wage))+
  geom_point()

medu<-lm(wage~educ, data=wage)

tidy(medu)

ggplot(data=wage, aes(x=educ, y=wage))+
  geom_point()+stat_smooth(method = "lm", se=T)

# Question 3 Answer - For each additional year of education, there is a 95% chance that the
#                     average weekly wages will increase by $40.04 to $71.39

confint(medu)

#### Model diagnostics

medu_aug<-augment(medu)  # augment creates a data frame with variables and predicted values

View(medu_aug)

ggplot(data=medu_aug, aes(x=.fitted, y=.resid))+                    # ressidual vs. fitted plot
  geom_point(alpha=.6)+geom_hline(yintercept=0, linetype="dashed")+
  labs(x="Fitted", y="Residuals")

ggplot(data=medu_aug, aes(x=.resid))+
  geom_histogram(binwidth = 100)+xlab("Residuals")     # histogram of residuals

ggplot(data=medu_aug)+geom_qq(aes(sample=.std.resid))+
  geom_abline(slope = 1, intercept = 0, linetype="dashed")+
  labs(x="Theoretical quantiles", y="Standardized Residuals")    # qq plot 

## Answer to question 4

# The residuals are strongly left skewed, hence the normal distribution of errors contition
# is not met.

# The above statement is false, the residuals are right skewed

####

# log transform dependant variable to get rid of skew

miq<-lm(lwage~iq, data = wage)

tidy(miq)

plot(miq)

#######

# look for outliers

outl<-Bayes.outlier(miq, k=3)

outl_df<-data.frame(probability=outl$prob.outlier,
                    case=1:length(outl$prob.outlier))

ggplot(outl_df, aes(ymax=probability, x=case))+
  geom_linerange(ymin=0)+
  labs(y="Probability")

outl1_df_1<-outl_df %>%
  filter(probability>.5)

outl1_df_1

# Answer to question 5

# Case 616 has a probability of close to 1 that is is an outlier under the normal error
# model for regressing lwage on iq

# this statement is false .66 is not close to 1

# with large n, there can be a high probability of at least one error > 3 std

# Probabilit of a case being an outlier if below or above 3 sd from 0

(prob_outlier<-pnorm(-3) + pnorm(3, lower.tail = F))

# probability of a single case not being an outlier is complement

(prob_not_outlier<-1-prob_outlier)

# probability of no outliers in sample n

n<-nrow(wage)

(prob_no_outliers<-prob_not_outlier^n)

# probability of at least one outler in the sample is compliment

(prob_outlier_n<-1-prob_no_outliers)


# what does k need to be?

n<-nrow(wage)
(prob_obs_not_outlier<-.95^(1/n))

(newk<-qnorm(.5+.5*prob_obs_not_outlier))

# can also use Bayes.outlier function

outl2<-Bayes.outlier(miq, prior.prob = .95)

outl2_df<-data.frame(probability=outl2$prob.outlier,
                     case=1:length(outl2$prob.outlier))

outl2_df %>%
  filter(probability>.5)


# Answer to question 6 is case 784

####### Multiple linear regression / model selection

wmod<-lm(lwage~.-wage, data = wage)

summary(wmod)

# Answer to question 7 = married black man

BIC(wmod)

wmod1<-lm(lwage~.-wage-brthord, data=wage)
BIC1<-BIC(wmod1)

wmod2<-lm(lwage~.-wage-sibs, data=wage)
BIC2<-BIC(wmod2)

wmod3<-lm(lwage~.-wage-feduc, data=wage)
BIC3<-BIC(wmod3)

wmod4<-lm(lwage~.-wage-meduc, data=wage)
BIC4<-BIC(wmod4)

BIC1 # birthord
BIC2 # sibs
BIC3 # feduc
BIC4 # meduc

# Answer to question 8 

# removing feduc lowers BIC most

## Use stepAIC to select model

wage2<-na.omit(wage)  # drop N/A

n<-nrow(wage2)    # record new number of obs

wmod5<-lm(lwage~.-wage, data=wage2)   # fit new model on all variables

stepAIC(wmod5, direction="backward", k=log(n))  # use backwards stepAIC with BIC k (log(n))


#########

# BMA Bayesian Model Averaging

# use wage2 df as it has omitted N/As

bma_lwage<-bas.lm(lwage~.-wage, data=wage2,
                  prior="BIC", modelprior = uniform())

bma_lwage

summary(bma_lwage)

### look at coefficients

coef_bma_lwage<-coefficients(bma_lwage)

plot(coef_bma_lwage, subset=c(3,13), ask=F)


# 95% credible intervals for coefficients

confint(coef_bma_lwage)


########

# Reduced model analysis

## reduced data set

# use base r indexing to select columns (select not working for some reason)

wage3<-wage2[,c(2,3,4,5,6,7,8,9,10,11,12,17)]

# bayesian model averaging with zellner-siow prior on regression coefficients
  
bma_lwage_2<-bas.lm(lwage~., data=wage3,
                    prior = "ZS-null", modelprior = uniform())

bma_lwage_2

# Answer question 9

# lowest marginal posterior inclusion probability is age

summary(bma_lwage_2)

# Answer to question 10

# the naive model can't have a posterior probability of >.5 
# as the best model's posterior is .0805

coeff_lwage_2<-coefficients(bma_lwage_2)

plot(coeff_lwage_2, subset = c(8), ask = F)

### Re-inflating logged variables

ci_urban<- coef(bma_lwage_2) %>%
  confint(parm="urban1") %>%
  exp()

(ci_urban -1)*100


#########

#  Prediction with BAS

### Best Predictive Model (BPM)

BPM_pred_lwage<-predict(bma_lwage, estimator="BPM", se.fit = T)

variable.names(BPM_pred_lwage)

### Highest Probability Model

HPM_pred_lwage<-predict(bma_lwage, estimator="HPM")

variable.names(HPM_pred_lwage)

### Median Probability Model

MPM_pred_lwage<-predict(bma_lwage, estimator = "MPM")

variable.names(MPM_pred_lwage)

# Answer 11 - meduc, urban, married


#### Find highest predicted wage

opt<-which.max(BPM_pred_lwage$fit)

wage2 %>%
  slice(opt) %>%
  glimpse()

## 95% credible interval

ci_lwage<-confint(BPM_pred_lwage, parm="pred")

ci_lwage[opt,]  # log 95% credible interval for highest weekly wage in data set

exp(ci_lwage[opt,])  # re-inflated to actual dollars

### 95% prediction interval for BPM

BPM_pred_lwage<-predict(bma_lwage, estimator="BPM", se.fit=T)

ci_bpm_lwage<-confint(BPM_pred_lwage, estimator="BPM")

opt_bpm<-which.max(BPM_pred_lwage$fit)

exp(ci_bpm_lwage[opt_bpm,])


# Answer to question 12 is 782, 3154











