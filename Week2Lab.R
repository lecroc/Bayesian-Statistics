##### Week 2 Lab

# Packages
library(magrittr)
library(statsr)

data(brfss)

#1

# mean=10, var=5

std<- sqrt(5)

qnorm(c(0.025, 0.975), mean = 10, sd = 2.236)


#2

qbeta(c(0.05, 0.95), shape1 = 2, shape2 = 5)


#3

qgamma(c(0.005, 0.995), shape = 4, rate = 8)

#4

qbeta(c(0.025, 0.975), shape1 = 2587, shape2 = 2415)

# 6

qbeta(c(0.025, 0.975), shape1 = 3086, shape2 = 2914)

# 8

qbeta(c(0.025, 0.975), shape1 = 2591, shape2 = 2614)

#11

sum(brfss$fruit_per_day)
nrow(brfss)

#13

qgamma(c(0.05, 0.95), shape = 8115, rate = 5005)
