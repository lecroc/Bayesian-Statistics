#### Week 1 Lab

# Packages

library(magrittr)
library(statsr)


data1 = data.frame(machine=c(1L, 1L), outcome=c("W", "W"))
data2 = data.frame(machine=c(2L, 2L, 2L), outcome=c("L", "W", "W"))
bandit_posterior(data1) %>% bandit_posterior(data2, prior=.)

data1 = data.frame(machine=c(2L, 2L, 2L), outcome=c("W", "W", "L"))
data2 = data.frame(machine=c(1L, 1L), outcome=c("W", "W"))
bandit_posterior(data1) %>% bandit_posterior(data2, prior=.)