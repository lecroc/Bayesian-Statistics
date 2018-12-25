##########

# Bayesian Modelling Project

setwd("C:/Coursera/Statistics with R/Bayesian-Statistics/Project")

# load libraries

library(dplyr)
library(ggplot2)
library(GGally)
library(statsr)
library(BAS)

# load data

load("movies.Rdata")

d1<-as.data.frame(movies)

str(d1)

dim(d1)

summary(d1)