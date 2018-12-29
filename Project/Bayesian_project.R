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

dim(d1)

###### Winnow to appropriate variables

d2<-d1 %>% 
  select(title_type, genre, runtime, mpaa_rating, thtr_rel_year,
         thtr_rel_month, imdb_rating, imdb_num_votes, critics_score,
         best_pic_nom, best_pic_win, best_actor_win, best_actress_win,
         best_dir_win, top200_box) %>%
  mutate(feature_film=ifelse(title_type=="Feature Film", "yes", "no")) %>%
  mutate(drama=ifelse(genre=="Drama", "yes", "no")) %>%
  mutate(mpaa_rating_R=ifelse(mpaa_rating=="R", "yes", "no")) %>%
  mutate(oscar_season=ifelse(thtr_rel_month>9, "yes", "no")) %>%
  mutate(summer_season=ifelse(thtr_rel_month>4 & thtr_rel_month<9, "yes", "no"))

#####

# Create a subset table to confirm the manipulations worked correctly

check<-d2 %>%
  select(thtr_rel_month, oscar_season, summer_season) %>%
  arrange(thtr_rel_month)

View(check)

########

# select final data set for EDA, Modelling

movdat<-d2 %>%
  select(feature_film, drama, runtime, mpaa_rating_R, thtr_rel_year,
         oscar_season, summer_season, imdb_rating, imdb_num_votes,
         critics_score, best_pic_nom, best_pic_win, best_actor_win,
         best_actress_win, best_dir_win, top200_box)

View(movdat)


  

 
  






