##########

# Bayesian Modelling Project

setwd("C:/Coursera/Statistics with R/Bayesian-Statistics/Project")

# load libraries

library(dplyr)
library(ggplot2)
library(GGally)
library(statsr)
library(BAS)
library(knitr)

# load data

load("movies.Rdata")

d1<-as.data.frame(movies)

dim(d1)

###### Winnow to appropriate variables

d2<-d1 %>% 
  select(title_type, genre, runtime, mpaa_rating, thtr_rel_year,
         thtr_rel_month, imdb_rating, imdb_num_votes, critics_score,
         best_pic_nom, best_pic_win, best_actor_win, best_actress_win,
         best_dir_win, top200_box, audience_score) %>%
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

# View(check)

########

# select final data set for EDA, Modelling

movdat<-d2 %>%
  select(audience_score, feature_film, drama, runtime, mpaa_rating_R, thtr_rel_year,
         oscar_season, summer_season, imdb_rating, imdb_num_votes,
         critics_score, best_pic_nom, best_pic_win, best_actor_win,
         best_actress_win, best_dir_win, top200_box)

# View(movdat)

# Check for n/as - remove any we find

na<-sum(is.na(movdat))
na
cc<-complete.cases(movdat)
movdat<-movdat[cc,]
na<-sum(is.na(movdat))
na
dim(movdat)

# 1 na record removed

#########

# prep data set for EDA

# Evaluate numeric variables

# summary statistics

edan<-select_if(movdat, is.numeric)

s1<-summary(edan$audience_score)
s2<-summary(edan$runtime)
s3<-summary(edan$thtr_rel_year)
s4<-summary(edan$imdb_rating)
s5<-summary(edan$imdb_num_votes)
s6<-summary(edan$critics_score)

stats<-names(edan)

sumtable<-as.data.frame(rbind(s1,s2,s3,s4,s5,s6))
sumtable<-as.data.frame(cbind(stats, sumtable))

sumtable<-sumtable %>%
  mutate(Skew=ifelse(Mean>Median, "Right", "Left"))

View(sumtable)

#######

# plot correlation matrix

p1<-ggpairs(edan)+
  theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))

p1

####

# Evaluate new variables

eda<-movdat %>%
  select(audience_score, feature_film, drama,
         mpaa_rating_R, oscar_season, summer_season)

# Feature Film

eda1<-eda %>%
  group_by(feature_film) %>%
  summarise(n(), min(audience_score), quantile(audience_score, .25), 
            median(audience_score), mean(audience_score),
            quantile(audience_score, .75), max(audience_score))

names(eda1)<-c("FeatureFilm", "Count", "Min", "1st Qu.", "Median", "Mean",
               "3rd Qu.", "Max")
eda1<-mutate(eda1, Skew=ifelse(Mean>Median, "Right", "Left"))

# View(eda1)

p2<-ggplot(eda, aes(x=factor(feature_film), y=audience_score, fill=factor(feature_film)))+
  geom_boxplot()+
  theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))+
  ggtitle("Audience Score Box Plot")+
  labs(x="Feature Film?", y="Audience Score")

p2

mu2<-eda %>%
  group_by(feature_film) %>%
  summarise(Mean=mean(audience_score))

mu2

p2a<-ggplot(eda, aes(x=audience_score, color=feature_film, fill=feature_film))+
  geom_density()+facet_grid(feature_film~.)+
  geom_vline(data=mu2, aes(xintercept=Mean),
             linetype="dashed")+
  theme(legend.position = "right", axis.text.x = element_text(angle = 60,hjust = 1))+
  ggtitle("Audience Score Distribution")+
  labs(x="Audience Score", y="Density")

p2a

# Drama

eda2<-eda %>%
  group_by(drama) %>%
  summarise(n(), min(audience_score), quantile(audience_score, .25), 
            median(audience_score), mean(audience_score),
            quantile(audience_score, .75), max(audience_score))

names(eda2)<-c("Drama", "Count", "Min", "1st Qu.", "Median", "Mean",
               "3rd Qu.", "Max")
eda2<-mutate(eda2, Skew=ifelse(Mean>Median, "Right", "Left"))

View(eda2)

p3<-ggplot(eda, aes(x=factor(drama), y=audience_score, fill=factor(drama)))+
  geom_boxplot()+
  theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))+
  ggtitle("Audience Score Box Plot")+
  labs(x="Drama?", y="Audience Score")

p3

mu3<-eda %>%
  group_by(drama) %>%
  summarise(Mean=mean(audience_score))

mu3

p3a<-ggplot(eda, aes(x=audience_score, color=drama, fill=drama))+
  geom_density()+facet_grid(drama~.)+
  geom_vline(data=mu3, aes(xintercept=Mean),
             linetype="dashed")+
  theme(legend.position = "right", axis.text.x = element_text(angle = 60,hjust = 1))+
  ggtitle("Audience Score Distribution")+
  labs(x="Audience Score", y="Density")

p3a

# R Rating

eda3<-eda %>%
  group_by(mpaa_rating_R) %>%
  summarise(n(), min(audience_score), quantile(audience_score, .25), 
            median(audience_score), mean(audience_score),
            quantile(audience_score, .75), max(audience_score))

names(eda3)<-c("R Rating", "Count", "Min", "1st Qu.", "Median", "Mean",
               "3rd Qu.", "Max")
eda3<-mutate(eda3, Skew=ifelse(Mean>Median, "Right", "Left"))

View(eda3)

p4<-ggplot(eda, aes(x=factor(mpaa_rating_R), y=audience_score, fill=factor(mpaa_rating_R)))+
  geom_boxplot()+
  theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))+
  ggtitle("Audience Score Box Plot")+
  labs(x="Rated R?", y="Audience Score")

p4

mu4<-eda %>%
  group_by(mpaa_rating_R) %>%
  summarise(Mean=mean(audience_score))

mu4

p4a<-ggplot(eda, aes(x=audience_score, color=mpaa_rating_R, fill=mpaa_rating_R))+
  geom_density()+facet_grid(mpaa_rating_R~.)+
  geom_vline(data=mu4, aes(xintercept=Mean),
             linetype="dashed")+
  theme(legend.position = "right", axis.text.x = element_text(angle = 60,hjust = 1))+
  ggtitle("Audience Score Distribution")+
  labs(x="Audience Score", y="Density")

p4a

# Oscar Season

eda4<-eda %>%
  group_by(oscar_season) %>%
  summarise(n(), min(audience_score), quantile(audience_score, .25), 
            median(audience_score), mean(audience_score),
            quantile(audience_score, .75), max(audience_score))

names(eda4)<-c("Oscar Season", "Count", "Min", "1st Qu.", "Median", "Mean",
               "3rd Qu.", "Max")
eda4<-mutate(eda4, Skew=ifelse(Mean>Median, "Right", "Left"))

View(eda4)

p5<-ggplot(eda, aes(x=factor(oscar_season), y=audience_score, fill=factor(oscar_season)))+
  geom_boxplot()+
  theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))+
  ggtitle("Audience Score Box Plot")+
  labs(x="Released during Oscar Season?", y="Audience Score")

p5

mu5<-eda %>%
  group_by(oscar_season) %>%
  summarise(Mean=mean(audience_score))

mu5

p5a<-ggplot(eda, aes(x=audience_score, color=oscar_season, fill=oscar_season))+
  geom_density()+facet_grid(oscar_season~.)+
  geom_vline(data=mu5, aes(xintercept=Mean),
             linetype="dashed")+
  theme(legend.position = "right", axis.text.x = element_text(angle = 60,hjust = 1))+
  ggtitle("Audience Score Distribution")+
  labs(x="Audience Score", y="Density")

p5a

# Summer Season

eda5<-eda %>%
  group_by(summer_season) %>%
  summarise(n(), min(audience_score), quantile(audience_score, .25), 
            median(audience_score), mean(audience_score),
            quantile(audience_score, .75), max(audience_score))

names(eda5)<-c("Summer Release", "Count", "Min", "1st Qu.", "Median", "Mean",
               "3rd Qu.", "Max")
eda5<-mutate(eda5, Skew=ifelse(Mean>Median, "Right", "Left"))

View(eda5)

p6<-ggplot(eda, aes(x=factor(summer_season), y=audience_score, fill=factor(summer_season)))+
  geom_boxplot()+
  theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))+
  ggtitle("Audience Score Box Plot")+
  labs(x="Summer Release?", y="Audience Score")

p6

mu6<-eda %>%
  group_by(summer_season) %>%
  summarise(Mean=mean(audience_score))

mu6

p6a<-ggplot(eda, aes(x=audience_score, color=summer_season, fill=summer_season))+
  geom_density()+facet_grid(summer_season~.)+
  geom_vline(data=mu6, aes(xintercept=Mean),
             linetype="dashed")+
  theme(legend.position = "right", axis.text.x = element_text(angle = 60,hjust = 1))+
  ggtitle("Audience Score Distribution")+
  labs(x="Audience Score", y="Density")

p6a
