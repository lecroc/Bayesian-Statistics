###################



# Suppose in a bag of M&M's with 5 different colors: red, green, yellow, blue, 
# and orange. Suppose the general percentage of yellow M&M's in a bag is 10%, 
# i.e., the probability of getting a yellow M&M is 0.1. What would be the probability
# of getting 1 or more yellow M&M in a bag of 5 M&M's?

answer<-1-dbinom(0, 5, 0.1)
answer

# dbinom(0 successes out of 5 trials with .1 probability of success)
# getting 1 or more is same as getting 1, 2, 3, 4, or 5.  Only other option is 0.
# probability of 0 = 1- probability of (1,2,3,4,or5)
# probability of 1,2,3,4,or 5 = 1-probability of 0

p1<-dbinom(1,5,.1)
p2<-dbinom(2,5,.1)
p3<-dbinom(3,5,.1)
p4<-dbinom(4,5,.1)
p5<-dbinom(5,5,.1)

p1
p2
p3
p4
p5

answer2<-p1+p2+p3+p4+p5

answer2
answer

################

# In the 1980s, the U.S. Military provided the enzyme-linked immunosorbent assay 
# (ELISA) test to test for human immunodeficiency virus (HIV) among recruits. 
# The true positive (sensitivity) of the test was around 93%, while the true 
# negative (specificity) of the test was around 99%. It was estimated that 
# 1.48 / 1000 adult Americans were HIV positive. Suppose the result of each
# ELISA test is independent with each other.

# What is the probability of a recruit who has HIV getting 1 positive and 1 
# negative if they got 2 ELISA tests?

answer3<-dbinom(1,2,.93)
answer3


# What is the probability of a recruit getting 1 positive and 1 negative 
# ELISA results?


phiv<-1.48/1000
pnohiv<-1-phiv

answer4<-(dbinom(1,2,.93)*phiv)+(dbinom(1,2,.99)*pnohiv)
answer4



#############

# likelyhood from video 7

prior<-c(rep(.6,4), .52, rep(.6,4)) # not part of likelyhood calc

p<-seq(from =.1, to= .9, by=.1)
likelyhood<-dbinom(4,20,p)

# posterior from video 7

options(scipen=999) # get rid of scientific notation

num<-prior*likelyhood
den<-sum(num)
post<-num/den
post
sum(post)


#########################
# re-done with bigger sample

# video 8

prior<-c(rep(.6,4), .52, rep(.6,4)) # not part of likelyhood calc

p<-seq(from =.1, to= .9, by=.1)
likelyhood<-dbinom(8,40,p)

# posterior from video 7

options(scipen=999) # get rid of scientific notation

num<-prior*likelyhood
den<-sum(num)
post<-num/den
post
sum(post)

###########

# M&M example
prior<-c(.5,.5)
p<-c(.1,.2)
likelyhood<-dbinom(1,5,p)

num<-prior*likelyhood
den<-sum(num)
post<-num/den
post
sum(post)


######################

#Quix

# Of women ages 40 and over, 10 out of 1000 have breast cancer. A mammography 
# has 80% sensitivity (true positive rate) and a 90% specificity 
# (true negative rate). A woman walks into a clinic for a routine screening 
# (mammography) and tests positive for breast cancer. Now, what is the 
# probability that she has breast cancer?

pcan<-.01
pnocan<-.99

pcangpos<-.8
pcangneg<-.1

x<-pcangpos*pcan
y<-pnocan*pcangneg

x/(x+y)

# Hearing about your brilliant success in working with M&Ms, Mars Inc. transfers
# you over to the Skittles department. They recently have had complaints about
# tropical Skittles being mixed in with original Skittles. You decide to conduct
# a frequentist analysis. If the findings suggest than more than 1% of sampled 
# supposedly original Skittles are actually tropical you will recommend action 
# to be taken and the production process to be corrected. You will use a 
# significance level of \alpha = 0.1??=0.1. You randomly sample 300 supposedly 
# original skittles, and you find that five of them are tropical. What should 
# be the conclusion of your hypothesis test? 

tropical<-c(0,1,2,3,4)
tprob<-sum(dbinom(tropical, 300, .01))
1-tprob

# You go to Las Vegas and sit down at a slot machine. You are told by a highly
# reliable source that, for each spin, the probability of winning the jackpot
# is either 1 in 1,000 or 1 in 1,000,000, but you have no prior information 
# to tell you which of the two it is. You play ten times, but do not win the 
# jackpot. What is the posterior probability that the true odds of hitting the
# jackpot are 1 in 1,000?

prior<-c(.5,.5)
p<-c(1/1000, 1/1000000)
likelyhood<-dbinom(0,10,p)
num<-prior*likelyhood
den<-sum(num)
post<-num/den
post
sum(post)




