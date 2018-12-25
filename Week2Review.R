
# pmf = probability mass funcion (discrete)
# pdf = probability density function (continuous)
# cdf = cumulative distribution function (continuous gets area under the curve)




#############

# Binomial - discrete, pmf

p<-.5               # probability of success
n<-50               # number of trials
k<-25               # number of successes
pk<-p^k             # p to the power of k
mnp<-(1-p)^(n-k)    # 1-p to the power of n-k
nchk<-choose(n,k)   # n choose k

pk<-nchk*pk*mnp     # binomial function

pk1<-dbinom(25, 50, .5)   # dbinom() does this calc

pk
pk1

# Can do with a vector of k

k<-sample(1:50, 25, replace=T)

pk1<-dbinom(k, 50, .5)

pk1

######################

# Poisson - discrete, pmf

lambda<-1.5
k<-c(0,1)

fl<-(lambda^k/factorial(k))*exp(lambda*-1)

fl1<-dpois(k, lambda)

fl
fl1


###########

# Normal continuous, pdf, cdf

# parameters mu and sigma (mean and standard deviation)

mu<-0
sd<-.05
x<-.1

b<-(1/sqrt(2*pi*sd^2))*exp((-1/(2*sd^2))*((x-mu)^2)) # f of x for normal 

norm_val<-dnorm(.1, 0, .05)

norm_val

b


cdf_neg<-pnorm(-1, 0, 1, lower.tail = T)
cdf_neg

cdf_pos<-pnorm(1, 0, 1, lower.tail = T)
cdf_pos

cdf_pos-cdf_neg # probability that x is within 1 standard deviation of the mean



######################

# Beta - continuous, pdf, cdf

p<- .2
alpha<-3
beta<-5

fp<-((gamma(alpha+beta)/(gamma(alpha)*gamma(beta)))*p^(alpha-1))*(1-p)^(beta-1)

fp1<-dbeta(.2, 3,5)

fp
fp1

cdf_beta_neg<-pbeta(-.2,1,2)
cdf_beta_pos<-pbeta(.2,1,2)

cdf_beta_neg
cdf_beta_pos

pl.beta <- function(a,b, asp = if(isLim) 1, ylim = if(isLim) c(0,1.1)) {
  if(isLim <- a == 0 || b == 0 || a == Inf || b == Inf) {
    eps <- 1e-10
    x <- c(0, eps, (1:7)/16, 1/2+c(-eps,0,eps), (9:15)/16, 1-eps, 1)
  } else {
    x <- seq(0, 1, length = 1025)
  }
  fx <- cbind(dbeta(x, a,b), pbeta(x, a,b), qbeta(x, a,b))
  f <- fx; f[fx == Inf] <- 1e100
  matplot(x, f, ylab="", type="l", ylim=ylim, asp=asp,
          main = sprintf("[dpq]beta(x, a=%g, b=%g)", a,b))
  abline(0,1,     col="gray", lty=3)
  abline(h = 0:1, col="gray", lty=3)
  legend("top", paste0(c("d","p","q"), "beta(x, a,b)"),
         col=1:3, lty=1:3, bty = "n")
  invisible(cbind(x, fx))
}


########################


# Gamma

x<-1
k<-2
theta<-3

fgam<-1/(gamma(k)*theta^k)*x^(k-1)*exp(-x/theta)

fgam1<-dgamma(1,shape=2,scale=3)

fgam
fgam1



# Gamma 2

x<-1
alpha<-2
beta<-3

fgam2<-beta^alpha/gamma(alpha)*x^(alpha-1)*exp(-beta*x)

fgam2a<-dgamma(1, shape=2, rate=3)

fgam2
fgam2a













