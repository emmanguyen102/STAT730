##### Ex1: Suppose that the posterior distribution [p|X] is
##### Beta(6,5) for the unknown proportion of defective items 
##### produced by a firm

## a. Draw 10000 variables from the posterior distribution
## and make a histogram. Compare it with true probability
## density function

set.seed(456)

# 10,000 samples from a Beta(6,5) distribution
x <- rbeta(10000, 6, 5)

# plot a empirical distribution
hist(x, prob=T)

# compare to true density function
p.vec <- seq(0, 1, length =100)
lines(p.vec, dbeta(p.vec, 6, 5))


## b. Using the variables drawn in i),  plot the empirical 
## distribution of the odds to approximate the true density. 
## ( Use the option 'nclass=30' for better plot)

odds.sim = x/(1-x)
hist(odds.sim, prob=T, nclass=20)


## c. Find the approximate expectation and standard 
## deviation of the odds

# expectation 
mean(odds.sim)

# standard deviation 
sd(odds.sim)


## d. Approxiamte the probability that odds is greater than,
## or qual to 1. Compare it with the approxiamte probability
## taht p is gretaer than, or equal to .5
mean(odds >= 1)
mean(x >= 0.5)

mean(odds >= 1) - mean(x >= 0.5)


## e. Using appropriate quantiles, give the approximate
## 90% quantile intervals for odds
quantile(odds.sim, c(.05, .95))



##### Ex2: Performances of Monte Carlo estimation:

## a. Compute the probability: P(Z<=3) - p(Z<1), where
## Z ~ N(0,1)
pnorm(3, 0, 1) - pnorm(1, 0, 1)


## b. The above probability can be expressed as an integral
## with a standard normal probability density function where
## limits are 1 to 3. Generate 10,000 values from the 
## standard normal distribution and approximate the integral
## by the Monte Carlo Method.
b.sim <- rnorm(10000, 0, 1)

mean(b.sim <= 3 & b.sim >= 1)

# Evaluate the quality of the approximation (using the 
# sample variance)
var(b.sim <= 3 & b.sim >= 1)


## c. Draw 10,000 uniform variables and approximate the 
## integral (probability) by the Monte Carlo method.
## Evaluate the quality of the approximation
c.sim <- runif(10000, 1, 3)

mean ((2/sqrt(2*pi))*exp(-c.sim^2/2))

var((2/sqrt(2*pi))*exp(-c.sim^2/2))



##### Ex3:

## a. Draw 100,000 realizations from a uniform(0,1)
a <- runif(100000, 0, 1)

# Then use the probability integral transform, approximate
# the Exponential(2) density function. Compare it with
# the true probability density function
a.3.sim <- -2*log(1-a)
x=seq(0,10, length=100)
hist(sim.u, prob=T)
lines(x, dexp(x, 0.5),type= "l")

## b. Consider the integral: Int(from 0 to infinity):
## x^2*sin(pi*x)*exp(-x/2)*dx
## Draw 100,000 values from Exp(2)
## and approximate the value of the integral above

b <- rexp(100000, 1/2)
mean(2*b^2*sin(pi*b))

## c. Approximate the integral using the uniform variables
## drawn in a.
mean(2*a.3.sim^2*sin(pi*a.3.sim))


##### Ex4: Find the normalizing constant

## a. using the exponential(0.5) random variable
pdf <- function(y){
  (2+ sin(y)*sin(y))*exp(-(3+ cos(y)*cos(y)*cos(y))*y)
}
  
x.sim.1 <- rexp(10000,2) 

den.1 = function(x){
  2*exp(-2*x)
}

1/mean(den.1(x.sim.1)/ pdf(x.sim.1))

## b. Using the exponential (10) random variable

x.sim.2 <- rexp(10000,1/10) 

den.2 = function(x){
  (1/10)*exp(-x/10)
}

1/mean(den.2(x.sim.2)/ pdf(x.sim.2))


##### Ex5: 
## Let X be a random variable with the following
## probability density function:
## f(x) = 2/5*(2+cos(x)) * exp(-x)

## a. Use an exponential distribution with mean 1
## as an instrumental distribution: g(x) = exp(-x )
## find an appropriate bound first

f <- function(x){
  (2/5)*(2+cos(x))*exp(-x)
} 

g <- function(x){
  exp(-x)
}

M <- f(1)/g(1)

## b. Write a function that simulates N independent
## realizations from X by using accept-reject sampling 
## method

ar <- function(n){
  samp<-rep(NA,length=n)
  
  for (i in 1:n){
    bool=T
    
    while (bool == T){
      
      x=rexp(1)
      u=runif(1)
      ratio=f(x)/(M*g(x)) 
      
      if ( u <= ratio){
        bool=F
        samp[i]=x
      }
    }
  }
  
  samp
}

hist(ar(10000), nclass=100, prob=T)
curve(f,0,8,add=T)

