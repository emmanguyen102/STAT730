##### 1. Empirical discrete approximation to a probability distribution
## Empiricals for an Exponential distribution

set.seed(1345)

x <- rexp(10000,4)      # 10000 samples from a Exp(1/4) distribution: f(x)= 4 exp(-4x)  E[X]=1/4
hist(x, prob=T)         # plot of the empirical distribution

## Compare to the true density function

xrange <- seq( min(x), max(x), length=30 )  # a sequence covering the range of X
lines(xrange, dexp(xrange, 4))

mean(x)              #large sample mean
# [1] 0.2518447

var(x)                 #large sample variance
# [1] 0.06292401

median(x)           # sample median
# [1] 0.1765852
qexp(.5,4)           # 50% quantile
# [1] 0.1732868

mean( x<=1 )     # empirical prob that X<=1 (large sample prob)
# [1] 0.9815
pexp( 1,4)           # the exact probability
# [1] 0.98168

##### 2. Direct simulation

## Random samples from the Dirichlet distribution:

rdirichlet <- function(n,p) {
  mat <- matrix(NA, n,length(p))
   for (i in 1:length(p)) {
    mat[,i]<- rgamma(n,p[i],1);
   }
  mat <- mat/ apply(mat,1,sum);
  mat;
}
 
## Samples from the Posterior distribution for (theta1, theta2, theta3) given X:
thetas <- rdirichlet(10000, c(728,584,174) )

hist(thetas[,1], nclass=50)  # approximate to marginal posterior distribution: theta1 | X
hist(thetas[,2], nclass=50)  # approximate to marginal posterior distribution: theta2 | X

hist(thetas[,1]-thetas[,2], nclass=50) # approximate to posterior dist: theta1-theta2| X

quantile(thetas[,1]-thetas[,2] , c(.025,.975) )
#       2.5%      97.5%
# 0.04885152 0.14394429

##### 3. Order statistics

## Expected value and variance of the 5th order statistics from a sample of size 25 from standard
## normal distribution.

sim.n = matrix( rnorm(250000),  25, 10000 )
sim.ord= apply(sim.n, 2, sort)

mean(sim.ord[5, ])

##### 4. Tail probability of Cauchy distribution

## Int(2, infinity) 1/(pi (1+x^2)) dx

sim.1= rcauchy(10000)
sum(sim.1 > 2)/10000       # or,
mean(sim.1> 2)
# [1] 0.1435

## 1/2 -Int(0,2) 1/(pi (1+x^2)) dx

sim.2 = runif( 10000, 0, 2)
0.5- mean(2/ (pi*(1+sim.2^2) ) )
# [1] 0.1468432
var(2/ (pi*(1+sim.2^2) ) )
# [1] 0.02871329

##Int(0,1/2) y^{-2}/(pi (1+y^{-2}) dy

sim.3= runif(10000, 0, 0.5)
mean( sim.3^(-2) / (2*pi*(1+sim.3^(-2))  ) )
# [1] 0.14752
var( sim.3^(-2) / (2*pi*(1+sim.3^(-2))  ) )
# [1] 9.532248e-05


##### 5. Normalizing constant (Importance sampling)

## For X ~N(0,1), the normalizing constant is 1/sqrt(2 pi) = 0.3989423

x.sim= rnorm(10000,0,1)

den = function(x){
  exp(-x^2/2)
}

1/mean( den(x.sim)/ dnorm(x.sim,0,1) )

##### 6. Accept-Reject sampling

ar.norm <- function(n){
samp<-rep(NA,length=n)

  for (i in 1:n){
    bool=T

   while (bool == T){

     x=rcauchy(1)
     ratio=dnorm(x)/ ( dcauchy(x)* sqrt(2*pi/exp(1)) )

          if ( runif(1) <= ratio){
            bool=F
            samp[i]=x
           }# end of if
     }# end of while
  }# end of for

samp
}

hist( ar.norm(10000), nclass=100, prob=T)
curve(dnorm, -4, 4, add=T)
