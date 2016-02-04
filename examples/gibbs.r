#########################################
# A toy example of Gibbs sampling
# author: Alberto Lumbreras
#########################################
library(mvtnorm)
library(coda)

# Parameters of a multivarite gaussian
mu1 <- 0
mu2 <- 0
cov12 <- 0.5 # same than cov21

# Plot the target distribution
dugly <- function(xy) {
  x <- xy[1]
  y <- xy[2]
  covariance <- matrix(c(1, cov12, cov12, 1), nrow=2)
  mvtnorm::dmvnorm(c(x,y), mean = c(mu1,mu2), sigma=covariance)
}
x <- seq(-3, 3, length.out=100)
y <- seq(-3, 3, length.out=100)
z <- matrix(NA, nrow=100, ncol=100)
i <- 1
for (n in 1:length(x)){
  for(m in 1:length(y)){
    z[n,m] <- dugly(c(x[n],y[m]))
    i <- i+1
  }
}
contour(x,y,z)
title("Gibbs sampling") 

# Gibbs sampler
nsamples <-1000
samples <- matrix(NA,nsamples, 2)
samples[1,] <-c(0,0) # init
for(i in 2:nsamples){
  # Sample x1 given last x2
  samples[i,1] <- rnorm(1, mu1 + cov12*(samples[i-1,2]-mu2), sqrt(1-cov12^2)) # easy!
  # Sample x2 given last x1
  samples[i,2] <- rnorm(1, mu2 + cov12*(samples[i,1]-mu1), sqrt(1-cov12^2)) # easy!
}

# Plot samples showing the unidimensional steps at each iteration
steps.x1 <- rep(samples[,1], each=2)
steps.x2 <- rep(samples[,2], each=2)
lines(steps.x1[1:(length(steps.x1)-1)], steps.x2[2:(length(steps.x2))], col="green")
lines(samples, col='yellow')
points(samples, col='red', pch=19, cex=0.5)

# Check the MCMC traces
plot(mcmc(samples))
