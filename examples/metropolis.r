#########################################
# A toy example of Metropolis- Hastings 
# (a Markov Chain Monte Carlo method)
# author: Alberto Lumbreras
#########################################
library(mvtnorm) # multivariate normal
library(coda) # MCMC checks
par(mfrow=c(1,1))

# demo 1: A mixture of Gammas (unidimensional)
# demo 2: A mixture of Gaussians (bi-dimensional)
demo <- 2

if(demo==1){
  
  # Target distribution
  dugly <- function(x) {
    0.5*dgamma(x, shape=15) + 0.5*dgamma(x, shape=30)    
  }
  
  x <- seq(-10,75, by=0.1)
  plot(x, dugly(x), type='l', lwd=3)
  title("Metropolis-Hastings samplings") 


  # Proposal distribution 
  # (propose jumps to another state in the Markov Chain)
  rproposal <- function(jump){
    rnorm(1, 0, jump)
  }
  
  # Metropolis-Hastings
  ###########################
  nsamples <-10000
  samples <- matrix(NA,nsamples, 1)
  jump <- 5
  samples[1] <- 200 # init
  for (i in 2:nsamples){
    candidate <- samples[i-1] + rproposal(jump) # hence the "Markov Chain"!
    
    # take it or leave it !
    p.acceptance <- dugly(candidate)/dugly(samples[i-1])
    if (runif(1) > p.acceptance){
      samples[i] <- samples[i-1]
    }
    else{
      samples[i] <- candidate
    }
  }

  hist(samples, breaks=100, add=TRUE, probability = TRUE)
  
  # The Coda package allows to plot results, 
  # perform convergence checks, etc
  plot(mcmc(samples))
}

# 2-D demo for the sake of beauty
# (and because we will be able to visalize the )
if(demo==2){
  par(mfrow=c(1,1))
  
  # Target distribution
  dugly <- function(xy) {
    x <- xy[1]
    y <- xy[2]
    covariance1 <- matrix(c(1, 0, 0, 100), nrow=2)
    covariance2 <- matrix(c(100,0, 0, 1), nrow=2)
    res <- (1/3)*mvtnorm::dmvnorm(c(x,y), mean = c(-5,0), sigma=covariance1) + 
           (1/3)*mvtnorm::dmvnorm(c(x,y), mean = c(+0,0), sigma=covariance2) +
           (1/3)*mvtnorm::dmvnorm(c(x,y), mean = c(+5,0), sigma=covariance1)
    res
  }
  
  # Plot target distribution
  x <- seq(-20, 20, length.out=100)
  y <- seq(-20, 20, length.out=100)
  z <- matrix(NA, nrow=100, ncol=100)
  i <- 1
  for (n in 1:length(x)){
    for(m in 1:length(y)){
      z[n,m] <- dugly(c(x[n],y[m]))
      i <- i+1
    }
  }
  contour(x,y,z)
  title("Metropolis-Hastings sampling") 
  
  # Proposal distribution 
  # (propose jumps to another state in the Markov Chain)
  rproposal <- function(jump){
    covariance <- matrix(c(1, 0, 0, 1), nrow=2)
    mvtnorm::rmvnorm(1, mean = c(0,0), sigma=covariance*jump)
  }
  
  # Metropolis-Hastings
  ###########################
  nsamples <- 10000
  samples <- matrix(NA,nsamples, 2)
  jump <- 20 # Play with it to get around a 20-30% of acceptance rate
  samples[1,] <-c(20,20) # init
  for (i in 2:nsamples){
    candidate <- samples[i-1,] + rproposal(jump) # hence the "Markov Chain"!
    
    # take it or leave it !
    p.acceptance <- dugly(candidate)/dugly(samples[i-1,])
    if (runif(1) > p.acceptance){
      samples[i,] <- samples[i-1,]
    }
    else{
      samples[i,] <- candidate
    }
  }
}

# Plot samples
lines(samples, col='yellow')
points(samples, col='red', pch=19, cex=0.2)

# The Coda package allow to plot results, 
# perform convergence checks, etc
plot(mcmc(samples))

# Check that the acceptance rate (after burn-in) is good (20-30%)
burn <- nsamples*0.5 # drop the first half of the chain
acceptance <- 100-mean(duplicated(samples[-(1:burn),]))*100
cat ("Acceptance ratio (recommended 30%):", acceptance, "%")