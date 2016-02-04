########################################
# An example of Rejection sampling
# author: Alberto Lumbreras
########################################

# A gamma distribution
dugly2 <- function(x) {
  dgamma(x, shape=20)
}

# A mixture of gammas
dugly <- function(x) {
  0.5*dgamma(x, shape=15) + 0.5*dgamma(x, shape=30)
}

# Proposal distribution 
#(the pretty one from which we how how to sample from)
rproposal <- function(){
  rnorm(1, 22, 12)
}
dproposal <- function(x){
  dnorm(x, 22, 12)
}

# Scale factor to make the proposal be always above the target
k <- 4

# Plot the ugly distribution
x <- seq(-10,75, by=0.1)
plot(x, dugly(x), type='l', lwd=3, ylim=c(0,.15))
lines(x, dproposal(x), col='red', lwd=2)
lines(x, k*dproposal(x), col='red', lwd=3, lty=2)
title("Rejection Sampling")

# Rejection sampler
###########################
nsamples <-100000
samples <- rep(NA,nsamples)
for(i in 1:nsamples){
  candidate <- rproposal()
  # accept with probabity (1/k)*p(candidate)/q(candidate)
  # k factor guarantees that there are no areas where samples are 
  # automatically accepted (p/q > 1)
  if (runif(1)*(k*dproposal(candidate)) < dugly(candidate)){
    samples[i] <- candidate
  }
}

hist(samples, breaks=100, add=TRUE, probability = TRUE)