# Sampling from easy distributions
# with the method of inversion
#
# author: Alberto Lumbreras
######################################
par(mfrow=c(1,1))

# demo 1: logistic distribution
# demo 2: exponential distribution
demo <- 1

# Logistic distribution
###########################
if(demo==1){
    
  # Plot density and cdf
  x <- seq(-10,10, by=0.1)
  plot(x, dlogis(x), type='l', lwd=3, col="black", ylim=c(0,1)) # density
  lines(x, plogis(x), lwd=3, col="red") # cdf
  title("Inverse method (Logistic distribution)")

  # Inverse CDF
  cdf_inv <- function(x, loc=0, scale=10){
    log(x/(1-x))
  }

}

# Exponential distribution
###########################
if(demo==2){
  
  # Plot density and cdf
  x <- seq(0,10, by=0.1)
  plot(x, dexp(x), type='l', lwd=3, col="black", ylim=c(0,1)) # density
  lines(x, pexp(x), lwd=3, col="red") # cdf
  title("Inverse method (Exponential distribution)")
  
  # Inverse CDF
  cdf_inv <- function(x){
    -log(1-x)
  }
}


# Sampling by the inverse method
#################################
nsamples <-100000
samples <- rep(NA,nsamples)
for (i in 1:nsamples){
  x <- runif(1)
  samples[i] <- cdf_inv(x)
}

# Plot samples
hist(samples, breaks=100, add=TRUE, probability = TRUE)

