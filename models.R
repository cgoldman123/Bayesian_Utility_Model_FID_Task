library(proto)      # object-oriented support
library(magrittr)   # %>% pipe operator

# This model allows us to learn the "attack distance" of each predator based on noisy observations
# It learns as an ideal Bayesian observer would, accounting for both ucertainty in the current estimate and in observations.
bayes.player.known.variance <- proto(expr = {
    x <- NULL  # no data observed initially
    sigma.x <- 5.2 # data likelihood x ~ N(mu, sigma.x)
    mean.mu <- 40 # prior (or old posterior) on mu: mu ~ N(mean.mu, sd.mu)
    sd.mu <- 12
    # Get posterior
    Posterior <- function(.) {
        return(c(mean.mu = .$mean.mu, sd.mu = .$sd.mu))
    }
    # Set prior/previous posterior
    Set.Prior <- function(., mean.mu, sd.mu) {
        .$mean.mu <- mean.mu
        .$sd.mu <- sd.mu
    }
    # update posterior by observing new data
    Observe <- function(., x.new) {
        .$x <- c(.$x, x.new)
        n.new <- length(x.new)
        # update posterior on mu
        old.sd.mu <- .$sd.mu
        .$sd.mu <- 1 / sqrt(n.new / sigma.x^2 + 1 / old.sd.mu^2)
        .$mean.mu <- .$sd.mu^2 * (.$mean.mu / old.sd.mu^2 +
                                      sum(x.new) / sigma.x^2)
        return(c(mean.mu = .$mean.mu, sd.mu = .$sd.mu))
    }
    # CDF of posterior predictive of x, used to compute P(escape)
    # i.e. P(x_new < y | x observed)
    CDF.Posterior.Predictive <- function(., y) {
        return(pnorm(y, mean = .$mean.mu, sd = sqrt(.$sigma.x^2 + .$sd.mu^2)))
    }
    # probability of escape
    P.Escape <- function(., FID, a, b) {
        return(.$CDF.Posterior.Predictive(a * FID + b))
    }
    # sample from posterior predictive
    Sample.Posterior.Predictive <- function(., n = 1) {
        return(rnorm(n, mean = .$mean.mu, sd = sqrt(.$sigma.x^2 + .$sd.mu^2)))
    }
})