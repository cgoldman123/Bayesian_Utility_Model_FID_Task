source("models.R")

library(ggplot2)
library(data.table)
library(plyr)
library(gridExtra)

# read data


## data without reward/shock level
setwd("L://rsmith//lab-members//cgoldman//ironside_FID//LIBR_FID_scripts_CMG")
# data.DT <- data.table(read.csv("data/data.csv"))
##

data.DT <- data.table(read.csv("data/expanded_data.csv"))

data.DT <- data.DT[data.DT$subject!=17 & data.DT$subject!=20 & data.DT$subject!=28 & data.DT$subject!=14 & data.DT$subject!=29 & data.DT$subject!=27 & data.DT$subject!=26,]

#data.DT <- filter(data.DT,subject!=17 & subject!=20 & subject!=28 & subject!=14 & subject!=29 & subject!=27 & subject!=26 )

data.DT$subject <- as.numeric(data.DT$subject)

for (i in 1:1344){
 if (data.DT$subject[i]>13){
    data.DT$subject[i] = data.DT$subject[i] - 1
  }
}

data.DT <- data.DT[color %in% c(1,2,3)]   # discard control trials
data.DT[, subject := as.factor(subject)]
data.DT[, color := factor(c("fast", "mid", "slow")[color],
                             levels = c("fast", "mid", "slow"))]
if (is.null(data.DT$reward.level)) {
    data.DT$reward.level <- 1
    data.DT$shock.level <- 1
}
# data.DT <- data.DT[FID > 0, ]  # discard those FID <= 0
setkey(data.DT, subject, trial, color)

# plot
fig.1 <- ggplot(data.DT, aes(x = trial, y = AD, color = color)) +
    geom_line(size = 1, alpha = .5) +
    facet_wrap( ~ subject) +
    ggtitle("AD ~ trial by subject")
print(fig.1)

fig.2 <- ggplot(data.DT, aes(x = trial, y = FID, color = color)) +
    geom_line(size = 1, alpha = .5) +
    facet_wrap( ~ subject) +
    ggtitle("FID ~ trial by subject")
print(fig.2)

fig.3 <- ggplot(data.DT, aes(x = AD, y = FID, color = color)) +
    geom_point(size = 1, alpha = .5) +
    facet_wrap( ~ subject) +
    ggtitle("FID ~ AD by subject")
print(fig.3)

# estimate p(escape under FID) for each (subject, color, trial)
# p(escape) = p( AD < a * FID + b)
V.predator.fast <- 26
V.predator.slow <- 26/7
V.subject.run <- 1
L <- 5

a <- V.predator.fast / (V.predator.fast - V.predator.slow)
b <- L * a * (1 - V.predator.slow / V.subject.run)

# Bayesian player model
likelihood.sigma.x <- 5.2
prior.mean.mu <- 50
prior.sd.mu <- 30

# sequentially process trials for each (subject, color)

# estimate mean and variance of AD for each color from data
AD.estimates <- data.DT[subject == "1",  # AD sequence the same for all subjects
                        .(mu = mean(AD), sigma = sd(AD)), by = color]

.ProcessTrialsSequential <- function(FID, AD, trial) {
    stopifnot(!is.unsorted(trial))
    stopifnot(length(FID) == length(AD), length(trial) == length(AD))
    # a new object
    # TODO: use estimate of sigma here. replace with known variance^(1/2)
    this.bayes <- bayes.player.known.variance$proto(x = NULL,
                                    sigma.x = likelihood.sigma.x)
    this.bayes$Set.Prior(prior.mean.mu, prior.sd.mu)
    # sequentially observe
    return(ldply(1:length(trial), .fun = function(i) {
        # P(escape)
        p.escape = this.bayes$P.Escape(FID[i], a = a, b = b)
        # observe and update belief
        this.bayes$Observe(AD[i])
        #
        return(c(p.escape = p.escape,
                 posterior.AD.mean = this.bayes$mean.mu,
                 posterior.AD.sd = this.bayes$sd.mu,
                 trial = trial[i]))
    }))
}

results.DT <- data.DT[, as.list(.ProcessTrialsSequential(FID, AD, trial)),
                      by = .(subject, color)]
setkey(results.DT, subject, trial, color)
results.DT <- merge(data.DT, results.DT)

# plot results
fig <- ggplot(results.DT, aes(x = trial, y = p.escape, color = color)) +
    geom_point(size = 1, alpha = .5) +
    facet_wrap( ~ subject) +
    ggtitle("P(escape) by subject")
print(fig)

fig <- ggplot(results.DT, aes(x = trial, y = p.escape, color = color)) +
    geom_point(size = 1, alpha = .5) +
    facet_wrap( ~ color) +
    ggtitle("P(escape) by color")
print(fig)

fig <- ggplot(results.DT[subject == "1",], aes(x = trial, y = posterior.AD.mean,
                              color = color)) +
    geom_point(size = 0.8, alpha = 0.8) +
    geom_linerange(aes(ymin = posterior.AD.mean - 2 * posterior.AD.sd,
                        ymax = posterior.AD.mean + 2 * posterior.AD.sd )) +
    geom_hline(aes(yintercept = mu, color = color),
               data = AD.estimates, linetype = "dashed") +
  scale_colour_manual(values = c("dodgerblue2","orange", "red")) +
    facet_wrap( ~ color, scales = "free_y") +
    theme_bw()
print(fig + ggtitle("Bayesian estimates for AD (95% C.I.)"))
print(fig +
          geom_point(aes(y = FID), color = "grey",
                       data = data.DT, size = 0.3, alpha = 0.5) +
          ggtitle("Bayesian estimates and FID"))

fig <- ggplot(results.DT, aes(x = p.escape, y = FID, color = color)) +
    geom_point(size = 0.8, alpha = 0.5) +
    facet_wrap(~ subject, scales = "free_y") +
    ggtitle("FID ~ p(escape | FID) by subject")
print(fig)

source("plot_distribution.R")
