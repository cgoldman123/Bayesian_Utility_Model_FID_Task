source("models.R")

library(ggplot2)
library(data.table)
library(plyr)
library(gridExtra)

# read data
setwd("L://rsmith//lab-members//cgoldman//ironside_FID//LIBR_FID_scripts_CMG")
# data.DT <- data.table(read.csv("data/data.csv"))
data.DT <- data.table(read.csv("data/expanded_data_LIBR.csv"))
data.DT$subject_id = data.DT$subject
data.DT$subject <- as.factor(as.numeric(factor(data.DT$subject_id)))
# Create a mapping of subject IDs to numeric values
subject_mapping <- unique(data.DT[, .(subject, subject_id)])
data.DT$color = data.DT$PredatorSpeed
data.DT[, color := factor(c("slow", "fast")[color],
                             levels = c("slow", "fast"))]
data.DT$reward.level = data.DT$RewardLevel
# round FID to nearest integer
data.DT$FID = round(data.DT$FID) # necessary for plots; perhaps we don't want to lose this info for analysis

# Even subject IDs received the "right" distribution of attack speeds and rewards for v1, but the "left" distribution for v2 (see Rayus' readme for explanation)
data.DT <- data.DT %>%
  mutate(
    # Extract the numeric part of the subject_id
    numeric_id = as.numeric(substring(subject_id, 3, 5)),
    # Extract the session version (1 or 2)
    session = sub(".*_ses-v", "", subject_id),
    # Apply the logic to create the distribution column
    distribution = dplyr::case_when(
      numeric_id %% 2 == 0 & session == "1" ~ "right",
      numeric_id %% 2 == 0 & session == "2" ~ "left",
      numeric_id %% 2 == 1 & session == "1" ~ "left",
      numeric_id %% 2 == 1 & session == "2" ~ "right"
    )
  ) %>%
  dplyr::select(-numeric_id, -session)  # Optionally remove the helper columns




data.DT <- data.DT[FID > 0, ]  # discard those FID <= 0
data.DT$trial = data.DT$Trial
data.DT$shock.level = data.DT$ShockLevel
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
  scale_colour_manual(values = c("dodgerblue2", "red")) +
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
