source("models.R")

library(ggplot2)
library(data.table)
library(plyr)
library(gridExtra)

# read data
setwd("L://rsmith//lab-members//cgoldman//ironside_FID//LIBR_FID_scripts_CMG")
data.DT <- data.table(read.csv("data/expanded_data_LIBR.csv"))
# Rename trial column
data.DT <- data.DT %>% dplyr::rename(trial = Trial)


# Note that participants performed four runs of 30 trials in each session.
# Runs 1 and 2 contained the same color predators, as did runs 3 and 4.
data.DT[trial <= 60, subject := paste0(subject, "_run_1_2")]
data.DT[trial <= 60, run := "run_1_2"]
data.DT[trial > 60 & trial <= 120, subject := paste0(subject, "_run_3_4")]
data.DT[trial > 60 & trial <= 120, run := "run_3_4"]
# Re-label trials of runs 3_4 (e.g., instead of trial 65, it would be trial 5 of run 3_4)
data.DT[trial > 60, trial := trial - 60]


data.DT$subject_id = data.DT$subject
data.DT$subject <- as.factor(as.numeric(factor(data.DT$subject_id)))
# Create a mapping of subject IDs to numeric values
subject_mapping <- unique(data.DT[, .(subject, subject_id)])
data.DT$color = data.DT$PredatorSpeed
data.DT[, color := factor(c("slow", "fast")[color],
                             levels = c("slow", "fast"))]
# round FID to nearest integer
data.DT$FID = round(data.DT$FID) # necessary for plots; also would be needed for mulitnomial logit model

# Even subject IDs received the "right" distribution for v1, but the "left" distribution for v2 (see Rayus' readme for explanation)
# For the right distribution, the predator will travel an average of 5 units more before attacking (for both fast and slow predators),
# making it slower on average (i.e., lower attack distances)
data.DT <- data.DT %>%
  mutate(
    # Extract the numeric part of the subject_id
    numeric_id = as.numeric(substring(subject_id, 3, 5)),
    # Extract the session version (1 or 2)
    session = sub(".*_ses-v(.)", "\\1", subject_id),
  ) %>% 
  mutate(session = substr(session, 1, 1)) %>%
  mutate(    # Apply the logic to create the distribution column
    distribution = dplyr::case_when(
      numeric_id %% 2 == 0 & session == "1" ~ "right",
      numeric_id %% 2 == 0 & session == "2" ~ "left",
      numeric_id %% 2 == 1 & session == "1" ~ "left",
      numeric_id %% 2 == 1 & session == "2" ~ "right"
    ))

# Two subjects were accidentally given the same distribution for both sessions, probably because someone put T0 or T1 for both
# Adjust the rows that were improperly labeled
data.DT[subject_id == "BV250_ses-v1_run_1_2" | subject_id == "BV250_ses-v1_run_3_4", distribution:= "left"]
data.DT[subject_id == "BR982_ses-v2_run_1_2" | subject_id == "BR982_ses-v2_run_3_4", distribution:= "right"]





data.DT <- data.DT[FID > 0, ]  # discard those FID <= 0
setkey(data.DT, subject, trial, color)

# plot
fig.1 <- data.DT %>% dplyr::filter(subject %in% c("1","2","3")) %>%
    ggplot(., aes(x = trial, y = AD, color = color)) +
    geom_line(size = 1, alpha = .5) +
    facet_wrap( ~ subject) +
    ggtitle("AD ~ trial by subject")
print(fig.1)

fig.2 <- ggplot(data.DT, aes(x = trial, y = FID, color = color)) +
    geom_line(size = 1, alpha = .5) +
    facet_wrap( ~ subject) +
    ggtitle("FID ~ trial by subject")
print(fig.2)


fig.3 <- data.DT %>% dplyr::filter(subject %in% c("1","2","3")) %>%
  dplyr::filter(RewardLevel == 2, ShockLevel == 1) %>% 
  dplyr::mutate(trial = as.numeric(as.character(trial))) %>%  # Ensure numeric
  dplyr::group_by(subject) %>% 
  dplyr::arrange(subject, trial) %>%  # Arrange within each subject
  dplyr::mutate(trial_new = dplyr::row_number()) %>% 
  dplyr::ungroup() %>%  
  ggplot(aes(x = trial_new, y = FID, color = color)) +
  geom_line(size = 1, alpha = 0.5) +
  facet_wrap(~ subject) +
  ggtitle("FID ~ trial by subject for the high reward low shock condition")
print(fig.3)

fig.4 <- data.DT %>% dplyr::filter(subject %in% c("1","2","3")) %>%
  dplyr::filter(RewardLevel == 1, ShockLevel == 2) %>% 
  dplyr::mutate(trial = as.numeric(as.character(trial))) %>%  # Ensure numeric
  dplyr::group_by(subject) %>% 
  dplyr::arrange(subject, trial) %>%  # Arrange within each subject
  dplyr::mutate(trial_new = dplyr::row_number()) %>% 
  dplyr::ungroup() %>%  
  ggplot(aes(x = trial_new, y = FID, color = color)) +
  geom_line(size = 1, alpha = 0.5) +
  facet_wrap(~ subject) +
  ggtitle("FID ~ trial by subject for the low reward high shock condition")
print(fig.4)

fig.5 <- ggplot(data.DT, aes(x = AD, y = FID, color = color)) +
    geom_point(size = 1, alpha = .5) +
    facet_wrap( ~ subject) +
    ggtitle("FID ~ AD by subject")
print(fig.5)






# estimate p(escape under FID) for each (subject, color, trial)
# p(escape) = p( AD < a * FID + b)
V.predator.fast <- 26
V.predator.slow <- 26/7
V.subject.run <- 1
L <- 5

# Remember the FID has to be greater than the attack distance since the 
# predator moves faster than we do. The constants a and b account for this difference.
a <- V.predator.fast / (V.predator.fast - V.predator.slow)
b <- L * a * (1 - V.predator.slow / V.subject.run)

# Bayesian player model
likelihood.sigma.x <- 5.2 # standard deviation of observations
prior.mean.mu <- 40 # prior mean for attack distance based on true mean of practice trials
prior.sd.mu <- 12 # prior standard deviation for attack distance based on true sd of practice trials

# estimate mean and variance of AD for each color from data
# This should be the same for the left and right distributions
# Across all participants
AD.estimates.left.all.pts <- data.DT[distribution == "left",  # AD sequence the same for all subjects
                                     .(mu = mean(AD), sigma = sd(AD)), by = .(subject_id, color)]
# summarized
AD.estimates.left <- data.DT[distribution == "left",  
                              .(mu = mean(AD), sigma = sd(AD)), 
                              by = color]
# Across all participants
AD.estimates.right.all.pts <- data.DT[distribution == "right",  
                              .(mu = mean(AD), sigma = sd(AD)), 
                              by = .(subject_id, color)]
# Summarized
AD.estimates.right <- data.DT[distribution == "right",  
                              .(mu = mean(AD), sigma = sd(AD)), 
                              by = color]

# BV250 has fast for both
# BR982 has slow for both
# In the balance document, it seems that right corresponds to slow?


# sequentially process trials for each (subject, color)
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

# Filter to one subject who experienced the left distribution
fig <- ggplot(results.DT[distribution == "left" & numeric_id=="202",], aes(x = trial, y = posterior.AD.mean,
                              color = color)) +
    geom_point(size = 0.8, alpha = 0.8) +
    geom_linerange(aes(ymin = posterior.AD.mean - 2 * posterior.AD.sd,
                        ymax = posterior.AD.mean + 2 * posterior.AD.sd )) +
    geom_hline(aes(yintercept = mu, color = color),
               data = AD.estimates.left, linetype = "dashed") +
  scale_colour_manual(values = c("dodgerblue2", "red")) +
    facet_wrap( ~ color, scales = "free_y") +
    theme_bw()
print(fig + ggtitle("Left Distribution: Bayesian estimates for AD (95% C.I.)"))

# Filter to one subject who experienced the right distribution
fig <- ggplot(results.DT[distribution == "right" & numeric_id=="202",], aes(x = trial, y = posterior.AD.mean,
                                                                           color = color)) +
  geom_point(size = 0.8, alpha = 0.8) +
  geom_linerange(aes(ymin = posterior.AD.mean - 2 * posterior.AD.sd,
                     ymax = posterior.AD.mean + 2 * posterior.AD.sd )) +
  geom_hline(aes(yintercept = mu, color = color),
             data = AD.estimates.right, linetype = "dashed") +
  scale_colour_manual(values = c("dodgerblue2", "red")) +
  facet_wrap( ~ color, scales = "free_y") +
  theme_bw()
print(fig + ggtitle("Right Distribution: Bayesian estimates for AD (95% C.I.)"))


# Not the most helpful plot
# print(fig +
#           geom_point(aes(y = FID), color = "grey",
#                        data = data.DT, size = 0.3, alpha = 0.5) +
#           ggtitle("Bayesian estimates and FID"))

fig <- ggplot(results.DT, aes(x = p.escape, y = FID, color = color)) +
    geom_point(size = 0.8, alpha = 0.5) +
    facet_wrap(~ subject, scales = "free_y") +
    ggtitle("FID ~ p(escape | FID) by subject")
print(fig)

# Get the estimated probability of escape under various FID based on a person's estimate
# of the attack distance
source("plot_distribution.R")
