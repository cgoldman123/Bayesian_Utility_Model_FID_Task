source("models.R")

library(ggplot2)
library(data.table)
library(plyr)
library(gridExtra)

# Read data
setwd("L://rsmith//lab-members//cgoldman//ironside_FID//LIBR_FID_scripts_CMG")
data.DT <- data.table(read.csv("task_data/expanded_data_LIBR_5-28-25.csv"))
# data.DT <- data.table(read.csv("task_data/carter_processed_data_7-9-25.csv"))
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
# filter out anyone who only has 30 trials for runs_1_2 or runs_3_4
data.DT = data.DT[, if (.N == 60) .SD, by = subject]


# Create a new subject_id column
data.DT$subject_id = data.DT$subject
# Re-code the subject column to be a number for each participant
data.DT$subject <- as.factor(as.numeric(factor(data.DT$subject_id)))
# Create a mapping of subject IDs to numeric values
subject_mapping <- unique(data.DT[, .(subject, subject_id)])
data.DT$color = data.DT$PredatorSpeed
data.DT[, color := factor(c("slow", "fast")[color],
                             levels = c("slow", "fast"))]
# Round FID to nearest integer
# necessary for plots; also would be needed for mulitnomial logit model
data.DT$FID = round(data.DT$FID) 

# Even subject IDs received the "right" distribution for v1, but the "left" distribution for v2 (see Rayus' readme for explanation)
# For the right distribution, the predator will travel an average of 5 units more before attacking (for both fast and slow predators),
# making it slower on average (i.e., lower attack distances)
# Create a "distribution" column that tracks if the session was "right" or "left"
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
# Carter played his task run using the left distribution
data.DT[subject_id == "CG000_ses-v1_run_1_2" | subject_id == "CG000_ses-v1_run_3_4", distribution:= "left"]


# Verify that this distribution mapping is correct by looking through the json files that contain distribution info:
library(jsonlite)
# Define the base directory where the JSON files are located
base_dir <- "L:/NPC/DataSink/study-Ironside-2023-TCADPilot/data-original/functional_session"
# Find all FID_balance.json files recursively
json_files <- list.files(base_dir, pattern = "FID_balance\\.json$", recursive = TRUE, full.names = TRUE)
distribution_info_json <- data.frame(id = character(), distribution = character(), stringsAsFactors = FALSE)
# Loop through each file and extract the needed information
for (file in json_files) {
  json_data <- fromJSON(file)
  if (!is.null(json_data$balance)) {
    balance_entries <- json_data$balance
    distribution_info_json <- dplyr::bind_rows(distribution_info_json, data.frame(
      id = balance_entries$sid,
      distribution = balance_entries$distribution,
      stringsAsFactors = FALSE
    ))
    
  }
}
# Compare the distribution info from the extracted JSON files to the distribution info in data.DT
distribution_info_DT <- data.DT %>%
  dplyr::mutate(subject_short = paste0(substr(subject_id, 1, 5), ifelse(session == 1, "_T0", "_T1"))) %>%
  dplyr::distinct(subject_short, session, distribution, .keep_all = TRUE) %>%
  dplyr::select(subject_short, distribution) %>%
  dplyr::rename(distribution_DT = distribution)
  
combined_distribution_dataframe <- dplyr::left_join(distribution_info_json, distribution_info_DT, by = c("id" = "subject_short"))
combined_distribution_dataframe$identical_dist = combined_distribution_dataframe$distribution == combined_distribution_dataframe$distribution_DT


## Plot out behavior
# discard those FID <= 0
data.DT <- data.DT[FID > 0, ]  
setkey(data.DT, subject, trial, color)


# Plot FID ~ AD for all subjects
fig.1 <- ggplot(data.DT, aes(x = trial, y = FID, color = color)) +
    geom_line(size = 1, alpha = .5) +
    facet_wrap( ~ subject) +
    ggtitle("FID ~ trial by subject")
print(fig.1)


# Plot FID ~ AD for all subjects
fig.2 <- ggplot(data.DT, aes(x = AD, y = FID, color = color)) +
    geom_point(size = 1, alpha = .5) +
    facet_wrap( ~ subject) +
    ggtitle("FID ~ AD by subject")
print(fig.2)

# For one subject, plot the FID ~ Trial by condition (high/low reward/shock values)
# Note that the line is truncated because the condition reward/shock=0 is not included in the plot.
plot_data <- data.DT %>%
  dplyr::filter(subject == "46", RewardLevel %in% c(1, 2), ShockLevel %in% c(1, 2)) %>%
  dplyr::mutate(
    condition_label = dplyr::case_when(
      RewardLevel == 2 & ShockLevel == 1 ~ "High Reward, Low Shock",
      RewardLevel == 1 & ShockLevel == 2 ~ "Low Reward, High Shock",
      RewardLevel == 2 & ShockLevel == 2 ~ "High Reward, High Shock",
      RewardLevel == 1 & ShockLevel == 1 ~ "Low Reward, Low Shock"
    ),
    trial = as.numeric(as.character(trial))
  ) %>%
  dplyr::group_by(subject, condition_label) %>%
  dplyr::arrange(trial) %>%
  dplyr::mutate(trial_new = dplyr::row_number()) %>%
  dplyr::ungroup()

# Calculate mean FID per condition
mean_lines <- plot_data %>%
  dplyr::group_by(condition_label) %>%
  dplyr::summarize(mean_FID = mean(FID, na.rm = TRUE))

# Plot with mean line
fig_combined <- ggplot(plot_data, aes(x = trial_new, y = FID, color = color)) +
  geom_line(size = 1, alpha = 0.5) +
  geom_hline(data = mean_lines, aes(yintercept = mean_FID), linetype = "dashed", color = "black") +
  facet_wrap(~ condition_label, scales = "free_x") +
  labs(
    title = paste("FID ~ Trial by Condition (Subject", dplyr::first(plot_data$subject), ")"),
    x = "Trial", y = "FID"
  ) +
  theme_minimal()

print(fig_combined)




# estimate p(escape under FID) for each (subject, color, trial)
# p(escape) = p( AD < a * FID + b)
# The constants below are from Song's original code. Perhaps we should adjust this?
V.predator.fast <- 26
V.predator.slow <- 26/7
V.subject.run <- 1
L <- 5

# Remember the FID has to be greater than the attack distance since the 
# predator moves faster than we do. The constants a and b account for this difference.
a <- V.predator.fast / (V.predator.fast - V.predator.slow)
b <- L * a * (1 - V.predator.slow / V.subject.run)

# Bayesian player model
likelihood.sigma.x <- 5.2 # standard deviation of observations based on what Song had here. Perhaps we should adjust this?
prior.mean.mu <- 40 # prior mean for attack distance based on true mean of practice trials
prior.sd.mu <- 12 # prior standard deviation for attack distance based on true sd of practice trials

# estimate mean and variance of AD for each color from data
# This should be the same for the left and right distributions
# Across all participants
# Remember that right distribution corresponds to slow!

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


# sequentially process trials for each (subject, color), getting the probability of escape on each trial based on FID
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
# Save results to results.DT
results.DT <- data.DT[, as.list(.ProcessTrialsSequential(FID, AD, trial)),
                      by = .(subject, color)]
setkey(results.DT, subject, trial, color)
results.DT <- merge(data.DT, results.DT)

# Plot the probability of escape for each subject
fig <- ggplot(results.DT, aes(x = trial, y = p.escape, color = color)) +
    geom_point(size = 1, alpha = .5) +
    facet_wrap( ~ subject) +
    ggtitle("P(escape) by subject")
print(fig)

# Plot the probability of escape separated by predator type
fig <- ggplot(results.DT, aes(x = trial, y = p.escape, color = color)) +
    geom_point(size = 1, alpha = .5) +
    facet_wrap( ~ color) +
    ggtitle("P(escape) by color")
print(fig)

# Plot learning for one subject who experienced the left distribution
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

# Plot learning for one subject who experienced the right distribution
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

# Plot FID ~ p(escape | FID) by subject
fig <- ggplot(results.DT, aes(x = p.escape, y = FID, color = color)) +
    geom_point(size = 0.8, alpha = 0.5) +
    facet_wrap(~ subject, scales = "free_y") +
    ggtitle("FID ~ p(escape | FID) by subject")
print(fig)


# Plot FID by reward level and shock level
plot_df <- data.DT[, .(
  mean_FID = mean(FID, na.rm = TRUE),
  se_FID = sd(FID, na.rm = TRUE) / sqrt(.N)
), by = .(RewardLevel, ShockLevel)]

# Convert grouping variables to factors in plot_df only
plot_df[, RewardLevel := as.factor(RewardLevel)]
plot_df[, ShockLevel := as.factor(ShockLevel)]

# Plot mean FID by reward level and shock level
ggplot(plot_df, aes(x = RewardLevel, y = mean_FID, fill = ShockLevel)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean_FID - se_FID, ymax = mean_FID + se_FID),
                position = position_dodge(width = 0.8), width = 0.2) +
  labs(x = "Reward Level", y = "Mean FID ± SE", fill = "Shock Level") +
  theme_minimal()


# Get the estimated probability of escape under various FID based on a person's estimate
# of the attack distance
source("plot_distribution.R")


