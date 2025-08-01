# Note: Make plot window big before running; helps with preventing R Studio from Crashing
rm(list = ls())
setwd("L://rsmith//lab-members//cgoldman//ironside_FID//LIBR_FID_scripts_CMG")

source("analysis.R")
source("reward_function.R")
source("models.R")
source("bayesian_utilities.R")
source("choice.R")
source("regrets.R")

# Merge the dataframes
merged_data <- merge(choice.coeff.DT, regrets.DT, by = "subject")
merged_data <- merge(merged_data, subject_mapping, by = "subject")
merged_data <- merge(merged_data, summarized_abs_error, by="subject")

# Extract id and session
merged_data$id <- substr(merged_data$subject_id, 1, 5)
merged_data$session <- as.numeric(substr(merged_data$subject_id, 12, 12))
merged_data$run <- (substr(merged_data$subject_id, 18, 20))


# Save merged data file
# write.csv(merged_data, paste0("L://rsmith/lab-members/cgoldman/ironside_FID/LIBR_FID_scripts_CMG/results/FID_merged_data",format(Sys.time(), "_%Y-%m-%d_%H_%M_%S"),".csv"))



# These notes are based on a discussion with Kyle Goldman. I adjusted my data processing pipeline on 5-28-25 to ensure I was not missing anything.
# Subjects. Note that if there are no notes, data is good
# BW169
# BN959
# AV532
# AA374 - didn't play last two runs
# BW137
# BC553
# BV250 - received left distribution for both runs
# BR982 - received right distribution for both runs
# BV156 - currently have no session 1 data. they initially had problems with audio, but were able to fix it so raw data for session 1 in 2023 July 11th for runs 3 and 4 is under artest1 folder
# AV503
# BD162
# BV520
# BV935
# BW460 - didn't play last run
# BD730
# BR372
# AE202
# AS988
# BV679 - aardron had to re-run bidsification scripts to process this person's data
# BV227
# DONT INCLUDE BW730; not in the study






