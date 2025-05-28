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

# Extract id and session
merged_data$id <- substr(merged_data$subject_id, 1, 5)
merged_data$session <- as.numeric(substr(merged_data$subject_id, 12, 12))
merged_data$run <- (substr(merged_data$subject_id, 18, 20))


# Save merged data file
# write.csv(merged_data, paste0("L://rsmith/lab-members/cgoldman/ironside_FID/LIBR_FID_scripts_CMG/results/FID_merged_data",format(Sys.time(), "_%Y-%m-%d_%H_%M_%S"),".csv"))

# Optionally load in most recent merged data file
folder <- "L://rsmith/lab-members/cgoldman/ironside_FID/LIBR_FID_scripts_CMG/results/"
files <- list.files(folder, pattern = "merged_data.*\\.csv$", full.names = TRUE)
merged_data <- read.csv(files[which.max(file.info(files)$mtime)])

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

# Don't move these libraries to the top of the file
library(tidyr)
library(dplyr)

# Pivot the data so each subject (id) has one row, with session-specific columns
wide_data <- merged_data %>%
  select(c(id, session, run, regret, pain, money, money.per.pain)) %>%  # Ensure only relevant columns are included
  distinct() %>%  # Remove duplicates if necessary
  pivot_wider(
    names_from = c(session,run),
    values_from = c(regret, pain, money, money.per.pain),
    names_glue = "{.value}_session_{session}_run_{run}"
  )


# Comparing Runs and Sessions of FID Task
source("comparing_runs_and_sessions.R")



# correlation between anxiety level and modelling parameters.







# Correlation between anxiety level and utility received
  all.utility <- bayesian.retrace.DT %>%
  dplyr::group_by(subject) %>%
  dplyr::summarize(utilityx = mean(total.util, na.rm = TRUE), .groups = "drop")




# Correlation between mean diff FID for fast and slow predators and anxiety




# correlation figures
flight_anxiety_cor = data.frame(anxiety = subject_anxiety_2016$anxiety, flight = choice.coeff.DT$money)

g <- ggplot(flight_anxiety_cor,aes(x=anxiety, y=flight)) + geom_point(shape=16, size=3) +
  geom_smooth(method=lm,lwd=1,color="forestgreen") +
  xlab("Trait Anxiety Inventory") +
  ylab("Reward Preference") +
  theme_grey()+
  theme(axis.title.x = element_text(size=10),
        axis.text.x  = element_text(size=10)) +
  theme(axis.title.y = element_text(size=10),
        axis.text.y  = element_text(size=10)) +
  ggtitle("Trait Anxiety ~ Reward Preference") +
  #scale_y_continuous(limits = c(0,40)) +
  theme(plot.title = element_text(size=10, face="bold"))

g

flight_anxiety_cor = data.frame(anxiety = all.anxiety$anxiety, flight = all.coeff$pain)

g <- ggplot(flight_anxiety_cor,aes(x=anxiety, y=flight)) + geom_point(shape=16, size=3) +
  geom_smooth(method=lm,lwd=1,color="red") +
  xlab("Trait Anxiety Inventory") +
  ylab("Punishment Preference") +
  theme_grey()+
  theme(axis.title.x = element_text(size=10),
        axis.text.x  = element_text(size=10)) +
  theme(axis.title.y = element_text(size=10),
        axis.text.y  = element_text(size=10)) +
  ggtitle("Trait Anxiety ~ Punishment Preference") +
  #scale_y_continuous(limits = c(0,40)) +
  theme(plot.title = element_text(size=10, face="bold"))

g



# plot parameter graph without outliers

all.coeff.DT <- choice.coeff.DT[-27,]
all.coeff.confint.DT <- choice.coeff.confint.DT[-27,]

{ggplot(cbind(all.coeff.DT, all.coeff.confint.DT),
        aes(x = pain, y = money,
            xmin = low.pain, xmax = high.pain,
            ymin = low.money, ymax = high.money,
            color = subject)) +
  geom_errorbar() + geom_errorbarh() +
  geom_label(aes(label = subject)) +
  ggtitle("Estimated coefficients and 95% C.I. by subject") +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position="none")} %>% print


write.csv(all.regrets.DT,"~/Downloads/all.regrets.DT.csv")
write.csv(all.coeff.DT,"~/Downloads/all.coeff.DT.csv")


# plot FID retrace graph without outliers

all.retrace.DT <- filter(bayesian.retrace.DT,subject!=16 & subject!=19 & subject!=27)
all.retrace.DT <- bayesian.retrace.DT
all.regrets.DT <- filter(regrets.DT,subject!=16 & subject!=19 & subject!=27)
all.regrets.DT <- regrets.DT

{
  ggplot(all.retrace.DT,
         aes(x = trial)) +
  geom_line(aes(y = FID.optimal, color = color), size = 1) +
  geom_point(aes(y = FID, color = color), shape = 1) +
  #geom_text(aes(label = display), x = 50, y = 10, size = 3, data = all.regrets.DT) +
  scale_colour_manual(values = c("dodgerblue2","orange", "red")) +
  facet_wrap(~ subject,nrow = 4) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())
  #ggtitle(paste( "Actual FID (dot) and bayesian optimal alternative (line)", sep = "\n"))+
  #theme_bw()
} %>% print








# save the result parameter files
write.csv(subject_anxiety_2016,"~/Downloads/stats_files_for_paper/subject_anxiety_2016.csv")
write.csv(choice.coeff.DT,"~/Downloads/stats_files_for_paper/choice.coeff.DT.csv")
write.csv(all.utility,"~/Downloads/stats_files_for_paper/all.utility.csv")


# R^2 for the corrlation between predicted fid and actual fid?
all.fast <- filter(all.retrace.DT,color=="fast")
all.mid <- filter(all.retrace.DT,color=="mid")
all.slow <- filter(all.retrace.DT,color=="slow")


cor.test(all.fast$FID,all.fast$FID.optimal)
cor.test(all.mid$FID,all.mid$FID.optimal)
cor.test(all.slow$FID,all.slow$FID.optimal)

cor.test(bisbas2$V4[1:23],all.fast$FID.optimal[1:23])
cor.test(bisbas2$V5[1:23],all.fast$FID.optimal[1:23])

cor.test(bisbas2$V4[1:23],all.slow$FID.optimal[1:23])
cor.test(bisbas2$V5[1:23],all.slow$FID.optimal[1:23])





setwd("/Users/songqi/Downloads/kat-risk-estimate-master 2")




# parametric analysis with trial-by-trial regret (absolute value)

bayesian.retrace.DT[,regret := total.util.optimal - total.util]
bayesian.retrace.DT.far <- filter(bayesian.retrace.DT,color == "fast")
bayesian.retrace.DT.close <- filter(bayesian.retrace.DT,color == "slow")

para <- bayesian.retrace.DT %>%
  group_by(subject) %>%
  summarize(regret = mean(regret))

far_para <- bayesian.retrace.DT.far %>%
  group_by(subject) %>%
  summarize(regret = mean(regret))

close_para <- bayesian.retrace.DT.close %>%
  group_by(subject) %>%
  summarize(regret = mean(regret))

cor.test(far_para$regret,subject_anxiety_2016$anxiety)
cor.test(close_para$regret,subject_anxiety_2016$anxiety)
cor.test(para$regret,subject_anxiety_2016$anxiety)

flight_anxiety_cor = data.frame(anxiety = subject_anxiety_2016$anxiety, flight = far_para$regret)

g <- ggplot(flight_anxiety_cor,aes(x=anxiety, y=flight)) + geom_point(shape=16, size=3) +
  geom_smooth(method=lm,lwd=1,color="grey") +
  xlab("Trait Anxiety") +
  ylab("Distance to Bayesian Ideal") +
  theme_bw()+
  theme(axis.title.x = element_text(size=10),
        axis.text.x  = element_text(size=10)) +
  theme(axis.title.y = element_text(size=10),
        axis.text.y  = element_text(size=10)) +
  ggtitle("") +
  #scale_y_continuous(limits = c(0,40)) +
  theme(plot.title = element_text(size=10, face="bold"))

g



far_para_normal <- far_para$regret/choice.coeff.DT$money
close_para_normal <- close_para$regret/choice.coeff.DT$money
para_normal <- para$regret/choice.coeff.DT$money



cor.test(-scale(perf$rewardx),para$regret)

cor.test(-scale(perf$escapex),para$regret)

cor.test(-scale(perf_close$rewardx),close_para$regret)

cor.test(-scale(perf_far$rewardx),far_para$regret)




flight_anxiety_cor = data.frame(anxiety = far_para$regret, flight = -scale(perf_far$rewardx))

g <- ggplot(flight_anxiety_cor,aes(x=anxiety, y=flight)) + geom_point(shape=16, size=3) +
  geom_smooth(method=lm,lwd=1,color="royalblue") +
  xlab("Distance to Bayesian Ideal (fast predators)") +
  ylab("Performance Score (fast predators)") +
  theme_bw()+
  theme(axis.title.x = element_text(size=10),
        axis.text.x  = element_text(size=10)) +
  theme(axis.title.y = element_text(size=10),
        axis.text.y  = element_text(size=10)) +
  ggtitle("") +
  #scale_y_continuous(limits = c(0,40)) +
  theme(plot.title = element_text(size=10, face="bold"))

g



write.xlsx(far_para ,"/Users/songqi/Downloads/kat-risk-estimate-master 2/far_para.xlsx")
write.xlsx(close_para ,"/Users/songqi/Downloads/kat-risk-estimate-master 2/close_para.xlsx")

