FID.choices <- 1:81
data.DT[FID < 1, FID := 0]

Util.Pain <- function(.P.Escape) {
    .U <- function(FID) {
        p.escape <- sapply(FID, .P.Escape)
        return(1 - p.escape)
    }
    return(.U)
}

Util.Money <- function(.P.Escape, .Money) {
    .U <- function(FID) {
        money <- sapply(FID, .Money)
        p.escape <- sapply(FID, .P.Escape)
        return(p.escape * money)
        
        # note that you can comment out line above and just do to confirm that function correctly
        # outputs the reward that the person observed; return(money)
    }
    return(.U)
}


## CMG: I took away the .money argument because we assign the .money within the function
.MakeUtilityTable <- function(distribution, .color, AD, trial, FID.choices) {
    stopifnot(!is.unsorted(trial), length(trial) == length(AD))
    # a new object
    # TODO: use estimate of sigma here. replace with known variance^(1/2)
    this.bayes <- bayes.player.known.variance$proto(x = NULL,
                                    sigma.x = likelihood.sigma.x)
    this.bayes$Set.Prior(prior.mean.mu, prior.sd.mu)
    
    # sequentially observe
    util.table <- ldply(1:length(trial), .fun = function(i) {
      
        # print(paste("Index:", i, "Color:", .color, "Distribution:", distribution[i]))
      
        # estimate utility
        # CMG added since money depends on predator (fast/slow) and distribution (left/right)
        if (.color == "fast" & distribution[i] == "left") {
          .Money <- Money.Exponential.Fast.Left
        } else if (.color == "fast" & distribution[i] == "right") {
          .Money <- Money.Exponential.Fast.Right
        } else if (.color == "slow" & distribution[i] == "left") {
          .Money <- Money.Exponential.Slow.Left
        } else if (.color == "slow" & distribution[i] == "right") {
          .Money <- Money.Exponential.Slow.Right
        }
          
        .Util.Pain <- Util.Pain(. %>% this.bayes$P.Escape(a = a, b = b))
        .Util.Money <- Util.Money(. %>% this.bayes$P.Escape(a = a, b = b),
                                  .Money)
 
        
        util.money <- .Util.Money(FID.choices) 
        util.pain <- .Util.Pain(FID.choices)
        # observe and update belief
        this.bayes$Observe(AD[i])
        return(c(trial = trial[i],
                 util.money, util.pain))
    })
    colnames(util.table)[(1:length(FID.choices)) + 1] <-
        paste("money.FID", FID.choices, sep = ".")
    colnames(util.table)[(1:length(FID.choices)) + length(FID.choices) + 1] <-
        paste("pain.FID", FID.choices, sep = ".")
    return(util.table)
}


bayesian.utility.DT <- data.DT[subject == "22",
                           as.list(.MakeUtilityTable(distribution, color, AD, trial,
                                                         FID.choices)),
                               by = color]


## CMG: instead of using one subjects utility estimations (bayesian.utility.DT), calculate the utility for each subject
# given what that particular subject saw ---> Big.bayesian.utility.DT
Big.bayesian.utility.DT <- list()
subjects = levels(as.factor(data.DT$subject))
# Loop through each subject
for (i in 1:length(subjects)) {
  
  # Call the function for each subject and store the result in a temporary variable
  temp_result <- data.DT[subject == subjects[i],
                         as.list(.MakeUtilityTable(distribution, color, AD, trial,
                                                   FID.choices)),
                         by = color]
  # Add the subject variable to the temporary result
  temp_result[, subject := subjects[i]]
  # Store the temporary result in the list
  Big.bayesian.utility.DT[[i]] <- temp_result
}
# Bind the results vertically into one data.table
Big.bayesian.utility.DT <- rbindlist(Big.bayesian.utility.DT)
setkey(Big.bayesian.utility.DT, subject, trial, color)





## Plot estimated expected reward and pain for one person
setkey(bayesian.utility.DT, trial, color)

util.money.DT <- bayesian.utility.DT[, grep("money.",
                                            colnames(bayesian.utility.DT)),
                                     with = FALSE]
util.pain.DT <- bayesian.utility.DT[, grep("pain.",
                                            colnames(bayesian.utility.DT)),
                                    with = FALSE]

# melt
b.util.money.DT.melted <-
    cbind(util.money.DT, bayesian.utility.DT[, .(trial, color)]) %>%
    melt(id.vars = c("trial", "color"))
b.util.money.DT.melted[, variable := as.numeric(variable)]
setnames(b.util.money.DT.melted, c("variable", "value"), c("FID", "util.money"))

b.util.pain.DT.melted <-
    cbind(util.pain.DT, bayesian.utility.DT[, .(trial, color)]) %>%
    melt(id.vars = c("trial", "color"))
b.util.pain.DT.melted[, variable := as.numeric(variable)]
setnames(b.util.pain.DT.melted, c("variable", "value"), c("FID", "util.pain"))


# plot
b.util.money.DT.melted[, trial.within.color := 1:.N, by = .(color, FID)]
b.util.money.DT.melted[, trial.within.color := as.factor(trial.within.color)]
b.util.pain.DT.melted[, trial.within.color := 1:.N, by = .(color, FID)]
b.util.pain.DT.melted[, trial.within.color := as.factor(trial.within.color)]

fig.1 <- ggplot(b.util.money.DT.melted[trial.within.color %in% c("1", "5", "10", "15", "20")],
                aes(x = FID, y = util.money, color = color, alpha = trial.within.color)) +
    geom_line(size = 0.8) +
    facet_wrap(~ color, ncol = 1, scale = "free_y") +
    ggtitle("Estimated expected reward:\nprogression with trials (by color)")
# print(fig.1)

fig.2 <- ggplot(b.util.pain.DT.melted[trial.within.color %in% c("1", "5", "10", "15", "20")],
                aes(x = FID, y = util.pain, color = color, alpha = trial.within.color)) +
    geom_line(size = 0.8) +
    facet_wrap(~ color, ncol = 1, scale = "free_y") +
    ggtitle("Estimated expected pain:\nprogression with trials (by color)")
# print(fig.2)

grid.arrange(fig.1, fig.2, nrow = 1)
