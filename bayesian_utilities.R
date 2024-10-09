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
    }
    return(.U)
}

.MakeUtilityTable <- function(.color, AD, trial, FID.choices, .Money) {
    stopifnot(!is.unsorted(trial), length(trial) == length(AD))
    # a new object
    # TODO: use estimate of sigma here. replace with known variance^(1/2)
    this.bayes <- bayes.player.known.variance$proto(x = NULL,
                                    sigma.x = likelihood.sigma.x)
    this.bayes$Set.Prior(prior.mean.mu, prior.sd.mu)
    # sequentially observe
    util.table <- ldply(1:length(trial), .fun = function(i) {
        # estimate utility
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


bayesian.utility.DT <- data.DT[subject == "6",
                           as.list(.MakeUtilityTable(color, AD, trial,
                                                         FID.choices,
                                                         Money.Linear)),
                               by = color]
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
