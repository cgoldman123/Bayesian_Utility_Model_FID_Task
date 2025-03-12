.SequentialEstimateProb <- function(FID.vec, AD) {
    # a new object
    # TODO: use estimate of sigma here. replace with known variance^(1/2)
    this.bayes <- bayes.player.known.variance$proto(x = NULL,
                                                    sigma.x = likelihood.sigma.x)
    this.bayes$Set.Prior(prior.mean.mu, prior.sd.mu)
    # sequentially observe
    M <- ldply(1:length(AD), .fun = function(i) {
        # P(escape)
        p.escape.vec = this.bayes$P.Escape(FID.vec, a = a, b = b)
        # observe and update belief
        this.bayes$Observe(AD[i])
        return(p.escape.vec)})
    .P <- data.frame(M)
    .P$trial.within.color <- 1:length(AD)
    return(.P)
}

FID.vec <- 0:80

## LEFT Distribution
p.escape.by.FID.DT <- data.DT[numeric_id=="202"& distribution=="left",
                              .SequentialEstimateProb(FID.vec, AD),
                              by = color]
p.escape.by.FID.DT <- melt(p.escape.by.FID.DT,
                           id.vars = c("color", "trial.within.color"),
                           value.name = "p.escape")
p.escape.by.FID.DT[, FID := FID.vec[as.factor(variable)]]
p.escape.by.FID.DT[, trial.within.color := as.factor(trial.within.color)]

fig <- ggplot(p.escape.by.FID.DT[trial.within.color %in% c("1", "5", "10", "15", "20")],
              aes(x = FID, y = p.escape, color = color)) +
    geom_line(size = 0.8, aes(alpha = trial.within.color)) +
   scale_colour_manual(values = c("dodgerblue2", "red")) +
    facet_wrap(~ color, scales = "free_y", ncol=1) +
    ylab("probability of escape") +
    ggtitle("Estimated probability of escape under various FID for left distribution") +
   theme_bw()
print(fig)

## RIGHT Distribution
p.escape.by.FID.DT <- data.DT[numeric_id=="202" & distribution=="right",
                              .SequentialEstimateProb(FID.vec, AD),
                              by = color]
p.escape.by.FID.DT <- melt(p.escape.by.FID.DT,
                           id.vars = c("color", "trial.within.color"),
                           value.name = "p.escape")
p.escape.by.FID.DT[, FID := FID.vec[as.factor(variable)]]
p.escape.by.FID.DT[, trial.within.color := as.factor(trial.within.color)]

fig <- ggplot(p.escape.by.FID.DT[trial.within.color %in% c("1", "5", "10", "15", "20")],
              aes(x = FID, y = p.escape, color = color)) +
  geom_line(size = 0.8, aes(alpha = trial.within.color)) +
  scale_colour_manual(values = c("dodgerblue2", "red")) +
  facet_wrap(~ color, scales = "free_y", ncol=1) +
  ylab("probability of escape") +
  ggtitle("Estimated probability of escape under various FID for right distribution") +
  theme_bw()
print(fig)