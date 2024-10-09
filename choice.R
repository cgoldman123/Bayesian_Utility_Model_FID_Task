# run after source("bayesian_utilities.R)
library(ggplot2)
library(mlogit) # note that an older version of mlogit must be used; remotes::install_version("mlogit", version = "1.0-3.1")


choices.data.DT <- merge(data.DT[FID>0], bayesian.utility.DT)

# let util. <- util. x levels
for (col in grep("pain\\.", colnames(choices.data.DT))) {
    set(choices.data.DT, i=NULL, col,
        choices.data.DT$shock.level * choices.data.DT[, col, with=F])
}
for (col in grep("money\\.", colnames(choices.data.DT))) {
    set(choices.data.DT, i=NULL, col,
        choices.data.DT$reward.level * choices.data.DT[, col, with=F])
}
choices.data.DT[, FID := factor(FID, levels = as.character(FID.choices))]
subjects <- levels(choices.data.DT$subject)

# estimate
choice.models.fit <- llply(
    subjects,
    function(i) {
        choices.sub.DT.wide <- choices.data.DT[subject == i,
                                               grep("trial|FID|pain\\.|money\\.",
                                                    colnames(choices.data.DT)),
                                               with = FALSE]
        # print(head(choices.sub.DT.wide,4))
        choices.sub.DT.long <- mlogit.data(
            choices.sub.DT.wide,
            choice = "FID",
            shape = "wide",
            sep = ".FID.",
            alt.levels = as.factor(FID.choices),
            chid.var = "trial",
            varying = grep("pain\\.|money\\.", colnames(choices.sub.DT.wide)))
        .fit <- mlogit(FID ~ pain + money | -1, choices.sub.DT.long)
        cat("\n=== Subject:", i, "===\n")
        print(summary(.fit))
        return(.fit)
    }
)

# coefficients
choice.coeff.DT <- ldply(seq_along(subjects),
                         .fun = function(i) coef(choice.models.fit[[i]]))
choice.coeff.DT$subject <- as.factor(as.integer(subjects))
choice.coeff.DT <- data.table(choice.coeff.DT)

choice.coeff.confint.DT <- data.table(
    ldply(seq_along(subjects),
          .fun = function(i) c(confint(choice.models.fit[[i]]))))
setnames(choice.coeff.confint.DT, c("low.pain", "low.money",
                                    "high.pain", "high.money"))

{ggplot(cbind(choice.coeff.DT, choice.coeff.confint.DT),
        aes(x = pain, y = money,
            xmin = low.pain, xmax = high.pain,
            ymin = low.money, ymax = high.money,
            color = subject)) +
    geom_errorbar() + geom_errorbarh() +
    geom_label(aes(label = subject)) +
    ggtitle("Estimated coefficients and 95% C.I. by subject") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme(legend.position="none")} %>% print

# money per pain = utility per pain / utility per money
choice.coeff.DT[, money.per.pain := - pain / money]

{ggplot(choice.coeff.DT, aes(x = subject, y = money.per.pain)) +
    geom_bar(stat = "identity") +
    ggtitle("Money per pain by subject")} %>% print

{ggplot(melt(choice.coeff.DT, id.vars=c("subject", "money.per.pain")),
        aes(x = subject, y = value, color = variable)) +
        geom_point() +
        facet_wrap(~ variable, scale = "free_y", ncol = 1)} %>% print
