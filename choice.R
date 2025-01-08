# run after source("bayesian_utilities.R)
library(ggplot2)
library(mlogit) # note that an older version of mlogit must be used; remotes::install_version("mlogit", version = "1.0-3.1")


# choices.data.DT <- merge(data.DT[FID>0], bayesian.utility.DT)

## CMG: instead of using one subjects utility estimations, use the bayesian utility for each subject
# given what that particular subject saw ---> Big.bayesian.utility.DT
choices.data.DT <- merge(data.DT[FID>0], Big.bayesian.utility.DT)



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
subjects <- levels(as.factor(choices.data.DT$subject))
# subjects <- levels((choices.data.DT$subject))

# Estimate ~~~~~
# We use a multinomial logit model that seeks to maximize the probability assigned to the FID 
# that the participant chose on a given trial. It fits two parameters, pain and money, that 
# multiply the utility of pain or money for each FID (previously determined by the predicted 
# attack distance → probability of escape for each FID → utility for each FID). 
# The model uses a softmax rule to compare FID choices. 
# e.g
# Value of FID 1 = money*bayesian_utility_money_FID_1 + pain*bayesian_utility_pain_FID_1 
# Value of FID 2 = money*bayesian_utility_money_FID_2 + pain*bayesian_utility_pain_FID_2
# Probability of Choosing FID 1 = softmax = exp(Value of FID1)/(exp(Value of FID1)+exp(Value of FID2))

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
