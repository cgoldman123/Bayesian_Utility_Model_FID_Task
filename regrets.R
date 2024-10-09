# retrace the optimal decision under player's coefficients
# run after source("choice.R")

bayesian.retrace.DT <- ldply(seq_along(subjects), .fun = function(i) {
    # iterate over subjects
    .subject <- subjects[i]
    .total.util.by.FID <- choice.coeff.DT[i, pain] * choices.data.DT[subject==.subject,
                             grep("pain\\.", colnames(choices.data.DT)),
                             with=F] +
        choice.coeff.DT[i, money] * choices.data.DT[subject==.subject,
                             grep("money\\.", colnames(choices.data.DT)),
                             with=F]
    .FID.optimal <- FID.choices[apply(.total.util.by.FID, 1, which.max)]
    .total.util.optimal <- apply(.total.util.by.FID, 1, max)
    .trial <- choices.data.DT[subject == .subject, trial]
    .FID <- choices.data.DT[subject == .subject, FID]
    .AD <- choices.data.DT[subject == .subject, AD]
    .color <- choices.data.DT[subject == .subject, color]
    .total.util <- unlist(laply(1:length(.FID),
                                function(i) {
        .col <- (1:length(FID.choices))[FID.choices == .FID[i]]
        return(.total.util.by.FID[i, .col, with = FALSE])}
        ))
    return(data.frame(subject = .subject,
                      trial = .trial,
                      color = .color,
                      AD = .AD,
                      FID = as.numeric(as.character(.FID)),
                      FID.optimal = .FID.optimal,
                      total.util = .total.util,
                      total.util.optimal = .total.util.optimal))
})

bayesian.retrace.DT <- data.table(bayesian.retrace.DT)

# compute regrets
regrets.DT <- bayesian.retrace.DT[FID>0,
                                  .(regret = sum(total.util.optimal - total.util)),
                                  by = subject]
regrets.DT[, display := sprintf("regret = %.1f", regret), by = subject]
print(regrets.DT)

# plotting
{
    ggplot(bayesian.retrace.DT,
           aes(x = trial)) +
    geom_line(aes(y = FID.optimal, color = color), size = 1) +
    geom_point(aes(y = FID, color = color), shape = 1) +
    geom_text(aes(label = display), x = 50, y = 10, size = 3, data = regrets.DT) +
    scale_colour_manual(values = c("dodgerblue2","orange", "red")) + 
    facet_wrap(~ subject) +
    ggtitle(paste("Retracing by subjects with choice model coefficients:",
                  "FID (dot) and its optimal alternative (line)", sep = "\n"))
} %>% print

{
    ggplot(bayesian.retrace.DT,
           aes(x = trial)) +
        geom_line(aes(y = total.util.optimal, color = color), size = 0.7) +
        geom_point(aes(y = total.util, color = color), shape = 1) +
        geom_text(aes(label = display), x = 50, y = -5, size = 3, data = regrets.DT) +
        scale_colour_manual(values = c("dodgerblue2","orange", "red")) + 
        facet_wrap(~ subject) +
        ggtitle(paste("Retracing by subjects with choice model coefficients:",
                      "Actual utility (dot) and its optimal alternative (line)", sep = "\n"))
} %>% print