# ------------------------------------------------------------------------------
# Replication of Ockenfels, Sliwka & Werner (2025), Table 3: Signals and
# supervisor decisions. Random-effects panel model (group x round), standard
# errors clustered at the group level (Stata-style small-sample correction).
# Run from the project folder; the data file is expected in data/.
# Packages (install once): haven, plm, lmtest, car, flextable
# ------------------------------------------------------------------------------
library(haven); library(plm); library(lmtest); library(car); library(flextable)
dir.create("output", showWarnings = FALSE)

# 1. Data ----------------------------------------------------------------------
data <- read_dta("data/Replication_Data_OSW_Multi-rater.dta")
t3 <- subset(data, agent == 0)                                   # supervisors only

# signal used for the rating: 1Su1Si = first signal, 1Su3Si = mean of the three
# signals, 3Su3Si = mean of the three supervisors' first signals (per group, round)
t3$used_signal <- ifelse(t3$treatment == 1, t3$first_signal,
                         (t3$first_signal + t3$second_signal + t3$third_signal) / 3)
mean_first <- ave(t3$first_signal, t3$group_id, t3$period)
t3$used_signal[t3$treatment == 3] <- mean_first[t3$treatment == 3]
t3 <- subset(t3, !(treatment == 3 & employer > 1))              # one supervisor per 3Su3Si group
stopifnot(nrow(t3) == 730)                                       # check against the paper

t3$used_signalXtr_1Su3Si <- t3$used_signal * t3$tr_1Su3Si
t3$used_signalXtr_3Su3Si <- t3$used_signal * t3$tr_3Su3Si

# 2. Models --------------------------------------------------------------------
re <- function(f, d) plm(f, data = d, index = c("group_id", "period"), model = "random")
models <- list(
  "1Su1Si (1)" = re(rating ~ used_signal, subset(t3, treatment == 1)),
  "1Su3Si (2)" = re(rating ~ used_signal, subset(t3, treatment == 2)),
  "3Su3Si (3)" = re(rating ~ used_signal, subset(t3, treatment == 3)),
  "All (4)"    = re(rating ~ used_signal + tr_1Su3Si + used_signalXtr_1Su3Si +
                      tr_3Su3Si + used_signalXtr_3Su3Si, t3)
)
vc <- function(m) plm::vcovHC(m, type = "sss", cluster = "group")   # clustered SE as in Stata

# Wald test: equal interaction effects (paper: p = 0.457)
linearHypothesis(models[[4]], "used_signalXtr_1Su3Si = used_signalXtr_3Su3Si",
                 vcov. = vc(models[[4]]), test = "Chisq")

# 3. Table 3 (all numbers computed from the models) -----------------------------
vars  <- c(used_signal = "signal", tr_1Su3Si = "1Su3Si", used_signalXtr_1Su3Si = "signal × 1Su3Si",
           tr_3Su3Si = "3Su3Si", used_signalXtr_3Su3Si = "signal × 3Su3Si", `(Intercept)` = "Constant")
# stars from normal (z) p-values, as in Stata's xtreg, re
stars <- function(p) ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .1, "*", "")))

column <- function(m) {
  ct <- coeftest(m, vcov. = vc(m))
  est <- unlist(lapply(names(vars), function(v)
    if (v %in% rownames(ct)) c(sprintf("%.3f%s", ct[v, 1], stars(2 * pnorm(-abs(ct[v, 3])))), sprintf("[%.3f]", ct[v, 2]))
    else c("", "")))
  slopes <- setdiff(names(coef(m)), "(Intercept)")
  chi2 <- linearHypothesis(m, slopes, vcov. = vc(m), test = "Chisq")$Chisq[2]
  c(est, as.character(nobs(m)), sprintf("%.2f", chi2))
}
tab3 <- data.frame(Variable = c(as.vector(rbind(unname(vars), "")), "Observations", "Chi-squared"),
                   sapply(models, column), check.names = FALSE)
print(tab3, row.names = FALSE)

ft <- flextable(tab3)
ft <- set_header_labels(ft, Variable = "Dependent Variable")
ft <- add_header_row(ft, values = c("", "Rating"), colwidths = c(1, 4))
ft <- theme_booktabs(ft)
ft <- align(autofit(ft), j = 2:5, align = "center", part = "all")
ft
save_as_docx(ft, path = "output/Table3_Replication.docx")
save_as_image(ft, path = "output/Table3_Replication.png")
