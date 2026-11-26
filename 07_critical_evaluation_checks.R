# ------------------------------------------------------------------------------
# Checks for the critical evaluation (Section 4)
#  (1) Omitted variables: do observable supervisor traits change the signal slopes?
#  (2) Reverse causality / feedback: does last round's rating predict this
#      round's employee performance (and hence the signal)?
#  (3) Selection in 3Su3Si: do the three supervisors of a team submit the same rating?
#  (4) Learning: signal slopes in the first (1-5) vs. second half (6-10)
#  (5) Omitted variables: correlation between a group's mean signal and supervisor traits
# Run from the project folder; the data file is expected in data/.
# Packages (install once): haven, plm, lmtest
# ------------------------------------------------------------------------------
library(haven); library(plm); library(lmtest)
data <- read_dta("data/Replication_Data_OSW_Multi-rater.dta")

# supervisor traits are recorded only once per subject -> copy them to all rounds
traits <- c("female", "crt_score", "raven", "altruism1", "wiso", "age")
for (v in traits) data[[v]] <- ave(data[[v]], data$session, data$subject_id,
                                   FUN = function(z) if (all(is.na(z))) NA else max(z, na.rm = TRUE))

t3 <- subset(data, agent == 0)
t3$used_signal <- ifelse(t3$treatment == 1, t3$first_signal,
                         (t3$first_signal + t3$second_signal + t3$third_signal) / 3)
mean_first <- ave(t3$first_signal, t3$group_id, t3$period)
t3$used_signal[t3$treatment == 3] <- mean_first[t3$treatment == 3]

# (3) before dropping: same rating by all three supervisors of a 3Su3Si team?
t3_team <- subset(t3, treatment == 3)
same <- tapply(t3_team$rating, paste(t3_team$group_id, t3_team$period), function(x) length(unique(x)) == 1)
cat("(3) Share of 3Su3Si team-rounds with identical ratings:", mean(same), "\n")

t3 <- subset(t3, !(treatment == 3 & employer > 1))
t3 <- t3[order(t3$group_id, t3$period), ]
t3$rating_lag <- ave(t3$rating, t3$group_id, FUN = function(x) c(NA, head(x, -1)))
t3$s1 <- t3$used_signal * t3$tr_1Su3Si
t3$s3 <- t3$used_signal * t3$tr_3Su3Si
pd <- pdata.frame(as.data.frame(t3), index = c("group_id", "period"))
vc <- function(m) plm::vcovHC(m, type = "sss", cluster = "group")
f0 <- rating ~ used_signal + tr_1Su3Si + s1 + tr_3Su3Si + s3

# (1) omitted variables: same sample with and without supervisor traits
d1 <- pd[complete.cases(as.data.frame(pd)[, traits]), ]
m0 <- plm(f0, data = d1, model = "random")
m1 <- plm(update(f0, as.formula(paste(". ~ . +", paste(traits, collapse = " + ")))), data = d1, model = "random")
cat("\n(1) Signal slopes without / with supervisor traits (N =", nobs(m1), ")\n")
print(round(cbind(without = coef(m0)[c("used_signal", "s1", "s3")],
                  with_traits = coef(m1)[c("used_signal", "s1", "s3")]), 3))
print(round(coeftest(m1, vcov. = vc(m1))[traits, ], 3))

# (2) feedback: performance_t on rating_(t-1), group and round fixed effects
for (r in list(2:10, 7:10)) {
  d <- pd[as.numeric(as.character(pd$period)) %in% r, ]
  m <- plm(performance ~ rating_lag, data = d, model = "within", effect = "twoways")
  cat("\n(2) Feedback, rounds", min(r), "to", max(r), "\n"); print(round(coeftest(m, vcov. = vc(m)), 3))
}

# (4) learning: column (4) of Table 3 separately for both halves
for (r in list(1:5, 6:10)) {
  m <- plm(f0, data = pd[as.numeric(as.character(pd$period)) %in% r, ], model = "random")
  cat("\n(4) Rounds", min(r), "to", max(r), "\n")
  print(round(coeftest(m, vcov. = vc(m))[c("used_signal", "s1", "s3"), c(1, 2, 4)], 3))
}

# (5) correlation between a group's mean signal and supervisor traits (one row per group)
g <- aggregate(t3[, c("used_signal", traits)], by = list(group = t3$group_id), FUN = mean)
cat("\n(5) Correlation of a group's mean signal with supervisor traits\n")
for (v in traits) {
  ct <- cor.test(g$used_signal, g[[v]])
  cat(sprintf("%-10s r = %6.3f   p = %.3f   groups = %d\n", v, ct$estimate, ct$p.value,
              sum(complete.cases(g[, c("used_signal", v)]))))
}
