# ------------------------------------------------------------------------------
# Extension: Fixed effects (FE) versus random effects (RE), with Hausman test.
# The authors estimate Table 3 with random effects, which requires that the
# group effect alpha_i is uncorrelated with the signal. FE does not need this.
# We re-estimate all four columns of Table 3 with FE and test RE against FE.
# Run from the project folder; the data file is expected in data/.
# Packages (install once): haven, plm, lmtest, flextable
# ------------------------------------------------------------------------------
library(haven); library(plm); library(lmtest); library(flextable)
dir.create("output", showWarnings = FALSE)

# 1. Data (same construction as for Table 3) ------------------------------------
data <- read_dta("data/Replication_Data_OSW_Multi-rater.dta")
t3 <- subset(data, agent == 0)
t3$used_signal <- ifelse(t3$treatment == 1, t3$first_signal,
                         (t3$first_signal + t3$second_signal + t3$third_signal) / 3)
mean_first <- ave(t3$first_signal, t3$group_id, t3$period)
t3$used_signal[t3$treatment == 3] <- mean_first[t3$treatment == 3]
t3 <- subset(t3, !(treatment == 3 & employer > 1))
t3$used_signalXtr_1Su3Si <- t3$used_signal * t3$tr_1Su3Si
t3$used_signalXtr_3Su3Si <- t3$used_signal * t3$tr_3Su3Si

# 2. Models: RE and FE for each column of Table 3 -------------------------------
f_single <- rating ~ used_signal
f_all_re <- rating ~ used_signal + tr_1Su3Si + used_signalXtr_1Su3Si + tr_3Su3Si + used_signalXtr_3Su3Si
f_all_fe <- rating ~ used_signal + used_signalXtr_1Su3Si + used_signalXtr_3Su3Si  # treatment dummies are absorbed by the group effect
specs <- list(
  "1Su1Si (1)" = list(d = subset(t3, treatment == 1), f_re = f_single, f_fe = f_single),
  "1Su3Si (2)" = list(d = subset(t3, treatment == 2), f_re = f_single, f_fe = f_single),
  "3Su3Si (3)" = list(d = subset(t3, treatment == 3), f_re = f_single, f_fe = f_single),
  "All (4)"    = list(d = t3, f_re = f_all_re, f_fe = f_all_fe)
)
fit <- function(f, d, model) plm(f, data = d, index = c("group_id", "period"), model = model)
vc  <- function(m) plm::vcovHC(m, type = "sss", cluster = "group")      # clustered SE as in Table 3

# Hausman test, robust to clustering (regression-based version, "aux"; Wooldridge 2010).
# H0: RE is consistent (group effect uncorrelated with the regressors).
# The RE formula (incl. treatment dummies) is used; only time-varying terms are tested.
hausman <- function(s) phtest(s$f_re, data = s$d, index = c("group_id", "period"),
                              method = "aux", vcov = function(x) plm::vcovHC(x, type = "sss", cluster = "group"))

# 3. Table: panel A = RE, panel B = FE, Hausman p-value --------------------------
slopes <- c(used_signal = "Signal", used_signalXtr_1Su3Si = "Signal × 1Su3Si", used_signalXtr_3Su3Si = "Signal × 3Su3Si")
stars  <- function(p) ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .1, "*", "")))
cells  <- function(m) {
  ct <- coeftest(m, vcov. = vc(m))
  unlist(lapply(names(slopes), function(v)
    if (v %in% rownames(ct)) c(sprintf("%.3f%s", ct[v, 1], stars(2 * pnorm(-abs(ct[v, 3])))), sprintf("[%.3f]", ct[v, 2]))
    else c("", "")))
}
lab <- as.vector(rbind(unname(slopes), ""))
re_col <- sapply(specs, function(s) cells(fit(s$f_re, s$d, "random")))
fe_col <- sapply(specs, function(s) cells(fit(s$f_fe, s$d, "within")))
haus   <- sapply(specs, function(s) { h <- hausman(s); sprintf("%.2f (p = %.3f)", h$statistic, h$p.value) })
nobs_  <- sapply(specs, function(s) as.character(nrow(s$d)))

blank <- rep("", length(specs))
tab <- data.frame(
  Variable = c("Panel A: Random effects", lab, "Panel B: Fixed effects", lab,
               "Hausman test: chi2 (p-value)", "Observations"),
  rbind(blank, re_col, blank, fe_col, haus, nobs_),
  check.names = FALSE, row.names = NULL)
names(tab)[-1] <- names(specs)
print(tab, row.names = FALSE)

ft <- flextable(tab)
ft <- set_header_labels(ft, Variable = "Dependent Variable: Rating")
ft <- bold(ft, i = c(1, 2 + length(lab)), j = 1)
ft <- theme_booktabs(ft)
ft <- align(autofit(ft), j = 2:5, align = "center", part = "all")
ft
save_as_docx(ft, path = "output/Table_Extension_FE_vs_RE.docx")
save_as_image(ft, path = "output/Table_Extension_FE_vs_RE.png")
