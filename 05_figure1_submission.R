# ------------------------------------------------------------------------------
# Replication of Ockenfels, Sliwka & Werner (2025), Figure 1: predicted and
# fitted performance evaluations (rating vs. signal, by treatment).
# Run from the project folder; the data file is expected in data/.
# Packages (install once): haven, ggplot2, gridExtra
# ------------------------------------------------------------------------------
library(haven); library(ggplot2); library(gridExtra)
dir.create("output", showWarnings = FALSE)

# 1. Data (same construction as for Table 3) ------------------------------------
data <- read_dta("data/Replication_Data_OSW_Multi-rater.dta")
t3 <- subset(data, agent == 0)
t3$used_signal <- ifelse(t3$treatment == 1, t3$first_signal,
                         (t3$first_signal + t3$second_signal + t3$third_signal) / 3)
mean_first <- ave(t3$first_signal, t3$group_id, t3$period)
t3$used_signal[t3$treatment == 3] <- mean_first[t3$treatment == 3]
t3 <- subset(t3, !(treatment == 3 & employer > 1))

# 2. Bayesian benchmark (prior mean and variance of performance as in the do-file;
#    noise variance of a single signal = 9) ------------------------------------
mP <- 8.112329; vP <- 3.196103^2
pred_1 <- c(const = 9 * mP / (vP + 9),     slope = vP / (vP + 9))           # one signal
pred_3 <- c(const = 9 * mP / (3 * vP + 9), slope = 3 * vP / (3 * vP + 9))   # three signals

# 3. One panel per treatment: scatter + OLS line (fitted) + Bayesian line (predicted)
panel <- function(tr, title, pred) {
  d   <- subset(t3, treatment == tr)
  ols <- coef(lm(rating ~ used_signal, data = d))
  ggplot(d, aes(used_signal, rating)) +
    geom_point(size = 0.8, alpha = 0.7, colour = "grey35") +
    stat_function(fun = function(x) ols[1] + ols[2] * x, xlim = c(0, 20), aes(colour = "fitted")) +
    stat_function(fun = function(x) pred["const"] + pred["slope"] * x, xlim = c(0, 20), aes(colour = "predicted")) +
    scale_colour_manual(values = c(fitted = "black", predicted = "grey60"), name = NULL) +
    coord_cartesian(xlim = c(-1, 25), ylim = c(-1, 31)) +
    labs(title = title, x = "Signal", y = "Rating") +
    theme_classic(base_size = 10) +
    theme(legend.position = if (tr == 1) c(0.2, 0.9) else "none")
}
plots <- list(panel(1, "1Su1Si", pred_1), panel(3, "3Su3Si", pred_3), panel(2, "1Su3Si", pred_3))

png("output/Figure1_Replication.png", width = 2200, height = 760, res = 200)
grid.arrange(grobs = plots, ncol = 3)
dev.off()
