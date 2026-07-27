# ==============================================================================
# HARNESS A2 - MOTOR ANOVA
# Multi-scenario Monte Carlo validation: anova_engine vs. aov()
# Tests: (1) numerical parity (parametric path), (2) Shapiro-Wilk auto-detection
#        of non-normality, (3) Levene homogeneity-of-variance flagging
# ==============================================================================
suppressMessages(source("../R/OLS_engine.R"))

make_anova_data <- function(n_per_group, non_normal, heterogeneous_var, seed) {
  set.seed(seed)
  group <- factor(rep(c("A", "B", "C"), each = n_per_group))
  if (non_normal) {
    y <- c(rexp(n_per_group, 0.2), rexp(n_per_group, 0.25), rexp(n_per_group, 0.22))
  } else {
    sds <- if (heterogeneous_var) c(2, 8, 15) else c(5, 5, 5)
    y <- c(rnorm(n_per_group, 10, sds[1]),
           rnorm(n_per_group, 12, sds[2]),
           rnorm(n_per_group, 11, sds[3]))
  }
  data.frame(y, group)
}

run_anova_scenario <- function(n_per_group, non_normal, heterogeneous_var, seed) {
  df <- make_anova_data(n_per_group, non_normal, heterogeneous_var, seed)

  base_fit <- aov(y ~ group, data = df)
  base_F <- summary(base_fit)[[1]]$`F value`[1]

  eng_fit <- anova_engine(y ~ group, data = df, non_parametric = FALSE)
  eng_F <- eng_fit$effects_table$Statistic

  f_diff <- abs(base_F - eng_F)

  eng_auto <- anova_engine(y ~ group, data = df, non_parametric = "auto")
  detected_nonnormal <- eng_auto$diagnostics$norm_p_value < 0.05
  detected_heterog   <- eng_auto$diagnostics$levene_p_value < 0.05

  data.frame(n_per_group, non_normal, heterogeneous_var, seed,
             f_diff = as.numeric(f_diff),
             true_nonnormal = non_normal, detected_nonnormal,
             true_heterog = heterogeneous_var, detected_heterog)
}

scenarios <- expand.grid(
  n_per_group = c(15, 50, 150),
  non_normal = c(FALSE, TRUE),
  heterogeneous_var = c(FALSE, TRUE)
)
scenarios <- scenarios[!(scenarios$non_normal & scenarios$heterogeneous_var), ] # keep orthogonal, avoid confound

n_seeds <- 250
results <- list()
idx <- 1
for (s in seq_len(nrow(scenarios))) {
  for (seed in seq_len(n_seeds)) {
    results[[idx]] <- run_anova_scenario(scenarios$n_per_group[s], scenarios$non_normal[s],
                                          scenarios$heterogeneous_var[s], seed + 40000)
    idx <- idx + 1
  }
}
res <- do.call(rbind, results)

cat("=== ANOVA ENGINE: numerical parity vs aov(), n runs =", nrow(res), "===\n")
cat("Max F-stat difference:", max(res$f_diff), "\n\n")

cat("=== Non-normality detection (Shapiro-Wilk), by n per group ===\n")
print(aggregate(detected_nonnormal ~ n_per_group + true_nonnormal, data = res, FUN = mean))

cat("\n=== Heterogeneous variance detection (Levene), by n per group ===\n")
print(aggregate(detected_heterog ~ n_per_group + true_heterog, data = res, FUN = mean))

saveRDS(res, "results_anova.rds")
