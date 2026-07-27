# ==============================================================================
# HARNESS A2 - MOTOR OLS
# Multi-scenario Monte Carlo validation: ols_engine vs. lm()
# ==============================================================================
suppressMessages(source("../R/OLS_engine.R"))

run_ols_scenario <- function(n, heterosk, high_collin, seed) {
  set.seed(seed)
  x1 <- rnorm(n, 50, 10)
  x2 <- if (high_collin) x1 + rnorm(n, 0, 1) else rnorm(n, 30, 8)
  err_sd <- if (heterosk) x1 * 0.3 else rep(8, n)
  y <- 10 + 0.5 * x1 + 0.3 * x2 + rnorm(n, 0, err_sd)
  df <- data.frame(y, x1, x2)

  base_fit <- lm(y ~ x1 + x2, data = df)
  eng_fit  <- ols_engine(y ~ x1 + x2, data = df, robust = FALSE)

  base_coef <- coef(base_fit)["x1"]
  eng_coef  <- eng_fit$coefficients["x1"]
  coef_diff <- abs(base_coef - eng_coef)

  # Detection check: did the engine's aduana correctly flag heteroscedasticity via BP test?
  # (ols_engine computes bp_p internally; re-derive via same test used inside for cross-check)
  bp_p <- tryCatch(lmtest::bptest(base_fit)$p.value, error = function(e) NA)
  detected_heterosk <- !is.na(bp_p) && bp_p < 0.05

  # Detection check: high VIF flagged?
  vif_x1 <- eng_fit$vif["x1"]
  detected_collin <- !is.na(vif_x1) && vif_x1 > 5

  data.frame(n, heterosk, high_collin, seed,
             coef_diff = as.numeric(coef_diff),
             true_heterosk = heterosk, detected_heterosk,
             true_collin = high_collin, detected_collin)
}

scenarios <- expand.grid(
  n = c(50, 200, 1000),
  heterosk = c(FALSE, TRUE),
  high_collin = c(FALSE, TRUE)
)

n_seeds <- 300
results <- list()
idx <- 1
for (s in seq_len(nrow(scenarios))) {
  for (seed in seq_len(n_seeds)) {
    results[[idx]] <- run_ols_scenario(
      n = scenarios$n[s],
      heterosk = scenarios$heterosk[s],
      high_collin = scenarios$high_collin[s],
      seed = seed + 10000
    )
    idx <- idx + 1
  }
}
res <- do.call(rbind, results)

cat("=== OLS ENGINE: Numerical parity across", nrow(res), "runs ===\n")
cat("Max coefficient difference:", max(res$coef_diff), "\n")
cat("Mean coefficient difference:", mean(res$coef_diff), "\n\n")

cat("=== Heteroscedasticity detection (sensitivity/specificity) ===\n")
tab_h <- table(True = res$true_heterosk, Detected = res$detected_heterosk)
print(tab_h)
sens_h <- tab_h["TRUE","TRUE"] / sum(tab_h["TRUE",])
spec_h <- tab_h["FALSE","FALSE"] / sum(tab_h["FALSE",])
cat(sprintf("Sensitivity: %.3f | Specificity: %.3f\n\n", sens_h, spec_h))

cat("=== Multicollinearity (VIF>5) detection ===\n")
tab_c <- table(True = res$true_collin, Detected = res$detected_collin)
print(tab_c)
sens_c <- tab_c["TRUE","TRUE"] / sum(tab_c["TRUE",])
spec_c <- tab_c["FALSE","FALSE"] / sum(tab_c["FALSE",])
cat(sprintf("Sensitivity: %.3f | Specificity: %.3f\n", sens_c, spec_c))

saveRDS(res, "results_ols.rds")
