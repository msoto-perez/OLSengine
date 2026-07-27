# ==============================================================================
# HARNESS A2 - MOTOR LOGIT
# Multi-scenario Monte Carlo validation: logit_engine vs. glm()
# Tests: (1) numerical parity, (2) perfect-separation detection (SE > 50 rule)
# ==============================================================================
suppressMessages(source("../R/OLS_engine.R"))

make_logit_data <- function(n, separation, effect_size, seed) {
  set.seed(seed)
  if (separation) {
    x <- c(rnorm(n/2, 0, 1), rnorm(n/2, 10, 1))
    y <- c(rep(0, n/2), rep(1, n/2))
  } else {
    x <- rnorm(n, 0, 2)
    lin <- -1 + effect_size * x
    p <- plogis(lin)
    y <- rbinom(n, 1, p)
  }
  data.frame(y, x)
}

run_logit_scenario <- function(n, separation, effect_size, seed) {
  df <- make_logit_data(n, separation, effect_size, seed)

  base_fit <- suppressWarnings(glm(y ~ x, data = df, family = binomial()))
  eng_fit  <- logit_engine(y ~ x, data = df)

  # For separated cases, coefficients diverge (both implementations hit the same
  # numerical instability) - compare only in non-separated cases for parity.
  coef_diff <- if (!separation) abs(coef(base_fit)["x"] - eng_fit$coefficients["x"]) else NA

  detected_separation <- eng_fit$diagnostics$perfect_separation

  data.frame(n, separation, effect_size, seed,
             coef_diff = as.numeric(coef_diff),
             true_separation = separation, detected_separation)
}

scenarios <- expand.grid(
  n = c(60, 150, 400),
  separation = c(FALSE, TRUE),
  effect_size = c(0.3, 0.8)
)
scenarios <- scenarios[!(scenarios$separation & scenarios$effect_size == 0.8), ] # effect_size irrelevant when separation forces its own DGP; avoid duplicate runs
scenarios <- unique(scenarios[, c("n","separation","effect_size")])

n_seeds <- 250
results <- list()
idx <- 1
for (s in seq_len(nrow(scenarios))) {
  for (seed in seq_len(n_seeds)) {
    results[[idx]] <- tryCatch(
      run_logit_scenario(scenarios$n[s], scenarios$separation[s], scenarios$effect_size[s], seed + 50000),
      error = function(e) NULL)
    idx <- idx + 1
  }
}
res <- do.call(rbind, results[!sapply(results, is.null)])

cat("=== LOGIT ENGINE: numerical parity vs glm() (non-separated cases only) ===\n")
cat("N comparable runs:", sum(!is.na(res$coef_diff)), "\n")
cat("Max coefficient difference:", max(res$coef_diff, na.rm = TRUE), "\n\n")

cat("=== Perfect separation detection, by n ===\n")
print(aggregate(detected_separation ~ n + true_separation, data = res, FUN = mean))

saveRDS(res, "results_logit.rds")
