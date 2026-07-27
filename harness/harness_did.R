# ==============================================================================
# HARNESS A2 - MOTOR DiD
# Multi-scenario Monte Carlo validation: did_engine vs. lm() with dummies
# Tests: (1) numerical parity, (2) pre-trends placebo test detection accuracy
#        (balanced vs unbalanced group sizes)
# ==============================================================================
suppressMessages(source("../R/OLS_engine.R"))

make_did_data <- function(n_per_cell, violated_pretrend, balanced, seed) {
  set.seed(seed)

  if (balanced) {
    n_treated <- n_per_cell
    n_control <- n_per_cell
  } else {
    n_treated <- round(n_per_cell * 0.4)
    n_control <- round(n_per_cell * 1.6)
  }

  # Pre-treatment gap between groups: 0 if parallel trends hold, nonzero if violated
  pre_gap <- if (violated_pretrend) 8 else 0

  treated_pre  <- rep(1, n_treated); post_pre_t  <- rep(0, n_treated)
  treated_post <- rep(1, n_treated); post_post_t <- rep(1, n_treated)
  control_pre  <- rep(0, n_control); post_pre_c  <- rep(0, n_control)
  control_post <- rep(0, n_control); post_post_c <- rep(1, n_control)

  y_treated_pre  <- 50 + pre_gap + rnorm(n_treated, 0, 4)
  y_treated_post <- 50 + pre_gap + 3 + 5 + rnorm(n_treated, 0, 4)  # +3 time trend, +5 true treatment effect
  y_control_pre  <- 50 + rnorm(n_control, 0, 4)
  y_control_post <- 50 + 3 + rnorm(n_control, 0, 4)

  data.frame(
    y = c(y_treated_pre, y_treated_post, y_control_pre, y_control_post),
    treated_num = c(treated_pre, treated_post, control_pre, control_post),
    post_num = c(post_pre_t, post_post_t, post_pre_c, post_post_c)
  )
}

run_did_scenario <- function(n_per_cell, violated_pretrend, balanced, seed) {
  df <- make_did_data(n_per_cell, violated_pretrend, balanced, seed)
  df$interaction <- df$treated_num * df$post_num
  df$treated_f <- factor(df$treated_num, levels = c(0,1), labels = c("Control","Treated"))
  df$post_f    <- factor(df$post_num,    levels = c(0,1), labels = c("Pre","Post"))

  base_fit <- lm(y ~ treated_num + post_num + interaction, data = df)
  base_did <- coef(base_fit)["interaction"]

  eng_fit <- did_engine(y ~ 1, data = df, treatment_var = "treated_f", time_var = "post_f",
                         treatment_level = "Treated", post_level = "Post")
  eng_did <- eng_fit$did_estimate

  did_diff <- abs(base_did - eng_did)

  # Did the engine's placebo test flag the pre-trend violation?
  flagged <- any(grepl("Parallel Trends.*Significant", eng_fit$aduana_msgs))

  data.frame(n_per_cell, violated_pretrend, balanced, seed,
             did_diff = as.numeric(did_diff),
             true_violation = violated_pretrend, detected_violation = flagged)
}

scenarios <- expand.grid(
  n_per_cell = c(30, 100, 300),
  violated_pretrend = c(FALSE, TRUE),
  balanced = c(TRUE, FALSE)
)

n_seeds <- 250
results <- list()
idx <- 1
for (s in seq_len(nrow(scenarios))) {
  for (seed in seq_len(n_seeds)) {
    results[[idx]] <- tryCatch(
      run_did_scenario(scenarios$n_per_cell[s], scenarios$violated_pretrend[s],
                        scenarios$balanced[s], seed + 60000),
      error = function(e) NULL)
    idx <- idx + 1
  }
}
res <- do.call(rbind, results[!sapply(results, is.null)])

cat("=== DiD ENGINE: numerical parity vs lm() w/ dummies, n runs =", nrow(res), "===\n")
cat("Max DiD-estimate difference:", max(res$did_diff), "\n\n")

cat("=== Pre-trends violation detection, by n and balance ===\n")
print(aggregate(detected_violation ~ n_per_cell + true_violation + balanced, data = res, FUN = mean))

saveRDS(res, "results_did.rds")
