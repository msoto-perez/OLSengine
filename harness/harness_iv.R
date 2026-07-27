# ==============================================================================
# HARNESS A2 - MOTOR IV/2SLS
# Multi-scenario Monte Carlo validation: iv_engine vs. AER::ivreg()
# Tests: (1) numerical parity, (2) weak-instrument detection (F<10 threshold),
#        (3) Sargan overidentification test power/specificity (valid vs invalid instrument)
# ==============================================================================
suppressMessages(source("../R/OLS_engine.R"))
suppressMessages(library(AER))

make_iv_data <- function(n, instrument_strength, overid, invalid_instrument, seed) {
  set.seed(seed)
  u <- rnorm(n, 0, 2)                      # structural error (drives endogeneity)
  gamma <- if (instrument_strength == "strong") 0.9 else 0.05

  z1 <- rnorm(n, 10, 3)
  x  <- 2 + gamma * z1 + u + rnorm(n, sd = 1)
  y  <- 5 + 1.5 * x + u + rnorm(n, sd = 3)

  df <- data.frame(y, x, z1)

  if (overid) {
    z2 <- rnorm(n, 5, 2)
    if (invalid_instrument) {
      # z2 correlated with structural error u -> violates exogeneity
      z2 <- z2 + 0.6 * u
    }
    df$z2 <- z2
  }
  df
}

run_iv_scenario <- function(n, instrument_strength, overid, invalid_instrument, seed) {
  df <- make_iv_data(n, instrument_strength, overid, invalid_instrument, seed)

  if (overid) {
    base_fit <- tryCatch(ivreg(y ~ x | z1 + z2, data = df), error = function(e) NULL)
    eng_fit  <- tryCatch(iv_engine(y ~ x, data = df, instruments = ~ z1 + z2), error = function(e) NULL)
  } else {
    base_fit <- tryCatch(ivreg(y ~ x | z1, data = df), error = function(e) NULL)
    eng_fit  <- tryCatch(iv_engine(y ~ x, data = df, instruments = ~ z1), error = function(e) NULL)
  }

  coef_diff <- if (!is.null(base_fit) && !is.null(eng_fit)) {
    abs(coef(base_fit)["x"] - eng_fit$coefficients["x"])
  } else NA

  # weak-instrument detection: engine flags if min first-stage F < 10
  min_fstat <- if (!is.null(eng_fit)) min(eng_fit$first_stage_fstat) else NA
  true_weak <- (instrument_strength == "weak")
  detected_weak <- if (!is.null(eng_fit)) (min_fstat < 10) else NA

  # Sargan test (only meaningful when overid = TRUE)
  sargan_p <- if (overid && !is.null(eng_fit)) eng_fit$sargan_p else NA
  sargan_flagged_invalid <- if (overid && !is.null(eng_fit)) (sargan_p < 0.05) else NA

  data.frame(n, instrument_strength, overid, invalid_instrument, seed,
             coef_diff = as.numeric(coef_diff),
             min_fstat, true_weak, detected_weak,
             sargan_p, sargan_flagged_invalid)
}

# --- Scenario set 1: parity + weak-instrument detection (exact identification) ---
scenarios1 <- expand.grid(
  n = c(100, 300, 1000),
  instrument_strength = c("strong", "weak"),
  overid = FALSE,
  invalid_instrument = FALSE
)

# --- Scenario set 2: Sargan test, overidentified, valid vs invalid second instrument ---
scenarios2 <- expand.grid(
  n = c(100, 300, 1000),
  instrument_strength = "strong",
  overid = TRUE,
  invalid_instrument = c(FALSE, TRUE)
)

scenarios <- rbind(scenarios1, scenarios2)

n_seeds <- 200
results <- list()
idx <- 1
for (s in seq_len(nrow(scenarios))) {
  for (seed in seq_len(n_seeds)) {
    results[[idx]] <- tryCatch(
      run_iv_scenario(scenarios$n[s], as.character(scenarios$instrument_strength[s]),
                       scenarios$overid[s], scenarios$invalid_instrument[s], seed + 30000),
      error = function(e) NULL
    )
    idx <- idx + 1
  }
}
res <- do.call(rbind, results[!sapply(results, is.null)])

cat("=== IV ENGINE: numerical parity vs AER::ivreg(), n runs =", nrow(res), "===\n")
cat("Max coefficient difference:", max(res$coef_diff, na.rm=TRUE), "\n")
cat("Mean coefficient difference:", mean(res$coef_diff, na.rm=TRUE), "\n\n")

cat("=== Weak-instrument detection (exact identification scenarios only) ===\n")
res_exact <- res[!res$overid, ]
tab_w <- table(True = res_exact$true_weak, Detected = res_exact$detected_weak)
print(tab_w)
sens_w <- tab_w["TRUE","TRUE"] / sum(tab_w["TRUE",])
spec_w <- tab_w["FALSE","FALSE"] / sum(tab_w["FALSE",])
cat(sprintf("Sensitivity: %.3f | Specificity: %.3f\n\n", sens_w, spec_w))

cat("=== Sargan overidentification test: valid vs invalid 2nd instrument ===\n")
res_over <- res[res$overid, ]
tab_s <- table(InvalidInstrument = res_over$invalid_instrument, FlaggedBySargan = res_over$sargan_flagged_invalid)
print(tab_s)
cat(sprintf("Power (detects invalid instrument): %.3f\n", tab_s["TRUE","TRUE"]/sum(tab_s["TRUE",])))
cat(sprintf("False positive rate (valid instrument flagged): %.3f\n", tab_s["FALSE","TRUE"]/sum(tab_s["FALSE",])))

saveRDS(res, "results_iv.rds")
