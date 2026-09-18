# ==============================================================================
# FOLLOW-UP - Refines the Type-I error estimate for Welch's ANOVA in the
# NON_UNEQ cell (non-normal + ~5:1 unequal variance) from
# harness_anova_welch_2x2.R, at 2,000 reps instead of 250, for a tighter CI
# to cite in the paper. Same DGP, same seed scheme, same design parameters
# as that script's NON_UNEQ cell -- only the rep count changes. The other
# three cells are untouched (not re-run here).
# ==============================================================================
suppressMessages(source("../R/OLS_engine.R"))

n_per_group <- 50
n_reps <- 2000
mu <- 50

# Identical DGP to harness_anova_welch_2x2.R's NON_UNEQ cell:
# non_normal = TRUE, unequal_var = TRUE, sds = c(1, 3, 5) (max/min = 5:1)
make_data_non_uneq <- function(seed) {
  set.seed(seed)
  sds <- c(1, 3, 5)
  y <- unlist(lapply(sds, function(s) rexp(n_per_group, rate = 1) * s - s))
  data.frame(y = mu + y, group = factor(rep(c("A", "B", "C"), each = n_per_group)))
}

# Same seed formula as the original script (cell index 4 -> base 4000),
# now extended to 2000 reps instead of 250.
branch <- character(n_reps)
norm_p <- numeric(n_reps)
levene_p <- numeric(n_reps)
p_val <- numeric(n_reps)

for (i in seq_len(n_reps)) {
  df <- make_data_non_uneq(seed = 4000 + i)
  res <- anova_engine(y ~ group, data = df, non_parametric = "auto")
  branch[i] <- res$effects_table$Test
  norm_p[i] <- res$diagnostics$norm_p_value
  levene_p[i] <- res$diagnostics$levene_p_value
  p_val[i] <- res$effects_table$p_value
}

res_df <- data.frame(branch, norm_p, levene_p, p_val)

cat("=== NON_UNEQ cell (non-normal + ~5:1 unequal var), n =", n_reps, "reps ===\n")
cat("Branch frequency:\n")
print(table(res_df$branch))

welch_rows <- res_df[res_df$branch == "Welch's ANOVA", ]
cat("\nRuns where Welch fired:", nrow(welch_rows), "of", n_reps, "\n")
n_reject <- sum(welch_rows$p_val < 0.05)
type1 <- n_reject / nrow(welch_rows)
ci <- prop.test(n_reject, nrow(welch_rows))$conf.int
cat(sprintf("Empirical Type-I error rate (p < .05): %.4f (%d / %d)\n", type1, n_reject, nrow(welch_rows)))
cat(sprintf("95%% CI (Wilson, via prop.test): [%.4f, %.4f]\n", ci[1], ci[2]))

saveRDS(res_df, "results_anova_welch_nonuneq_2000.rds")
