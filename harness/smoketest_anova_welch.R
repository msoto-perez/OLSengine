# ==============================================================================
# SMOKE TEST - ANOVA auto-pilot: Kruskal-Wallis vs. Welch's ANOVA branch
# (SoftwareX review point 2). Manual, ad-hoc scenarios -- NOT part of the
# Monte Carlo validation suite. Confirms the three decision branches added to
# anova_engine()'s "auto" path each fire and message correctly:
#   a) non-normal + clearly heterogeneous variances (levene_p < 0.01) -> Welch
#   b) non-normal + gray-zone variances (0.01 <= levene_p <= 0.10)   -> Kruskal-Wallis (with caveat)
#   c) non-normal + homogeneous variances (levene_p > 0.10)          -> Kruskal-Wallis (as before)
# ==============================================================================
suppressMessages(source("../R/OLS_engine.R"))

run_case <- function(label, df) {
  res <- anova_engine(y ~ group, data = df, non_parametric = "auto")
  cat("\n===", label, "===\n")
  cat("n =", nrow(df), " | groups:", paste(levels(df$group), collapse = ", "), "\n")
  cat("test_name executed:", res$effects_table$Test, "\n")
  cat("norm_p =", signif(res$diagnostics$norm_p_value, 4),
      " | levene_p =", signif(res$diagnostics$levene_p_value, 4), "\n")
  cat("aduana_msgs:\n")
  for (m in res$aduana_msgs) cat("  -", m, "\n")
  invisible(res)
}

# ------------------------------------------------------------------
# (a) Clearly heterogeneous variances -> force the Welch branch.
# Three groups with very different spread (sd 1 / 5 / 25); the mixture of
# very-different-scale normals also reliably fails Shapiro-Wilk on the
# pooled residuals, so norm_p < .05 is expected alongside levene_p < .01.
# ------------------------------------------------------------------
set.seed(101)
n_per_group <- 40
df_a <- data.frame(
  y = c(rnorm(n_per_group, 10, 1),
        rnorm(n_per_group, 10, 5),
        rnorm(n_per_group, 10, 25)),
  group = factor(rep(c("A", "B", "C"), each = n_per_group))
)
run_case("(a) Heterogeneous variances -> expect Welch's ANOVA", df_a)

# ------------------------------------------------------------------
# (b) Deliberate gray zone: search seeds/sd-gap until levene_p lands in
# [0.01, 0.10] AND norm_p < .05 (both required to reach the 3-way branch
# at all). Small sd gap (1 vs 2.2) makes the gray zone attainable but not
# guaranteed on any single seed, hence the search.
# ------------------------------------------------------------------
find_gray_zone <- function(max_tries = 5000) {
  n_per_group <- 30
  for (s in seq_len(max_tries)) {
    set.seed(s)
    df_try <- data.frame(
      y = c(rexp(n_per_group, rate = 1) * 3,      # skewed group 1
            rexp(n_per_group, rate = 1) * 3.6,    # skewed group 2, mild sd gap
            rexp(n_per_group, rate = 1) * 3.2),   # skewed group 3
      group = factor(rep(c("A", "B", "C"), each = n_per_group))
    )
    medians <- tapply(df_try$y, df_try$group, median)
    abs_dev <- abs(df_try$y - medians[df_try$group])
    levene_p <- summary(aov(abs_dev ~ df_try$group))[[1]]$`Pr(>F)`[1]
    norm_p <- shapiro.test(residuals(aov(y ~ group, data = df_try)))$p.value
    if (norm_p < 0.05 && levene_p >= 0.01 && levene_p <= 0.10) {
      return(list(seed = s, df = df_try, levene_p = levene_p, norm_p = norm_p))
    }
  }
  stop("No seed found in gray zone within max_tries -- widen the search or sd gap.")
}

gray <- find_gray_zone()
cat("\n[gray-zone search] found at seed =", gray$seed,
    " (levene_p =", signif(gray$levene_p, 4), ", norm_p =", signif(gray$norm_p, 4), ")\n")
run_case("(b) Gray-zone variances -> expect Kruskal-Wallis with 'inconclusive' caveat", gray$df)

# ------------------------------------------------------------------
# (c) Genuine non-normality (skew), homogeneous variances -> Kruskal-Wallis
# should still fire exactly as before. Exponential groups scaled to equal
# variance (rate differs, scale factor equalizes sd across groups).
# ------------------------------------------------------------------
set.seed(202)
n_per_group <- 40
df_c <- data.frame(
  y = c(rexp(n_per_group, rate = 1) * 5,   # sd ~= 5
        rexp(n_per_group, rate = 2) * 10,  # sd ~= 5
        rexp(n_per_group, rate = 4) * 20), # sd ~= 5
  group = factor(rep(c("A", "B", "C"), each = n_per_group))
)
run_case("(c) Homogeneous variances, genuine skew -> expect Kruskal-Wallis (unchanged)", df_c)
