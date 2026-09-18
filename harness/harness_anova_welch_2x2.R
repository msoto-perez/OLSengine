# ==============================================================================
# HARNESS - ANOVA auto-pilot 2x2 Monte Carlo (SoftwareX review point 2 follow-up)
# Extends the 2,250-run ANOVA validation (paper Sec. 2.3) to cover the new
# Welch's ANOVA branch. Design: Normality {normal, non-normal} x Homogeneity
# {equal variance, ~5:1 unequal variance}, k=3 groups, n_per_group=50,
# 250 reps/cell (same rep count as the rest of the ANOVA harness), simulated
# under a TRUE NULL (equal population means in every group/cell) so branch
# frequencies and Type-I error rates are both directly readable.
# ==============================================================================
suppressMessages(source("../R/OLS_engine.R"))

n_per_group <- 50
n_reps <- 250
mu <- 50 # common population mean across all groups/cells -> null is always true

make_data <- function(non_normal, unequal_var, seed) {
  set.seed(seed)
  sds <- if (unequal_var) c(1, 3, 5) else c(5, 5, 5)  # max/min = 5:1 when unequal
  if (!non_normal) {
    y <- unlist(lapply(sds, function(s) rnorm(n_per_group, mean = 0, sd = s)))
  } else {
    # Exponential(rate=1) has mean 1, sd 1; scale by s and recenter so every
    # group keeps population mean 0 (added to mu below) with sd = s, but a
    # skewed (non-normal) shape.
    y <- unlist(lapply(sds, function(s) rexp(n_per_group, rate = 1) * s - s))
  }
  data.frame(y = mu + y, group = factor(rep(c("A", "B", "C"), each = n_per_group)))
}

run_cell <- function(cell_label, non_normal, unequal_var) {
  branch <- character(n_reps)
  norm_p <- numeric(n_reps)
  levene_p <- numeric(n_reps)
  p_val <- numeric(n_reps)

  for (i in seq_len(n_reps)) {
    df <- make_data(non_normal, unequal_var, seed = 1000 * which(c("NN_EQ","NN_UNEQ","NON_EQ","NON_UNEQ") == cell_label) + i)
    res <- anova_engine(y ~ group, data = df, non_parametric = "auto")
    branch[i] <- res$effects_table$Test
    norm_p[i] <- res$diagnostics$norm_p_value
    levene_p[i] <- res$diagnostics$levene_p_value
    p_val[i] <- res$effects_table$p_value
  }

  data.frame(cell = cell_label, branch = branch, norm_p = norm_p, levene_p = levene_p, p_val = p_val)
}

cells <- list(
  NN_EQ    = c(non_normal = FALSE, unequal_var = FALSE),  # Normal, Equal var
  NN_UNEQ  = c(non_normal = FALSE, unequal_var = TRUE),   # Normal, Unequal var
  NON_EQ   = c(non_normal = TRUE,  unequal_var = FALSE),  # Non-normal, Equal var
  NON_UNEQ = c(non_normal = TRUE,  unequal_var = TRUE)    # Non-normal, Unequal var
)

all_res <- do.call(rbind, lapply(names(cells), function(lbl) {
  cfg <- cells[[lbl]]
  run_cell(lbl, as.logical(cfg["non_normal"]), as.logical(cfg["unequal_var"]))
}))

# ------------------------------------------------------------------
# 1. Branch frequency by cell
# ------------------------------------------------------------------
cat("=== 1. Branch frequency by cell (n =", n_reps, "reps/cell) ===\n")
branch_tab <- table(all_res$cell, all_res$branch)
print(branch_tab)
cat("\nAs proportions:\n")
print(round(prop.table(branch_tab, margin = 1), 3))

# ------------------------------------------------------------------
# 2. Type-I error rate of Welch's ANOVA, conditional on Welch firing,
#    in the NON_UNEQ cell (non-normal + unequal variance).
# ------------------------------------------------------------------
welch_rows <- all_res[all_res$cell == "NON_UNEQ" & all_res$branch == "Welch's ANOVA", ]
cat("\n=== 2. Type-I error, Welch's ANOVA, cell NON_UNEQ (non-normal + unequal var) ===\n")
cat("Runs where Welch fired:", nrow(welch_rows), "of", n_reps, "\n")
if (nrow(welch_rows) > 0) {
  type1_welch <- mean(welch_rows$p_val < 0.05)
  cat(sprintf("Empirical Type-I error rate (p < .05) among those runs: %.3f\n", type1_welch))
  ci <- prop.test(sum(welch_rows$p_val < 0.05), nrow(welch_rows))$conf.int
  cat(sprintf("95%% CI: [%.3f, %.3f]\n", ci[1], ci[2]))
}

# ------------------------------------------------------------------
# 3. NON_EQ cell (non-normal + equal var) -- should still route to
#    Kruskal-Wallis essentially always, matching pre-change behavior.
# ------------------------------------------------------------------
non_eq_rows <- all_res[all_res$cell == "NON_EQ", ]
cat("\n=== 3. Cell NON_EQ (non-normal + equal var) -- branch stability check ===\n")
print(table(non_eq_rows$branch))
kw_rate <- mean(non_eq_rows$branch %in% c("Kruskal-Wallis", "Mann-Whitney U (via Kruskal)"))
cat(sprintf("Kruskal-Wallis rate: %.3f (pre-change behavior: 1.000, always KW when norm_p < .05)\n", kw_rate))
if (kw_rate < 1) {
  off_target <- non_eq_rows[!(non_eq_rows$branch %in% c("Kruskal-Wallis", "Mann-Whitney U (via Kruskal)")), ]
  cat("Runs that did NOT go to Kruskal-Wallis:\n")
  print(off_target[, c("branch", "norm_p", "levene_p")])
}
type1_kw_non_eq <- mean(non_eq_rows$p_val[non_eq_rows$branch %in% c("Kruskal-Wallis","Mann-Whitney U (via Kruskal)")] < 0.05)
cat(sprintf("Type-I error rate of the executed test in this cell: %.3f\n", type1_kw_non_eq))

# ------------------------------------------------------------------
# 4. Gray-zone frequency by cell
# ------------------------------------------------------------------
cat("\n=== 4. Gray-zone frequency (.01 <= levene_p <= .10) by cell ===\n")
gray_raw <- tapply(all_res$levene_p, all_res$cell, function(p) mean(p >= 0.01 & p <= 0.10))
cat("Raw rate (levene_p in range, regardless of norm_p):\n")
print(round(gray_raw, 3))

reached_branching <- all_res[all_res$norm_p < 0.05, ]
gray_reached <- tapply(reached_branching$levene_p, reached_branching$cell,
                        function(p) mean(p >= 0.01 & p <= 0.10))
cat("\nRate among runs that actually reached the 3-way branch (norm_p < .05):\n")
print(round(gray_reached, 3))
cat("(n reaching branch per cell):\n")
print(table(reached_branching$cell))

saveRDS(all_res, "results_anova_welch_2x2.rds")
