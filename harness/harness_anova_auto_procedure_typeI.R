# ==============================================================================
# Evaluation criterion, fixed before running this simulation: the empirical Type I error rate is considered acceptable if it lies within [0.5*alpha, 1.5*alpha] = [2.5%, 7.5%] (Bradley, 1978).
#
# Bradley, J. V. (1978). Robustness? British Journal of Mathematical and
# Statistical Psychology, 31(2), 144-152.
#
# HARNESS - Type I error of the full anova_engine(non_parametric = "auto")
# procedure, with the routing introduced in OLSengine 1.2.1:
#   1. Normality test on the pooled one-way ANOVA residuals
#      (Shapiro-Wilk; all cells here have N = 150 <= 5000).
#   2. Normality p >= .05  -> classical one-way ANOVA (F test).
#      Normality p <  .05  -> Levene's test (Brown-Forsythe, median-centered):
#        Levene p <= .10   -> Welch's ANOVA (oneway.test, var.equal = FALSE)
#        Levene p >  .10   -> Kruskal-Wallis
#   3. The p-value of the FINAL test is compared with alpha = .05.
#
# Design: 2x2, normality {normal, non-normal (centered exponential)} x
# variance {equal (sd 5/5/5), unequal (sd 1/3/5, max/min = 5:1)}; k = 3 groups,
# n_per_group = 50. Every dataset is generated under a TRUE NULL of equal
# population means. The data-generating process is the same as in
# harness_anova_welch_2x2.R.
#
# Reported, per cell and overall (all cells pooled; cells have equal n_reps):
#   - frequency of each branch taken;
#   - rejection rate of the final test, with an exact (Clopper-Pearson) 95%
#     Monte Carlo confidence interval, and whether the point estimate lies
#     inside the criterion above.
# Branch-conditional rejection rates are printed as secondary, descriptive
# output only; the criterion applies to the unconditional rate of the full
# procedure.
#
# Optional sensitivity analysis (run_within_group_sensitivity = TRUE): on the
# same datasets, normality is assessed within groups (Shapiro-Wilk per group,
# Holm-adjusted; normality is rejected if any adjusted p < .05) instead of on
# pooled residuals. Levene's test and the final tests are computed exactly as
# in anova_engine().
#
# Usage, from the harness/ directory:
#   Rscript harness_anova_auto_procedure_typeI.R            # n_reps = 2000
#   Rscript harness_anova_auto_procedure_typeI.R 5000       # custom n_reps
# Output: results_anova_auto_procedure_typeI.rds (per-replicate results).
# ==============================================================================

suppressMessages(source("../R/OLS_engine.R"))

# The routing evaluated here was introduced in 1.2.1; refuse to run against an
# older engine.
engine_version <- package_version(read.dcf("../DESCRIPTION", fields = "Version")[1, 1])
if (engine_version < "1.2.1") {
  stop("This harness evaluates the OLSengine >= 1.2.1 routing; ../DESCRIPTION reports version ",
       engine_version, ".")
}

# ------------------------------------------------------------------------------
# Parameters
# ------------------------------------------------------------------------------
cli_args <- commandArgs(trailingOnly = TRUE)
n_reps <- if (length(cli_args) >= 1) as.integer(cli_args[1]) else 2000L
if (is.na(n_reps) || n_reps < 1 || n_reps >= 100000) {
  stop("n_reps must be an integer between 1 and 99999.")
}

alpha <- 0.05
criterion <- c(lower = 0.5 * alpha, upper = 1.5 * alpha)  # Bradley (1978)
levene_cutoff <- 0.10
n_per_group <- 50
mu <- 50
run_within_group_sensitivity <- TRUE

cells <- data.frame(
  cell        = c("NORMAL_EQ", "NORMAL_UNEQ", "NONNORMAL_EQ", "NONNORMAL_UNEQ"),
  non_normal  = c(FALSE, FALSE, TRUE, TRUE),
  unequal_var = c(FALSE, TRUE, FALSE, TRUE),
  stringsAsFactors = FALSE
)

# Seeds: cell c, replicate i -> c * 100000 + i (no overlap across cells).
replicate_seed <- function(cell_index, rep_index) cell_index * 100000 + rep_index

# ------------------------------------------------------------------------------
# Data-generating process (true null: equal population means in all groups)
# ------------------------------------------------------------------------------
make_data <- function(non_normal, unequal_var, seed) {
  set.seed(seed)
  sds <- if (unequal_var) c(1, 3, 5) else c(5, 5, 5)
  if (!non_normal) {
    y <- unlist(lapply(sds, function(s) rnorm(n_per_group, mean = 0, sd = s)))
  } else {
    # Exponential(rate = 1) scaled by s and recentred: mean 0, sd s, skewed.
    y <- unlist(lapply(sds, function(s) rexp(n_per_group, rate = 1) * s - s))
  }
  data.frame(y = mu + y, group = factor(rep(c("A", "B", "C"), each = n_per_group)))
}

# ------------------------------------------------------------------------------
# Sensitivity procedure: within-group normality (Shapiro-Wilk + Holm), then the
# same Levene routing and final tests as anova_engine(non_parametric = "auto").
# ------------------------------------------------------------------------------
auto_within_groups <- function(df) {
  shapiro_p <- tapply(df$y, df$group, function(v) shapiro.test(v)$p.value)
  normality_rejected <- min(p.adjust(shapiro_p, method = "holm")) < 0.05

  group_medians <- tapply(df$y, df$group, median)
  abs_dev <- abs(df$y - group_medians[df$group])
  levene_p <- summary(aov(abs_dev ~ df$group))[[1]]$`Pr(>F)`[1]

  if (!normality_rejected) {
    list(branch = "One-Way ANOVA",
         p = summary(aov(y ~ group, data = df))[[1]]$`Pr(>F)`[1])
  } else if (levene_p <= levene_cutoff) {
    list(branch = "Welch's ANOVA",
         p = oneway.test(y ~ group, data = df, var.equal = FALSE)$p.value)
  } else {
    list(branch = "Kruskal-Wallis",
         p = kruskal.test(y ~ group, data = df)$p.value)
  }
}

# ------------------------------------------------------------------------------
# Simulation
# ------------------------------------------------------------------------------
cell_results <- vector("list", nrow(cells))
for (cell_index in seq_len(nrow(cells))) {
  branch <- character(n_reps)
  p_final <- numeric(n_reps)
  branch_wg <- rep(NA_character_, n_reps)
  p_wg <- rep(NA_real_, n_reps)

  for (i in seq_len(n_reps)) {
    df <- make_data(cells$non_normal[cell_index], cells$unequal_var[cell_index],
                    seed = replicate_seed(cell_index, i))

    eng <- anova_engine(y ~ group, data = df, non_parametric = "auto")
    branch[i] <- eng$effects_table$Test
    p_final[i] <- eng$effects_table$p_value

    if (run_within_group_sensitivity) {
      wg <- auto_within_groups(df)
      branch_wg[i] <- wg$branch
      p_wg[i] <- wg$p
    }
  }

  cell_results[[cell_index]] <- data.frame(
    cell = cells$cell[cell_index],
    rep = seq_len(n_reps),
    seed = replicate_seed(cell_index, seq_len(n_reps)),
    branch = branch,
    p_final = p_final,
    reject = p_final < alpha,
    branch_wg = branch_wg,
    p_wg = p_wg,
    reject_wg = p_wg < alpha,
    stringsAsFactors = FALSE
  )
}
res <- do.call(rbind, cell_results)
res$cell <- factor(res$cell, levels = cells$cell)

# ------------------------------------------------------------------------------
# Summaries
# ------------------------------------------------------------------------------
rejection_summary <- function(reject) {
  k <- sum(reject)
  n <- length(reject)
  ci <- binom.test(k, n)$conf.int
  rate <- k / n
  data.frame(n = n, rejections = k, rate = rate,
             ci95_low = ci[1], ci95_high = ci[2],
             within_criterion = rate >= criterion[["lower"]] && rate <= criterion[["upper"]])
}

summarise_by_cell <- function(reject, cell) {
  per_cell <- do.call(rbind, lapply(split(reject, cell), rejection_summary))
  overall <- rejection_summary(reject)
  out <- rbind(per_cell, overall)
  out <- cbind(cell = c(levels(cell), "OVERALL"), out)
  rownames(out) <- NULL
  out
}

summarise_by_branch <- function(reject, cell, branch) {
  key <- interaction(cell, branch, drop = TRUE, sep = " | ")
  out <- do.call(rbind, lapply(split(reject, key), rejection_summary))
  out$within_criterion <- NULL
  out <- cbind(cell_branch = rownames(out), out)
  rownames(out) <- NULL
  out
}

cat(sprintf("n_reps per cell = %d | alpha = %.2f | criterion = [%.3f, %.3f] (Bradley, 1978)\n\n",
            n_reps, alpha, criterion[["lower"]], criterion[["upper"]]))

cat("=== PRIMARY: pooled-residual normality (anova_engine, non_parametric = \"auto\") ===\n\n")
cat("Branch frequency by cell:\n")
print(table(res$cell, res$branch))
cat("\nBranch proportions by cell:\n")
print(round(prop.table(table(res$cell, res$branch), margin = 1), 4))

cat("\nType I error of the FINAL test, by cell and overall (exact 95% CI):\n")
print(summarise_by_cell(res$reject, res$cell), digits = 4, row.names = FALSE)

cat("\nSecondary (descriptive): rejection rate conditional on branch taken:\n")
print(summarise_by_branch(res$reject, res$cell, res$branch), digits = 4, row.names = FALSE)

if (run_within_group_sensitivity) {
  cat("\n=== SENSITIVITY: within-group normality (Shapiro-Wilk per group, Holm) ===\n\n")
  cat("Branch frequency by cell:\n")
  print(table(res$cell, res$branch_wg))

  cat("\nType I error of the FINAL test, by cell and overall (exact 95% CI):\n")
  print(summarise_by_cell(res$reject_wg, res$cell), digits = 4, row.names = FALSE)

  cat("\nSecondary (descriptive): rejection rate conditional on branch taken:\n")
  print(summarise_by_branch(res$reject_wg, res$cell, res$branch_wg), digits = 4, row.names = FALSE)

  cat("\nAgreement between procedures (rows: pooled residuals, cols: within groups):\n")
  print(table(pooled = res$branch, within_groups = res$branch_wg))
}

saveRDS(res, "results_anova_auto_procedure_typeI.rds")
