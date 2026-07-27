# ==============================================================================
# HARNESS B1 - PERFORMANCE BENCHMARK
# Wall-clock time: OLSengine's 6 engines vs. their reference implementations,
# across increasing dataset sizes. Median of `reps` runs per size.
# ==============================================================================
suppressMessages(source("../R/OLS_engine.R"))
suppressMessages(library(plm))
suppressMessages(library(AER))

time_median <- function(expr_fun, reps = 7) {
  ts <- replicate(reps, {
    t0 <- Sys.time()
    invisible(expr_fun())
    as.numeric(Sys.time() - t0, units = "secs")
  })
  median(ts)
}

results <- data.frame(engine=character(), n=integer(), reference_s=double(),
                       olsengine_s=double(), ratio=double())

# ---------------- OLS ----------------
for (n in c(500, 5000, 50000, 200000)) {
  set.seed(1); x1 <- rnorm(n); x2 <- rnorm(n); y <- 1 + 2*x1 - x2 + rnorm(n)
  df <- data.frame(y, x1, x2)
  t_ref <- time_median(function() lm(y ~ x1 + x2, data = df))
  t_eng <- time_median(function() ols_engine(y ~ x1 + x2, data = df))
  results <- rbind(results, data.frame(engine="OLS", n=n, reference_s=t_ref,
                                        olsengine_s=t_eng, ratio=t_eng/t_ref))
}

# ---------------- ANOVA ----------------
# NOTE: anova_engine() calls shapiro.test() unconditionally on pooled residuals.
# Base R's shapiro.test() hard-errors for n > 5000 ("sample size must be between
# 3 and 5000"). Unlike ols_engine() (which correctly switches to Kolmogorov-Smirnov
# for n > 5000), anova_engine() has NO such guard - it will crash outright on any
# dataset above 5000 rows. This is a genuine bug, documented here rather than
# worked around silently. Benchmark capped at n=5000 for this reason.
for (n in c(500, 2000, 4999)) {
  set.seed(1)
  g <- factor(rep(c("A","B","C"), length.out = n))
  y <- rnorm(n, ifelse(g=="A", 10, ifelse(g=="B", 12, 11)), 5)
  df <- data.frame(y, g)
  t_ref <- time_median(function() aov(y ~ g, data = df))
  t_eng <- time_median(function() anova_engine(y ~ g, data = df))
  results <- rbind(results, data.frame(engine="ANOVA", n=n, reference_s=t_ref,
                                        olsengine_s=t_eng, ratio=t_eng/t_ref))
}

# ---------------- Logit ----------------
for (n in c(500, 5000, 50000, 200000)) {
  set.seed(1); x <- rnorm(n); y <- rbinom(n, 1, plogis(-0.5 + 0.8*x))
  df <- data.frame(y, x)
  t_ref <- time_median(function() glm(y ~ x, data = df, family = binomial()))
  t_eng <- time_median(function() logit_engine(y ~ x, data = df))
  results <- rbind(results, data.frame(engine="Logit", n=n, reference_s=t_ref,
                                        olsengine_s=t_eng, ratio=t_eng/t_ref))
}

# ---------------- Panel ----------------
for (cfg in list(c(50,10), c(200,10), c(1000,10), c(2000,25))) {
  n_ent <- cfg[1]; n_time <- cfg[2]; n <- n_ent * n_time
  set.seed(1)
  ids <- rep(1:n_ent, each = n_time); times <- rep(1:n_time, times = n_ent)
  ent_eff <- rnorm(n_ent, 0, 5)
  x1 <- rnorm(n, 10, 3)
  y <- 5 + 1.2*x1 + ent_eff[ids] + rnorm(n, 0, 2)
  df <- data.frame(id=ids, time=times, y, x1)
  pdata <- pdata.frame(df, index = c("id","time"))
  t_ref <- time_median(function() plm(y ~ x1, data = pdata, model = "within"), reps = 5)
  t_eng <- time_median(function() panel_engine(y ~ x1, data = df, entity_id="id", time_id="time", method="fe"), reps = 5)
  results <- rbind(results, data.frame(engine="Panel", n=n, reference_s=t_ref,
                                        olsengine_s=t_eng, ratio=t_eng/t_ref))
}

# ---------------- IV ----------------
# NOTE: iv_engine() forms an explicit n x n projection matrix
# (PZ <- Z_int %*% solve(crossprod(Z_int)) %*% t(Z_int), line 688 of OLS_engine.R)
# instead of computing X_tilde via associativity without materializing PZ.
# This is O(n^2) memory, not O(n*k^2). At n=100,000 this attempts to allocate
# ~74.5 GB and crashes outright (confirmed empirically - see conversation notes).
# Benchmark capped well below that threshold given this sandbox's 3.9GB RAM;
# on a typical researcher's machine (16-32GB) the crash point would be
# roughly n ~ 40,000-60,000, still a real practical ceiling for panel/survey data.
for (n in c(200, 2000, 5000, 10000)) {
  set.seed(1)
  z <- rnorm(n, 10, 3); u <- rnorm(n, 0, 2)
  x <- 2 + 0.8*z + u + rnorm(n); y <- 5 + 1.5*x + u + rnorm(n, sd=3)
  df <- data.frame(y, x, z)
  t_ref <- time_median(function() ivreg(y ~ x | z, data = df), reps = 5)
  t_eng <- time_median(function() iv_engine(y ~ x, data = df, instruments = ~z), reps = 5)
  results <- rbind(results, data.frame(engine="IV", n=n, reference_s=t_ref,
                                        olsengine_s=t_eng, ratio=t_eng/t_ref))
}

# ---------------- DiD ----------------
for (n in c(400, 4000, 40000, 200000)) {
  set.seed(1)
  half <- n %/% 2
  treated_num <- rep(c(0,1), each = half); post_num <- rep(c(0,1), times = half)
  df <- data.frame(treated_num, post_num)
  df$interaction <- df$treated_num * df$post_num
  df$y <- 50 + 2*df$treated_num + 3*df$post_num + 5*df$interaction + rnorm(n, sd=3)
  df$treated_f <- factor(df$treated_num, levels=c(0,1), labels=c("Control","Treated"))
  df$post_f    <- factor(df$post_num, levels=c(0,1), labels=c("Pre","Post"))
  t_ref <- time_median(function() lm(y ~ treated_num + post_num + interaction, data = df))
  t_eng <- time_median(function() did_engine(y ~ 1, data = df, treatment_var="treated_f",
                                              time_var="post_f", treatment_level="Treated", post_level="Post"))
  results <- rbind(results, data.frame(engine="DiD", n=n, reference_s=t_ref,
                                        olsengine_s=t_eng, ratio=t_eng/t_ref))
}

cat("=== B1 PERFORMANCE BENCHMARK: OLSengine vs. reference implementations ===\n")
cat("(ratio > 1 means OLSengine is SLOWER; ratio < 1 means FASTER)\n\n")
print(results, row.names = FALSE, digits = 3)

saveRDS(results, "results_b1_benchmark.rds")
