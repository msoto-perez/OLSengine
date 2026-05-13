# ========================================================
# VALIDATION TEST 6: DIFFERENCE-IN-DIFFERENCES (DiD)
# ========================================================

# Clear environment for pure reproducibility
rm(list = ls())

# install directly from GitHub and load
devtools::install_github("msoto-perez/OLSengine")
library(OLSengine)

# 1. Simulate DiD data (Policy intervention study)
set.seed(789)
n_per_group <- 100  # 100 units per group

# Group assignment (Treated = 1, Control = 0)
treated <- rep(c(0, 1), each = n_per_group * 2)  # 2 time periods per unit

# Time periods (Pre = 0, Post = 1)
post <- rep(c(0, 1), times = n_per_group * 2)

# Create entity IDs (for repeated measures)
entity_id <- rep(1:(n_per_group * 2), each = 2)

# Simulate outcome with parallel trends pre-treatment
# True treatment effect = 5 units
baseline <- rnorm(n_per_group * 2, mean = 50, sd = 10)
baseline_expanded <- rep(baseline, each = 2)

# Time trend (same for both groups - parallel trends)
time_effect <- post * 3

# Treatment effect (only for treated group in post period)
treatment_effect <- treated * post * 5

# Outcome
y <- baseline_expanded + time_effect + treatment_effect + rnorm(n_per_group * 4, sd = 5)

# Covariates (optional)
x1 <- rnorm(n_per_group * 4, mean = 10, sd = 2)

test_data_did <- data.frame(
  y = y,
  treated = factor(treated, levels = c(0, 1), labels = c("Control", "Treated")),
  time = factor(post, levels = c(0, 1), labels = c("Pre", "Post")),
  x1 = x1,
  entity_id = entity_id
)

# 2. Estimate DiD without covariates
cat("\n======================================================\n")
cat("--- DIFFERENCE-IN-DIFFERENCES (Basic) ---\n")
cat("======================================================\n")

did_basic <- paper_engine(
  y ~ 1,  # No covariates, just the DiD structure
  data = test_data_did,
  model = "did",
  treatment_var = "treated",
  time_var = "time",
  treatment_level = "Treated",
  post_level = "Post"
)

cat("\n>> TABLE: DiD ESTIMATION\n")
print(did_basic$tables$Table2_DiD_Estimation)

cat("\n>> GROUP MEANS (for visualization):\n")
print(did_basic$tables$Group_Means)

cat("\n>> CUSTOMS MESSAGES\n")
print(did_basic$messages)

cat("\n>> Expected: Treated:Post interaction should be around 5.0 (true effect)\n")

# 3. Estimate DiD with covariates
cat("\n\n======================================================\n")
cat("--- DIFFERENCE-IN-DIFFERENCES (With Covariates) ---\n")
cat("======================================================\n")

did_covariates <- paper_engine(
  y ~ x1,  # Including covariate
  data = test_data_did,
  model = "did",
  treatment_var = "treated",
  time_var = "time",
  treatment_level = "Treated",
  post_level = "Post"
)

cat("\n>> TABLE: DiD WITH CONTROLS\n")
print(did_covariates$tables$Table2_DiD_Estimation)

cat("\n>> CUSTOMS MESSAGES\n")
print(did_covariates$messages)

# 4. Test with violation of parallel trends
cat("\n\n======================================================\n")
cat("--- TEST: VIOLATION OF PARALLEL TRENDS ---\n")
cat("======================================================\n")

# Create data where treated group already had higher growth pre-treatment
set.seed(999)
baseline_control <- rnorm(n_per_group, mean = 50, sd = 10)
baseline_treated <- rnorm(n_per_group, mean = 55, sd = 10)  # Higher baseline

baseline_all <- c(rep(baseline_control, each = 2), rep(baseline_treated, each = 2))

# Different time trends for treated vs control (violates parallel trends)
time_effect_control <- post * 2
time_effect_treated <- post * 6  # Steeper trend

time_effect_all <- c(
  rep(time_effect_control[1:(n_per_group * 2)], 1),
  rep(time_effect_treated[1:(n_per_group * 2)], 1)
)

y_violated <- baseline_all + time_effect_all + treatment_effect + rnorm(n_per_group * 4, sd = 5)

test_data_violated <- data.frame(
  y = y_violated,
  treated = factor(treated, levels = c(0, 1), labels = c("Control", "Treated")),
  time = factor(post, levels = c(0, 1), labels = c("Pre", "Post")),
  entity_id = entity_id
)

did_violated <- paper_engine(
  y ~ 1,
  data = test_data_violated,
  model = "did",
  treatment_var = "treated",
  time_var = "time",
  treatment_level = "Treated",
  post_level = "Post"
)

cat("\n>> CUSTOMS MESSAGES (Should warn about pre-trends):\n")
print(did_violated$messages)

cat("\n\n======================================================\n")
cat(">> Test Complete: DiD engine functional\n")
cat("======================================================\n")

# 5. Test plot
cat("\n>> GENERANDO GRÁFICO APA EN EL PANEL DE PLOTS...\n")
plot_engine(did_basic)