# ========================================================
# VALIDATION TEST 5: INSTRUMENTAL VARIABLES (2SLS)
# ========================================================

# Clear environment for pure reproducibility
rm(list = ls())

# install directly from GitHub and load
devtools::install_github("msoto-perez/OLSengine")
library(OLSengine)

# 1. Simulate IV data with endogeneity
set.seed(456)
n <- 300

# Instrument (exogenous)
z <- rnorm(n, mean = 10, sd = 3)

# Unobserved confounder (creates endogeneity)
u <- rnorm(n, mean = 0, sd = 2)

# Endogenous predictor (correlated with error through u)
x <- 2 + 0.8 * z + u + rnorm(n, mean = 0, sd = 1)

# Outcome (affected by x AND u, creating endogeneity bias in naive OLS)
y <- 5 + 1.5 * x + u + rnorm(n, mean = 0, sd = 3)

test_data_iv <- data.frame(y, x, z)

# 2. Compare Naive OLS vs IV (2SLS)
cat("\n======================================================\n")
cat("--- NAIVE OLS (Biased due to endogeneity) ---\n")
cat("======================================================\n")
naive_ols <- paper_engine(y ~ x, data = test_data_iv, model = "ols")

cat("\n>> OLS Coefficient (BIASED):\n")
print(naive_ols$tables$Table2_OLS_Estimation)

cat("\n>> Expected: Coefficient on x should be biased UPWARD (> 1.5 true effect)\n")

cat("\n\n======================================================\n")
cat("--- INSTRUMENTAL VARIABLES (2SLS - Consistent) ---\n")
cat("======================================================\n")
iv_model <- paper_engine(
  y ~ x,
  data = test_data_iv,
  model = "iv",
  instruments = ~ z
)

cat("\n>> TABLE: 2SLS ESTIMATION\n")
print(iv_model$tables$Table2_IV_2SLS)

cat("\n>> CUSTOMS MESSAGES\n")
print(iv_model$messages)

cat("\n>> Expected: IV coefficient on x should be closer to 1.5 (true causal effect)\n")

# 3. Test with weak instrument (should trigger warning)
cat("\n\n======================================================\n")
cat("--- TEST: WEAK INSTRUMENT (Should Warn) ---\n")
cat("======================================================\n")

# Create weak instrument (low correlation with x)
z_weak <- rnorm(n, mean = 0, sd = 10)
x_weak <- 2 + 0.05 * z_weak + u + rnorm(n, mean = 0, sd = 3)
y_weak <- 5 + 1.5 * x_weak + u + rnorm(n, mean = 0, sd = 3)

test_data_weak <- data.frame(y = y_weak, x = x_weak, z = z_weak)

iv_weak <- paper_engine(
  y ~ x,
  data = test_data_weak,
  model = "iv",
  instruments = ~ z
)

cat("\n>> CUSTOMS MESSAGES (Should warn about weak instrument):\n")
print(iv_weak$messages)

# 4. Test with overidentification (2 instruments for 1 endogenous variable)
cat("\n\n======================================================\n")
cat("--- TEST: OVERIDENTIFICATION (Sargan Test) ---\n")
cat("======================================================\n")

# Create second valid instrument
z2 <- rnorm(n, mean = 5, sd = 2)
x_over <- 2 + 0.7 * z + 0.5 * z2 + u + rnorm(n, mean = 0, sd = 1)
y_over <- 5 + 1.5 * x_over + u + rnorm(n, mean = 0, sd = 3)

test_data_over <- data.frame(y = y_over, x = x_over, z1 = z, z2 = z2)

iv_over <- paper_engine(
  y ~ x,
  data = test_data_over,
  model = "iv",
  instruments = ~ z1 + z2
)

cat("\n>> TABLE: 2SLS WITH 2 INSTRUMENTS\n")
print(iv_over$tables$Table2_IV_2SLS)

cat("\n>> CUSTOMS MESSAGES (Should report Sargan test):\n")
print(iv_over$messages)

cat("\n\n======================================================\n")
cat(">> Test Complete: IV engine functional\n")
cat("======================================================\n")

# 5. Test plot
cat("\n>> GENERANDO GRÁFICO APA EN EL PANEL DE PLOTS...\n")
plot_engine(iv_model)

