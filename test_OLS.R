# ========================================================
# VALIDATION TEST 1: OLS MODEL
# ========================================================

# Clear environment
rm(list = ls())

# CARGAR EL PAQUETE COMPLETO (Sustituye a source)
devtools::load_all()

# 1. Simulate data
set.seed(123)
n <- 200
x1 <- rnorm(n, mean = 50, sd = 10)
x2 <- x1 * 0.4 + rnorm(n, mean = 20, sd = 5) # Generates correlation to test VIF
y <- 15 + 2.5 * x1 - 1.2 * x2 + rnorm(n, mean = 0, sd = 8)

test_data <- data.frame(y, x1, x2)

# 2. Run our OLSengine (Silent Layer 1)
engine_result <- ols_engine(y ~ x1 + x2, data = test_data)
base_model <- lm(y ~ x1 + x2, data = test_data)

# 3. Mathematical precision comparison
cat("\n--- COEFFICIENTS COMPARISON ---\n")
print(round(engine_result$coefficients, 4))
print(round(coef(base_model), 4)) # Should be identical

cat("\n--- EFFECT SIZE (f2) ---\n")
print(round(engine_result$f2, 4))

cat("\n--- MANUAL VIF ---\n")
print(round(engine_result$vif, 4))

cat("\n--- RAW DIAGNOSTICS ---\n")
print(engine_result$diagnostics)

# 4. Test the Main Function (Layer 2: Customs and Formatting)
cat("\n======================================================\n")
cat("--- PAPER READY OUTPUT (OLS) ---\n")
cat("======================================================\n")
ols_model <- paper_engine(y ~ x1 + x2, data = test_data, model = "ols")

cat("\n>> TABLE 2: ESTIMATION\n")
print(ols_model$tables$Table2_OLS_Estimation)

cat("\n>> CUSTOMS MESSAGES\n")
print(ols_model$messages)

cat("\n>> GENERANDO GRÁFICO APA EN EL PANEL DE PLOTS...\n")
plot_engine(ols_model)

