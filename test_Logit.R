# ========================================================
# VALIDATION TEST 3: LOGIT MODEL
# ========================================================

# Clear environment
rm(list = ls())

# CARGAR EL PAQUETE COMPLETO (Sustituye a source)
devtools::load_all()

# 1. Simulate binary data (0 and 1)
set.seed(789)
n <- 300
x1 <- rnorm(n, mean = 50, sd = 10)  # Continuous predictor
x2 <- rbinom(n, size = 1, prob = 0.5) # Binary predictor (e.g., gender)

# Logit link function simulation
z <- -4 + 0.08 * x1 + 1.2 * x2
pr <- 1 / (1 + exp(-z))
y_binary <- rbinom(n, size = 1, prob = pr)

test_data <- data.frame(y = y_binary, x1, x2)

# 2. Test the Main Function (Routing to Logit)
cat("\n======================================================\n")
cat("--- PAPER READY OUTPUT (LOGIT) ---\n")
cat("======================================================\n")
logit_model <- paper_engine(y ~ x1 + x2, data = test_data, model = "logit")

cat("\n>> TABLE 2: LOGIT ESTIMATION (Odds Ratios & p-values)\n")
print(logit_model$tables$Table2_Logit_Estimation)

cat("\n>> CUSTOMS MESSAGES & FIT\n")
for (msg in logit_model$messages) {
  cat(msg, "\n")
}

cat("\n>> GENERANDO GRÁFICO APA EN EL PANEL DE PLOTS...\n")
plot_engine(logit_model, y_label = "Probabilidad de Compra")

