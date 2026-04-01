# ==============================================================================
# MASTER SCRIPT: GENERACIÓN DE RESULTADOS PARA LA SECCIÓN 3 (OLSengine)
# ==============================================================================
library(OLSengine)

# --- CASE 1: OLS CON HETEROCEDASTICIDAD (Tabla 4) ---
set.seed(123)
n1 <- 200
x1 <- rnorm(n1, 50, 10)
y1 <- 10 + 0.5 * x1 + rnorm(n1, 0, x1 * 0.2)
df1 <- data.frame(y = y1, x = x1)

res1 <- paper_engine(y ~ x, data = df1, model = "ols", robust = TRUE, digits = 2)

cat("\n=== CASE 1: OLS ROBUSTO (Tabla 4) ===\n")
print(res1$tables$Table2_OLS_Estimation)
cat("\nMensaje Aduana Case 1:\n", res1$messages[1], "\n")


# --- CASE 2: ANOVA CON NO-NORMALIDAD (Tabla 5) ---
set.seed(123)
group2 <- as.factor(rep(c("Control", "Trat_A", "Trat_B"), each = 50))
# Generamos datos con distribución exponencial (viola normalidad)
y2 <- c(rexp(50, 0.1), rexp(50, 0.5), rexp(50, 0.1))
df2 <- data.frame(y = y2, group = group2)

# Usamos 'auto' para que la aduana decida el test
res2 <- paper_engine(y ~ group, data = df2, model = "anova", non_parametric = "auto", digits = 2)

cat("\n=== CASE 2: ANOVA / NON-PARAMETRIC (Tabla 5) ===\n")
# Si la aduana detectó no-normalidad, esta tabla mostrará Kruskal-Wallis
print(res2$tables$Table2_ANOVA_Main_Effects)
cat("\nMensaje Aduana Case 2:\n", res2$messages[1], "\n")


# --- CASE 3: LOGIT CON SEPARACIÓN PERFECTA (Reporte Narrativo) ---
set.seed(123)
# Creamos un predictor que separa perfectamente el 0 del 1
x3 <- c(rnorm(50, 0, 1), rnorm(50, 10, 1))
y3 <- c(rep(0, 50), rep(1, 50))
df3 <- data.frame(y = y3, x = x3)

res3 <- paper_engine(y ~ x, data = df3, model = "logit", digits = 2)

cat("\n=== CASE 3: LOGIT / PERFECT SEPARATION ===\n")
# En este caso, lo más importante es el mensaje de advertencia
cat("Mensaje Aduana Case 3:\n", res3$messages[1], "\n")

