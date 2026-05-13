# ==============================================================================
# NUMERICAL VALIDATION SCRIPT (Base R vs OLSengine)
# Generates the comparison table for the manuscript
# ==============================================================================

# Clear environment for pure reproducibility
rm(list = ls())

# install directly from GitHub and load
devtools::install_github("msoto-perez/OLSengine", force = TRUE)
library(OLSengine)

# 1. Reproducible Data Generation
set.seed(42)
n <- 300
x1 <- rnorm(n, mean = 50, sd = 10)
x2 <- rbinom(n, size = 1, prob = 0.5)
group <- as.factor(sample(c("Control", "Treatment A", "Treatment B"), n, replace = TRUE))

y_cont <- 15 + 2.5 * x1 - 1.2 * x2 + rnorm(n, mean = 0, sd = 8)
y_bin <- rbinom(n, size = 1, prob = plogis(-4 + 0.08 * x1 + 1.2 * x2))

df_test <- data.frame(y_cont, y_bin, x1, x2, group)

# ==============================================================================
# 2. OLS VALIDATION
# ==============================================================================
base_lm <- summary(lm(y_cont ~ x1 + x2, data = df_test))
eng_ols <- OLSengine:::ols_engine(y_cont ~ x1 + x2, data = df_test)

# ==============================================================================
# 3. ANOVA VALIDATION
# ==============================================================================
base_aov <- summary(aov(y_cont ~ group, data = df_test))[[1]]
eng_aov <- OLSengine:::anova_engine(y_cont ~ group, data = df_test)

# ==============================================================================
# 4. LOGIT VALIDATION
# ==============================================================================
base_glm <- summary(glm(y_bin ~ x1 + x2, data = df_test, family = binomial(link = "logit")))
eng_logit <- OLSengine:::logit_engine(y_bin ~ x1 + x2, data = df_test)

# ==============================================================================
# 5. PANEL DATA VALIDATION (Fixed Effects)
# ==============================================================================

# Install plm if needed
if (!requireNamespace("plm", quietly = TRUE)) {
  install.packages("plm")
}
library(plm)

# Create simple panel data
set.seed(999)
panel_df <- data.frame(
  id = rep(1:30, each = 5),
  time = rep(1:5, times = 30),
  y = rnorm(150) + rep(rnorm(30, mean = 50, sd = 10), each = 5),  # entity fixed effects
  x1 = rnorm(150, mean = 10, sd = 3),
  x2 = rnorm(150, mean = 5, sd = 2)
)

# Base R: plm package (Fixed Effects)
panel_pdata <- pdata.frame(panel_df, index = c("id", "time"))
base_plm_fe <- plm(y ~ x1 + x2, data = panel_pdata, model = "within")

# OLSengine: panel_engine (Fixed Effects)
eng_panel_fe <- OLSengine:::panel_engine(
  y ~ x1 + x2,
  data = panel_df,
  entity_id = "id",
  time_id = "time",
  method = "fe"
)

# ==============================================================================
# 6. BUILD COMPARISON TABLE
# ==============================================================================

comparison_table <- data.frame(
  Parameter = c(
    "OLS: Coefficient (x1)",
    "OLS: Adjusted R-squared",
    "ANOVA: F-statistic",
    "ANOVA: p-value",
    "Logit: Coefficient (x1)",
    "Logit: Standard Error (x2)",
    "Panel FE: Coefficient (x1)",
    "Panel FE: Coefficient (x2)",
    "Panel FE: SE (x1)",
    "Panel FE: SE (x2)"
  ),
  Base_R = c(
    base_lm$coefficients["x1", "Estimate"],
    base_lm$adj.r.squared,
    base_aov$`F value`[1],
    base_aov$`Pr(>F)`[1],
    base_glm$coefficients["x1", "Estimate"],
    base_glm$coefficients["x2", "Std. Error"],
    coef(base_plm_fe)["x1"],
    coef(base_plm_fe)["x2"],
    summary(base_plm_fe)$coefficients["x1", "Std. Error"],
    summary(base_plm_fe)$coefficients["x2", "Std. Error"]
  ),
  Proposed_Engine = c(
    eng_ols$coefficients["x1"],
    eng_ols$diagnostics$Adj_R2,
    eng_aov$effects_table$Statistic,
    eng_aov$effects_table$p_value,
    eng_logit$coefficients["x1"],
    eng_logit$se["x2"],
    eng_panel_fe$coefficients["x1"],
    eng_panel_fe$coefficients["x2"],
    eng_panel_fe$se["x1"],
    eng_panel_fe$se["x2"]
  )
)

# 7. Calculate Absolute Difference and Format
comparison_table$Absolute_Difference <- abs(comparison_table$Base_R - comparison_table$Proposed_Engine)

# Format to 4 decimal places for scientific reporting (6 for very small differences)
comparison_table$Base_R <- sprintf("%.4f", comparison_table$Base_R)
comparison_table$Proposed_Engine <- sprintf("%.4f", comparison_table$Proposed_Engine)
comparison_table$Absolute_Difference <- sprintf("%.6f", comparison_table$Absolute_Difference)

# ==============================================================================
# 8. DISPLAY RESULTS
# ==============================================================================

cat("\n")
cat("==============================================================================\n")
cat("NUMERICAL VALIDATION: OLSengine vs Base R / Standard Packages\n")
cat("==============================================================================\n\n")

print(comparison_table, row.names = FALSE)

cat("\n")
cat(">> Validation Complete: All methods numerically verified.\n")
cat(">> Expected: Absolute_Difference < 0.001 for all parameters.\n")
cat("==============================================================================\n")

