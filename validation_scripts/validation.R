# ==============================================================================
# NUMERICAL VALIDATION SCRIPT (Base R vs OLSengine)
# Generates the comparison table for the manuscript
# ==============================================================================

# Clear environment for pure reproducibility
rm(list = ls())

# install directly from GitHub and load
devtools::install_github("msoto-perez/OLSengine")
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

# 2. Estimate Models (Base R vs OLSengine)
# --- OLS ---
base_lm <- summary(lm(y_cont ~ x1 + x2, data = df_test))
eng_ols <- OLSengine:::ols_engine(y_cont ~ x1 + x2, data = df_test)

# --- ANOVA ---
base_aov <- summary(aov(y_cont ~ group, data = df_test))[[1]]
eng_aov <- OLSengine:::anova_engine(y_cont ~ group, data = df_test)

# --- LOGIT ---
base_glm <- summary(glm(y_bin ~ x1 + x2, data = df_test, family = binomial(link = "logit")))
eng_logit <- OLSengine:::logit_engine(y_bin ~ x1 + x2, data = df_test)

# 3. Build Comparison Table
comparison_table <- data.frame(
  Parameter = c(
    "OLS: Coefficient (x1)",
    "OLS: Adjusted R-squared",
    "ANOVA: F-statistic",
    "ANOVA: p-value",
    "Logit: Coefficient (x1)",
    "Logit: Standard Error (x2)"
  ),
  Base_R = c(
    base_lm$coefficients["x1", "Estimate"],
    base_lm$adj.r.squared,
    base_aov$`F value`[1],
    base_aov$`Pr(>F)`[1],
    base_glm$coefficients["x1", "Estimate"],
    base_glm$coefficients["x2", "Std. Error"]
  ),
  Proposed_Engine = c(
    eng_ols$coefficients["x1"],
    eng_ols$diagnostics$Adj_R2,
    eng_aov$effects_table$Statistic,
    eng_aov$effects_table$p_value,
    eng_logit$coefficients["x1"],
    eng_logit$se["x2"]
  )
)

# 4. Calculate Absolute Difference and Format
comparison_table$Absolute_Difference <- abs(comparison_table$Base_R - comparison_table$Proposed_Engine)

# Format to 4 decimal places for scientific reporting
comparison_table$Base_R <- sprintf("%.4f", comparison_table$Base_R)
comparison_table$Proposed_Engine <- sprintf("%.4f", comparison_table$Proposed_Engine)
comparison_table$Absolute_Difference <- sprintf("%.4f", comparison_table$Absolute_Difference)

print(comparison_table)

# --- PANEL DATA ---
# Install plm for comparison
if (!requireNamespace("plm", quietly = TRUE)) {
  install.packages("plm")
}
library(plm)

# Create panel data
panel_df <- data.frame(
  id = rep(1:50, each = 3),
  time = rep(1:3, times = 50),
  y = rnorm(150) + rep(rnorm(50), each = 3),  # entity fixed effects
  x1 = rnorm(150),
  x2 = rnorm(150)
)

panel_pdata <- pdata.frame(panel_df, index = c("id", "time"))
base_plm <- plm(y ~ x1 + x2, data = panel_pdata, model = "within")
eng_panel <- OLSengine:::panel_engine(y ~ x1 + x2, data = panel_df,
                                      entity_id = "id", time_id = "time",
                                      method = "fe")

# Add to comparison table
panel_rows <- data.frame(
  Parameter = c(
    "Panel FE: Coefficient (x1)",
    "Panel FE: SE (x1)"
  ),
  Base_R = c(
    coef(base_plm)["x1"],
    summary(base_plm)$coefficients["x1", "Std. Error"]
  ),
  Proposed_Engine = c(
    eng_panel$coefficients["x1"],
    eng_panel$se["x1"]
  )
)

panel_rows$Absolute_Difference <- abs(panel_rows$Base_R - panel_rows$Proposed_Engine)
panel_rows$Base_R <- sprintf("%.4f", panel_rows$Base_R)
panel_rows$Proposed_Engine <- sprintf("%.4f", panel_rows$Proposed_Engine)
panel_rows$Absolute_Difference <- sprintf("%.6f", panel_rows$Absolute_Difference)

# Combine with existing table
comparison_table <- rbind(comparison_table, panel_rows)
print(comparison_table)
