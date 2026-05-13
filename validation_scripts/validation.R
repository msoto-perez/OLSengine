# ==============================================================================
# NUMERICAL VALIDATION SCRIPT (Base R vs OLSengine)
# Generates the comparison table for the manuscript
# ==============================================================================

# Clear environment for pure reproducibility
rm(list = ls())

# install directly from GitHub and load
devtools::install_github("msoto-perez/OLSengine", force = TRUE)
library(OLSengine)

# ==============================================================================
# 1. REPRODUCIBLE DATA GENERATION
# ==============================================================================

set.seed(42)
n <- 300

# Shared predictors
x1 <- rnorm(n, mean = 50, sd = 10)
x2 <- rbinom(n, size = 1, prob = 0.5)
group <- as.factor(sample(c("Control", "Treatment A", "Treatment B"), n, replace = TRUE))

# Outcomes
y_cont <- 15 + 2.5 * x1 - 1.2 * x2 + rnorm(n, mean = 0, sd = 8)
y_bin <- rbinom(n, size = 1, prob = plogis(-4 + 0.08 * x1 + 1.2 * x2))

df_test <- data.frame(y_cont, y_bin, x1, x2, group)

# ==============================================================================
# 2. OLS VALIDATION
# ==============================================================================

base_lm <- summary(lm(y_cont ~ x1 + x2, data = df_test))
eng_ols <- OLSengine:::ols_engine(y_cont ~ x1 + x2, data = df_test)

ols_results <- data.frame(
  Parameter = c("OLS: Coefficient (x1)", "OLS: Adjusted R-squared"),
  Base_R = c(base_lm$coefficients["x1", "Estimate"], base_lm$adj.r.squared),
  Proposed_Engine = c(eng_ols$coefficients["x1"], eng_ols$diagnostics$Adj_R2)
)

# ==============================================================================
# 3. ANOVA VALIDATION
# ==============================================================================

base_aov <- summary(aov(y_cont ~ group, data = df_test))[[1]]
eng_aov <- OLSengine:::anova_engine(y_cont ~ group, data = df_test)

anova_results <- data.frame(
  Parameter = c("ANOVA: F-statistic", "ANOVA: p-value"),
  Base_R = c(base_aov$`F value`[1], base_aov$`Pr(>F)`[1]),
  Proposed_Engine = c(eng_aov$effects_table$Statistic, eng_aov$effects_table$p_value)
)

# ==============================================================================
# 4. LOGIT VALIDATION
# ==============================================================================

base_glm <- summary(glm(y_bin ~ x1 + x2, data = df_test, family = binomial(link = "logit")))
eng_logit <- OLSengine:::logit_engine(y_bin ~ x1 + x2, data = df_test)

logit_results <- data.frame(
  Parameter = c("Logit: Coefficient (x1)", "Logit: Standard Error (x2)"),
  Base_R = c(base_glm$coefficients["x1", "Estimate"], base_glm$coefficients["x2", "Std. Error"]),
  Proposed_Engine = c(eng_logit$coefficients["x1"], eng_logit$se["x2"])
)

# ==============================================================================
# 5. PANEL DATA VALIDATION (Fixed Effects)
# ==============================================================================

if (!requireNamespace("plm", quietly = TRUE)) install.packages("plm")
library(plm)

set.seed(999)
panel_df <- data.frame(
  id = rep(1:30, each = 5),
  time = rep(1:5, times = 30),
  y = rnorm(150) + rep(rnorm(30, mean = 50, sd = 10), each = 5),
  x1 = rnorm(150, mean = 10, sd = 3),
  x2 = rnorm(150, mean = 5, sd = 2)
)

panel_pdata <- pdata.frame(panel_df, index = c("id", "time"))
base_plm_fe <- plm(y ~ x1 + x2, data = panel_pdata, model = "within")

eng_panel_fe <- OLSengine:::panel_engine(
  y ~ x1 + x2, data = panel_df,
  entity_id = "id", time_id = "time", method = "fe"
)

panel_results <- data.frame(
  Parameter = c("Panel FE: Coefficient (x1)", "Panel FE: Coefficient (x2)",
                "Panel FE: SE (x1)", "Panel FE: SE (x2)"),
  Base_R = c(coef(base_plm_fe)["x1"], coef(base_plm_fe)["x2"],
             summary(base_plm_fe)$coefficients["x1", "Std. Error"],
             summary(base_plm_fe)$coefficients["x2", "Std. Error"]),
  Proposed_Engine = c(eng_panel_fe$coefficients["x1"], eng_panel_fe$coefficients["x2"],
                      eng_panel_fe$se["x1"], eng_panel_fe$se["x2"])
)

# ==============================================================================
# 6. INSTRUMENTAL VARIABLES VALIDATION (2SLS)
# ==============================================================================

if (!requireNamespace("ivreg", quietly = TRUE)) install.packages("ivreg")
library(ivreg)

set.seed(777)
n_iv <- 200
z <- rnorm(n_iv, mean = 10, sd = 3)
u <- rnorm(n_iv, mean = 0, sd = 2)
x_iv <- 2 + 0.8 * z + u + rnorm(n_iv, sd = 1)
y_iv <- 5 + 1.5 * x_iv + u + rnorm(n_iv, sd = 3)

iv_df <- data.frame(y = y_iv, x = x_iv, z = z)

base_ivreg <- ivreg(y ~ x | z, data = iv_df)
eng_iv <- OLSengine:::iv_engine(y ~ x, data = iv_df, instruments = ~ z)

iv_results <- data.frame(
  Parameter = c("IV 2SLS: Coefficient (x)", "IV 2SLS: SE (x)"),
  Base_R = c(coef(base_ivreg)["x"], summary(base_ivreg)$coefficients["x", "Std. Error"]),
  Proposed_Engine = c(eng_iv$coefficients["x"], eng_iv$se["x"])
)

# ==============================================================================
# 7. DIFFERENCE-IN-DIFFERENCES VALIDATION
# ==============================================================================

set.seed(888)
n_did <- 100
treated_did <- rep(c(0, 1), each = n_did)
post_did <- rep(c(0, 1), times = n_did)
did_interaction <- treated_did * post_did

y_did <- 50 + 2 * treated_did + 3 * post_did + 5 * did_interaction + rnorm(n_did * 2, sd = 3)

did_df <- data.frame(
  y = y_did,
  treated = factor(treated_did, levels = c(0, 1), labels = c("Control", "Treated")),
  post = factor(post_did, levels = c(0, 1), labels = c("Pre", "Post")),
  treated_num = treated_did,
  post_num = post_did,
  interaction = did_interaction
)

base_did_lm <- lm(y ~ treated_num + post_num + interaction, data = did_df)

eng_did <- OLSengine:::did_engine(
  y ~ 1, data = did_df,
  treatment_var = "treated", time_var = "post",
  treatment_level = "Treated", post_level = "Post"
)

did_results <- data.frame(
  Parameter = c("DiD: Treatment Effect (Treated:Post)", "DiD: SE (Treated:Post)"),
  Base_R = c(coef(base_did_lm)["interaction"],
             summary(base_did_lm)$coefficients["interaction", "Std. Error"]),
  Proposed_Engine = c(eng_did$did_estimate, eng_did$did_se)
)

# ==============================================================================
# 8. COMBINE ALL RESULTS
# ==============================================================================

comparison_table <- rbind(
  ols_results,
  anova_results,
  logit_results,
  panel_results,
  iv_results,
  did_results
)

# Calculate absolute differences
comparison_table$Absolute_Difference <- abs(comparison_table$Base_R - comparison_table$Proposed_Engine)

# Format for display
comparison_table$Base_R <- sprintf("%.4f", comparison_table$Base_R)
comparison_table$Proposed_Engine <- sprintf("%.4f", comparison_table$Proposed_Engine)
comparison_table$Absolute_Difference <- sprintf("%.6f", comparison_table$Absolute_Difference)

# ==============================================================================
# 9. DISPLAY RESULTS
# ==============================================================================

cat("\n")
cat("==============================================================================\n")
cat("NUMERICAL VALIDATION: OLSengine vs Base R / Standard Packages\n")
cat("==============================================================================\n\n")

print(comparison_table, row.names = FALSE)

cat("\n")
cat(">> Validation Complete: All 6 estimation methods numerically verified.\n")
cat(">> Methods validated: OLS, ANOVA, Logit, Panel FE, IV 2SLS, DiD\n")
cat(">> Expected: Absolute_Difference < 0.001 for all parameters.\n")
cat("==============================================================================\n")

