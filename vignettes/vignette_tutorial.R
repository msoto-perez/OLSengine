## ----message=FALSE------------------------------------------------------------
library(OLSengine)

# Simulate data with non-constant variance
set.seed(123)
n <- 200
x <- rnorm(n, 50, 10)
y <- 10 + 0.5 * x + rnorm(n, 0, x * 0.2) # Heteroskedasticity

df <- data.frame(y, x)

# Run the engine
model <- paper_engine(y ~ x, data = df, model = "ols")
model$messages

## -----------------------------------------------------------------------------
model_robust <- paper_engine(y ~ x, data = df, model = "ols", robust = TRUE)
model_robust$tables$Table2_OLS_Estimation
plot_engine(model_robust)

## -----------------------------------------------------------------------------
# Simulating 3 groups with non-normal distribution
set.seed(789)
group_data <- data.frame(
  score = c(rgamma(30, 2, 0.5), rgamma(30, 5, 0.5), rgamma(30, 3, 0.5)),
  group = rep(c("Control", "Treatment A", "Treatment B"), each = 30)
)

# Auto-pilot switches to non-parametric if normality fails
model_anova <- paper_engine(score ~ group, data = group_data, 
                            model = "anova", non_parametric = "auto")

model_anova$tables$Table4_Mean_Differences
plot_engine(model_anova)

## -----------------------------------------------------------------------------
# Simulating binary data
set.seed(101)
n_logit <- 100
age <- rnorm(n_logit, 40, 10)
passed <- rbinom(n_logit, 1, plogis(-5 + 0.12 * age))

logit_df <- data.frame(passed, age)

# Run Logit engine
model_logit <- paper_engine(passed ~ age, data = logit_df, model = "logit")

model_logit$tables$Table2_Logit_Estimation
model_logit$messages
plot_engine(model_logit)

## -----------------------------------------------------------------------------
# Simulate panel data: 50 workers observed over 3 years
set.seed(456)
n_entities <- 50
n_time <- 3

panel_df <- data.frame(
  worker_id = rep(1:n_entities, each = n_time),
  year = rep(2018:2020, times = n_entities),
  wage = rnorm(n_entities * n_time, 50, 10) + 
         rep(rnorm(n_entities, 0, 5), each = n_time), # Entity fixed effect
  experience = rep(5:7, times = n_entities) + rnorm(n_entities * n_time, 0, 1)
)

# Hausman test decides between FE and RE
model_panel <- paper_engine(wage ~ experience, 
                            data = panel_df,
                            model = "panel",
                            entity_id = "worker_id",
                            time_id = "year",
                            method = "auto")

model_panel$tables[[1]]
model_panel$messages
plot_engine(model_panel)

## -----------------------------------------------------------------------------
# Simulate IV data with endogeneity
set.seed(789)
n_iv <- 200

z <- rnorm(n_iv, 10, 3)  # Instrument
u <- rnorm(n_iv, 0, 2)   # Unobserved confounder

x <- 2 + 0.8 * z + u + rnorm(n_iv, sd = 1)  # Endogenous predictor
y <- 5 + 1.5 * x + u + rnorm(n_iv, sd = 3)  # Outcome

iv_df <- data.frame(y, x, z)

# 2SLS estimation
model_iv <- paper_engine(y ~ x, 
                         data = iv_df,
                         model = "iv",
                         instruments = ~ z)

model_iv$tables$Table2_IV_2SLS
model_iv$messages  # Reports first-stage F-stat
plot_engine(model_iv)

## -----------------------------------------------------------------------------
# Simulate policy intervention data
set.seed(321)
n_per_group <- 100

did_df <- data.frame(
  outcome = c(rnorm(n_per_group, 50, 10), rnorm(n_per_group, 53, 10),  # Control: pre/post
              rnorm(n_per_group, 50, 10), rnorm(n_per_group, 58, 10)), # Treated: pre/post
  group = rep(c("Control", "Treated"), each = n_per_group * 2),
  period = rep(c("Pre", "Post"), times = n_per_group * 2)
)

# DiD estimation
model_did <- paper_engine(outcome ~ 1, 
                          data = did_df,
                          model = "did",
                          treatment_var = "group",
                          time_var = "period",
                          treatment_level = "Treated",
                          post_level = "Post")

model_did$tables$Table2_DiD_Estimation
model_did$tables$Group_Means
model_did$messages  # Reports parallel trends test
plot_engine(model_did)

## -----------------------------------------------------------------------------
data(academic_salaries)

# Explore salary determinants
salary_model <- paper_engine(salary ~ rank + discipline + years_since_phd + sex,
                             data = academic_salaries,
                             model = "ols",
                             robust = "auto")

salary_model$tables$Table2_OLS_Estimation
salary_model$messages

