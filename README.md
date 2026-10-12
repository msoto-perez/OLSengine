# OLSengine: Transparent Linear and Causal Inference Models 🚀

**OLSengine** is an open-source R package designed for applied researchers in social sciences. It provides a comprehensive, zero-dependency mathematical engine for fundamental statistical methods and modern causal inference techniques.

Built under the philosophy of **"Assisted Simplicity"**, OLSengine acts as a methodological customs filter (**"Aduana"**). It unifies model estimation and diagnostics in a single step, alerting researchers to violations of mathematical assumptions and guiding them toward robust alternatives without making automatic decisions behind their backs.

## 🌟 Core Features

* **Six Estimation Engines:** OLS regression, ANOVA/t-tests, logistic regression, panel data (FE/RE), instrumental variables (2SLS), and difference-in-differences (DiD).
* **Zero External Dependencies:** Built entirely on pure Base R matrix algebra and native stats functions for maximum long-term stability and algorithmic transparency.
* **The Methodological Customs (Aduana):** Automatically runs background diagnostics (Breusch-Pagan, Shapiro-Wilk/Lilliefors, Levene, VIF, Hosmer-Lemeshow, Hausman, first-stage F for weak instruments, a descriptive pre-treatment level comparison for DiD) and outputs actionable, literature-backed warnings.
* **Paper-Ready Outputs:** Returns hierarchical tables formatted for direct inclusion in academic manuscripts (APA style), automatically calculating effect sizes and exact p-values.
* **Publication-Ready Plots:** Generates APA-style, grayscale plots (forest plots, group means, logistic curves, panel trends, DiD group means with counterfactual) without requiring `ggplot2`.

## 📦 Installation

Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("msoto-perez/OLSengine")
```

## 🛠️ Quick Start

The package revolves around a single, powerful wrapper function: `paper_engine()`.

### 1. OLS Regression (with Robust SE)

Detects heteroskedasticity and multicollinearity. HC3 robust standard errors available.

```r
library(OLSengine)

# Standard OLS
model_ols <- paper_engine(y ~ x1 + x2, data = my_data, model = "ols")

# With HC3 Robust Standard Errors
model_robust <- paper_engine(y ~ x1 + x2, data = my_data, model = "ols", robust = TRUE)

# View results
model_robust$tables$Table2_OLS_Estimation
model_robust$messages

# Generate forest plot
plot_engine(model_robust)
```

### 2. ANOVA / t-tests (4-Way Engine)

Handles independent or paired designs, parametric and non-parametric tests.

```r
# Auto-pilot: if residual normality is rejected, Levene's test (Brown-Forsythe)
# routes to Welch's ANOVA (p <= .10) or Kruskal-Wallis (p > .10)
model_anova <- paper_engine(score ~ group, data = experiment_data, 
                            model = "anova", non_parametric = "auto")

# Generate group means plot with 95% CI
plot_engine(model_anova)
```

### 3. Logistic Regression

Reports Odds Ratios, McFadden's and Nagelkerke's Pseudo R², classification accuracy.

```r
model_logit <- paper_engine(purchased ~ age + income, data = consumer_data, 
                            model = "logit")

# Generate predicted probability curve
plot_engine(model_logit)
```

### 4. Panel Data (Fixed/Random Effects)

Hausman test automatically selects between fixed and random effects.

```r
model_panel <- paper_engine(wage ~ experience + education, 
                            data = panel_data, 
                            model = "panel",
                            entity_id = "worker_id",
                            time_id = "year",
                            method = "auto")  # Hausman test decides

plot_engine(model_panel)
```

### 5. Instrumental Variables (2SLS)

Reports the conventional first-stage F statistic, which assumes homoskedastic errors, with a rule-of-thumb weak-instrument warning at F < 10 (Staiger & Stock, 1997); F above 10 does not rule out weak identification. Overidentification is tested with the Sargan test, which also assumes homoskedasticity. Every right-hand-side variable is treated as endogenous: included exogenous control variables are not supported.

```r
model_iv <- paper_engine(education ~ income, 
                         data = wage_data,
                         model = "iv",
                         instruments = ~ father_education + region)

# Diagnostics include first-stage F-stat
model_iv$messages
plot_engine(model_iv)
```

### 6. Difference-in-Differences

Estimates the two-group, two-period DiD effect and reports a descriptive comparison of pre-treatment group means. With a single pre-treatment period this is not a test of parallel trends, which requires multiple pre-treatment periods.

```r
model_did <- paper_engine(outcome ~ 1, 
                          data = policy_data,
                          model = "did",
                          treatment_var = "treated",
                          time_var = "period",
                          treatment_level = "Treated",
                          post_level = "Post")

# Plot shows pre/post levels, the counterfactual, and the treatment effect
plot_engine(model_did)
```

## 📊 Example Dataset

The package includes `academic_salaries`, a real dataset of 397 U.S. college professors:

```r
data(academic_salaries)

# Explore salary determinants
salary_model <- paper_engine(salary ~ rank + discipline + years_since_phd + sex,
                             data = academic_salaries,
                             model = "ols",
                             robust = "auto")
```

## 🔬 Numerical Validation

All engines have been validated against standard R packages (`lm`, `aov`, `glm`, `plm`, `ivreg`) with numerical precision < 0.001. See `validation.R` for complete verification.

## 📖 Citation

To cite OLSengine in publications:

```
Soto-Pérez, M. (2026). OLSengine: A transparent and assisted econometric 
modelling engine in base R (v1.2.1). R package. 
https://github.com/msoto-perez/OLSengine
```

## 🤝 Contributing

Issues and pull requests are welcome at: https://github.com/msoto-perez/OLSengine/issues

## 📄 License

MIT License - see LICENSE file for details.
