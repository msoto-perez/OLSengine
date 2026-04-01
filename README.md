# OLSengine: Transparent and Assisted Linear Modeling in Base R 🚀

**OLSengine** is an open-source R package designed for applied researchers. It provides a robust, zero-dependency mathematical engine for fundamental parametric and non-parametric statistics (OLS, ANOVA, and Logit).

Built under the philosophy of **"Assisted Simplicity"**, OLSengine acts as a methodological customs filter (**"Aduana"**). It unifies model estimation and diagnostics in a single step, alerting researchers to violations of mathematical assumptions and guiding them toward robust alternatives without making automatic decisions behind their backs.

## 🌟 Core Features

* **Zero External Dependencies:** Built entirely on pure Base R matrix algebra and native stats functions for maximum long-term stability and algorithmic transparency.
* **The Methodological Customs (Aduana):** Automatically runs background diagnostics (Breusch-Pagan, Shapiro-Wilk, Levene, VIF, Hosmer-Lemeshow) and outputs actionable, literature-backed warnings.
* **Paper-Ready Outputs:** Returns hierarchical tables formatted for direct inclusion in academic manuscripts (APA style), automatically calculating Effect Sizes (Cohen's f2, Partial Eta-squared, Odds Ratios) and exact p-values.
* **Minimalist Visualization:** Generates publication-ready, grayscale plots (Forest Plots, Means with 95% CIs, and Logistic Probability Curves) without requiring `ggplot2`.

## 📦 Installation

You can install the development version of **OLSengine** directly from GitHub using:

```r
# install.packages("devtools")
devtools::install_github("msoto-perez/OLSengine")
```

## 🛠️ Usage Examples
The package revolves around a single, powerful wrapper function: paper_engine().

### 1. OLS Regression (with Robust SE guidance)
Detects heteroskedasticity and multicollinearity. Users can explicitly request HC3 robust standard errors.

```r
library(OLSengine)

# Standard execution
model_ols <- paper_engine(y ~ x1 + x2, data = my_data, model = "ols")

# Execution with HC3 Robust Standard Errors applied
model_robust <- paper_engine(y ~ x1 + x2, data = my_data, model = "ols", robust = TRUE)

# View APA-ready table
model_robust$tables$Table2_OLS_Estimation

# Generate Forest Plot
plot_engine(model_robust)
```

### 2. Experimental Differences (4-Way ANOVA Engine)
Intelligently handles independent or paired designs, supporting both parametric and non-parametric equivalents (One-Way ANOVA, Kruskal-Wallis, Paired t-test, Wilcoxon).

```r
# Let the "Customs" automatically switch to Non-Parametric if Normality fails
model_exp <- paper_engine(y ~ group, data = experiment_data, model = "anova", non_parametric = "auto")

# Generate Means Plot with 95% CI error bars
plot_engine(model_exp)
```

### 3. Binary Logistic Regression (Logit)
Reports Odds Ratios, McFadden's Pseudo R-Squared, and Classification Accuracy.

```r
model_logit <- paper_engine(buy ~ age + income, data = consumer_data, model = "logit")

# Generate predicted probability curve
plot_engine(model_logit)
```

## 📖 Citation
To cite OLSengine in publications, please use:

Soto-Pérez, M. (2026). 

OLSengine: A transparent and assisted linear modelling engine in base R (v1.0.0). 

Zenodo. https://doi.org/10.5281/zenodo.19375852
