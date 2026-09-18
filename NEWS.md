# OLSengine 1.2.0

## Bug fixes / corrections
* did_engine(): the pre-treatment placebo check is now correctly labelled 
  as a pre-treatment balance check rather than a parallel-trends test. 
  A single pre-treatment period cannot establish parallel trends, which 
  requires observing group trajectories across multiple periods. No 
  change to the underlying statistical test.

## New features
* anova_engine(): when non_parametric = "auto" and Shapiro-Wilk rejects 
  normality, Levene's test (Brown-Forsythe) now informs the correction: 
  clear heteroscedasticity (p < .01) routes to Welch's ANOVA instead of 
  Kruskal-Wallis; ambiguous cases (.01-.10) retain the previous default 
  with an explicit warning; homogeneous variance (p > .10) confirms 
  genuine non-normality as before. Validated via Monte Carlo simulation 
  (see harness/ for reproducible scripts).

# OLSengine 1.1.1

## Bug fixes

* `iv_engine()`: fixed an O(n^2) memory/time bug caused by explicitly forming
  the full n x n projection matrix when computing the second-stage variance.
  The offending step now computes `X_tilde` via associativity instead, giving
  O(n*k) memory. Previously this failed with "cannot allocate vector" errors
  around n = 100,000; it now runs in a fraction of a second at that size.
* `anova_engine()`: fixed a hard crash for n > 5,000, where `shapiro.test()`
  errors out ("sample size must be between 3 and 5000"). It now falls back to
  a Kolmogorov-Smirnov test above that threshold, matching the behavior
  `ols_engine()` already had.
* `panel_engine()`: fixed the internal Hausman test statistic, which did not
  match `plm::phtest()`. The between estimator used to construct the GLS
  transformation parameter (`theta`) was missing its intercept and was not
  isolating the idiosyncratic variance component, and the covariance-matrix
  positive-definiteness safeguard did not match the reference algorithm used
  by `plm::phtest()` (which inverts directly and takes the absolute value of
  the resulting statistic, without a positive-definiteness check). In some
  scenarios this could select the theoretically inconsistent estimator
  (Random Effects when Fixed Effects was actually warranted). See the
  "Statistical correctness note" in this release's `cran-comments.md` for
  scope and mitigation.

# OLSengine 1.1.0

## New features

* Added panel data estimation (`model = "panel"` in `paper_engine()`): fixed
  effects and random effects estimators with automatic Hausman-test-based
  selection (`method = "auto"`, `"fe"`, or `"re"`), plus balanced/unbalanced
  panel diagnostics.
* Added instrumental variables estimation (`model = "iv"`): two-stage least
  squares (2SLS) in pure base R, with first-stage weak-instrument diagnostics.
* Added difference-in-differences estimation (`model = "did"`): treatment x
  post-period interaction model with parallel-trends diagnostics.
* `plot_engine()` gains matching plot types for the three new models: a
  forest plot for panel estimates, a forest plot for 2SLS coefficients, and
  a parallel-trends plot for DiD.
* Added the `academic_salaries` example dataset, used in the new panel/IV/DiD
  vignette sections.

## Documentation

* Expanded `vignette_tutorial.Rmd` with worked examples for panel data, IV,
  and DiD models.
* Updated `paper_engine()` and `plot_engine()` documentation for the new
  parameters (`entity_id`, `time_id`, `method`, `instruments`, `treatment_var`,
  `time_var`, `treatment_level`, `post_level`).
* Updated README to reflect the six available estimation engines.

# OLSengine 1.0.0

* Initial CRAN release: OLS regression, ANOVA/t-tests, and logistic
  regression, with the integrated "Methodological Customs" diagnostics layer
  and APA-formatted output tables and plots.
