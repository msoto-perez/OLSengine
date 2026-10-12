# OLSengine 1.2.1

Changes made in response to peer review of the package description paper.

## Changes in statistical behavior
* `anova_engine()`, `non_parametric = "auto"`, independent groups: when the
  normality test rejects (p < .05), Levene's test (Brown-Forsythe) p <= .10 now
  routes to Welch's ANOVA, and Kruskal-Wallis is used only when Levene p > .10.
  In 1.2.0, Levene p between .01 and .10 still led to Kruskal-Wallis. Messages
  now state that a non-significant Levene test is insufficient evidence of
  unequal variances, not confirmation of equal variances or of genuine
  non-normality. When Welch's ANOVA is chosen with Levene p between .01 and
  .10, the max/min group variance ratio is reported.
* `anova_engine()`, `non_parametric = "auto"`, paired designs: fixed a 1.2.0
  regression where a Levene-based switch to Welch's ANOVA could lead to a paired
  t-test being run while the message reported Welch's ANOVA. Paired designs
  with a normality rejection now use the Wilcoxon signed-rank test, as before
  1.2.0.
* Normality testing for N > 5000 (`ols_engine()`, `anova_engine()`): the
  Kolmogorov-Smirnov test on standardized residuals, whose standard p-value is
  invalid when the mean and variance are estimated from the data, is replaced
  by a Lilliefors-corrected Kolmogorov-Smirnov test with the Dallal-Wilkinson
  (1986) p-value approximation, implemented in base R. The method is reported
  as "Lilliefors". For N > 5000 a note reports the residual skewness and excess
  kurtosis, because normality tests reject trivial departures at that size.

## Corrected messages and documentation
* `did_engine()`: the pre-treatment comparison of treated and control mean
  outcomes is now described as descriptive. A significant difference is
  reported as a NOTE (no longer a WARNING), stating that DiD differences out
  time-invariant level gaps and that assessing parallel trends requires
  multiple pre-treatment periods. The recommendation to include covariates or
  change the identification strategy was removed. Citations are now Roth
  (2022) and Rambachan & Roth (2023). The t-test itself is unchanged.
* `iv_engine()`: the F < 10 rule of thumb is now attributed to Staiger & Stock
  (1997); Stock & Yogo (2005) are cited for critical values. Messages state
  that the first-stage F is the conventional statistic assuming homoskedastic
  errors and that F > 10 does not rule out weak identification. With more than
  one endogenous regressor, a note states that the minimum of per-regressor
  first-stage F statistics is not a valid joint weak-identification test. The
  Sargan messages state the homoskedasticity assumption and no longer describe
  a non-rejection as the restrictions being "satisfied".
* Messages after a non-significant diagnostic test no longer claim the
  assumption is confirmed. They now report the p-value and state that no
  evidence against the assumption was detected, which does not confirm it.
  Affected: the Breusch-Pagan messages in `ols_engine()` (previously
  "Homoskedasticity confirmed" / "assumption is met"), the normality messages
  in `anova_engine()` (previously "Normality assumption met" / "is met"), the
  Hausman message in `panel_engine()` (previously "supports random effects"),
  and the `paper_engine()` fallback message (previously "The model meets all
  diagnosed assumptions").
* Documentation (`paper_engine()`, README, vignette) now states that the IV
  engine treats every right-hand-side variable as endogenous and does not
  support included exogenous controls, and describes the updated ANOVA routing
  and DiD pre-treatment comparison.
* Documented a known limitation: in paired designs the normality check uses
  the residuals of the between-group model, not the paired differences.

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
