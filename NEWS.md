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
