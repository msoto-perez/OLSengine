## Submission summary

This is an update from version 1.2.0 (published on CRAN on 2026-09-18) to
1.2.1. All changes respond to peer review of the package description paper.
There are no backwards-incompatible changes: no exported function signatures
were added, removed, or changed, and no new package dependencies were added
(base R only).

Changes in statistical behavior:

* `anova_engine()`, `non_parametric = "auto"`, independent groups: when the
  normality test rejects, Levene's test (Brown-Forsythe) p <= .10 now routes
  to Welch's ANOVA; Kruskal-Wallis is used only when Levene p > .10. In 1.2.0,
  Levene p between .01 and .10 still led to Kruskal-Wallis.
* `anova_engine()`, `non_parametric = "auto"`, paired designs: fixes a 1.2.0
  regression in which a Levene-based switch to Welch's ANOVA could lead to a
  paired t-test being run while the message reported Welch's ANOVA. Paired
  designs with a normality rejection use the Wilcoxon signed-rank test again.
* Normality testing for N > 5000 (`ols_engine()`, `anova_engine()`): the
  Kolmogorov-Smirnov test on standardized residuals, whose standard p-value is
  invalid when the mean and variance are estimated from the data, was replaced
  by a Lilliefors-corrected Kolmogorov-Smirnov test with the Dallal-Wilkinson
  (1986) p-value approximation, implemented in base R. At that sample size a
  note also reports the residual skewness and excess kurtosis.

Corrected messages and documentation:

* `did_engine()`: the comparison of treated and control mean outcomes in the
  single pre-treatment period is now reported as descriptive (a NOTE rather
  than a WARNING), stating that it is not a test of parallel trends, which
  requires multiple pre-treatment periods. The underlying t-test is unchanged.
* `iv_engine()`: the F < 10 rule of thumb is attributed to Staiger & Stock
  (1997); messages state that the first-stage F assumes homoskedastic errors,
  that F > 10 does not rule out weak identification, and that the minimum of
  per-regressor first-stage F statistics is not a valid joint test with more
  than one endogenous regressor. The Sargan messages state the
  homoskedasticity assumption.
* Messages after a non-significant diagnostic test (Breusch-Pagan, residual
  normality, Hausman) no longer claim the assumption is confirmed; they report
  the p-value and state that the absence of evidence does not confirm the
  assumption. The `paper_engine()` fallback message no longer states that the
  model meets all diagnosed assumptions.
* Documentation states that the IV engine treats every right-hand-side
  variable as endogenous (no included exogenous controls), and that in paired
  designs the normality check uses between-group residuals rather than paired
  differences (known limitation).

Full details in NEWS.md (entry 1.2.1).

## Test environments

* Local Windows 11, R 4.5.2 (x86_64-w64-mingw32): `R CMD build`, then
  `R CMD check --as-cran` run on the built tarball (`OLSengine_1.2.1.tar.gz`).
* win-builder, R Under development (unstable) (2026-10-10 r90659 ucrt):
  Status: OK.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

None. This package has no reverse dependencies on CRAN.
