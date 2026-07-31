## Submission summary

This is a bug-fix patch release (1.1.0 -> 1.1.1) of a package already on CRAN.
No new features, no exported function signatures added, removed, or changed.
No new package dependencies.

Three bugs are fixed, all internal to estimation engines reached through the
existing `paper_engine()` entry point:

* `iv_engine()`: an O(n^2) memory/time bug from materializing a full n x n
  projection matrix. Previously failed with "cannot allocate vector" errors
  around n = 100,000; the fix computes the same quantity via associativity,
  in O(n*k) memory, with numerically identical output.
* `anova_engine()`: a hard crash for n > 5,000 (`shapiro.test()`'s sample-size
  limit). Now falls back to a Kolmogorov-Smirnov test above that threshold,
  matching the pattern already used by `ols_engine()`.
* `panel_engine()`: an incorrect Hausman test statistic (did not match
  `plm::phtest()`), caused by an error in the random-effects GLS
  transformation parameter and a covariance-matrix safeguard that diverged
  from the reference algorithm. See the note below.

Full details in NEWS.md.

## Statistical correctness note (panel_engine Hausman fix)

I want to be explicit about this rather than downplay it: the `panel_engine()`
fix is not purely cosmetic. Before the fix, `panel_engine(..., method =
"auto")` could select Random Effects in cases where the correctly-computed
Hausman test rejects it in favor of Fixed Effects -- i.e., it could recommend
the theoretically inconsistent estimator. This was confirmed on a real
dataset (Fatalities, Stock & Watson): the buggy statistic gave chi-sq = 3.28,
p = 0.070 (fails to reject, selects RE), while the corrected statistic gives
chi-sq = 18.35, p = 1.8e-05, matching `plm::phtest()` exactly and correctly
selecting FE.

Anyone who used `panel_engine()`/`paper_engine(model = "panel", method =
"auto")` under v1.1.0 should be aware that the automatic FE/RE selection may
have been wrong for their data and should re-run their analysis under this
version. Results from explicit `method = "fe"` or `method = "re"` calls are
unaffected, since the fix only touches the Hausman test statistic and the
automatic-selection logic that depends on it, not the FE or RE coefficient
estimation itself.

## Test environments

* Local Windows 11, R 4.5.2 (x86_64-w64-mingw32), `R CMD check --as-cran` via
  `devtools::check()`.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

None. This package has no reverse dependencies on CRAN.
