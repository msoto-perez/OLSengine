## Submission summary

This is a minor release (1.1.1 -> 1.2.0) of a package already on CRAN. No
backwards-incompatible changes: no exported function signatures added,
removed, or changed, and no new package dependencies.

Two changes, both in internal estimation-engine logic reached through the
existing `paper_engine()` entry point:

* `did_engine()`: relabels the pre-treatment placebo check. It compares
  treated/control group levels in a single pre-treatment period, which
  tests for pre-existing imbalance, not parallel trends (parallel trends
  requires observing group trajectories across multiple pre-treatment
  periods). This was raised in a journal peer review of the package
  description paper. The underlying statistical test is unchanged -- same
  t-test of pre-treatment levels -- only the WARNING/INFO message wording,
  plot labels, and documentation were corrected to avoid overclaiming.
* `anova_engine()`: new decision logic for the `non_parametric = "auto"`
  path. Previously, whenever Shapiro-Wilk rejected normality, the function
  always fell back to Kruskal-Wallis. It now also consults Levene's test
  (Brown-Forsythe) before deciding: clear heteroscedasticity (Levene
  p < .01) routes to Welch's ANOVA instead, since heteroscedasticity-driven
  apparent non-normality is better addressed by a variance-robust test than
  a rank-based one; ambiguous cases (.01-.10) keep the previous
  Kruskal-Wallis default but flag the ambiguity explicitly in the message;
  clearly homogeneous variance (p > .10) behaves exactly as before. This
  was validated with a 2x2 Monte Carlo design (normality x variance
  homogeneity) and a targeted 2,000-replicate refinement of the
  heteroscedastic/non-normal cell: Welch's ANOVA Type-I error rate is 7.11%
  (95% CI [6.04%, 8.34%]) under combined skew and heteroscedasticity,
  within the pre-specified 7.5% tolerance for that design. Reproducible
  validation scripts are included under `harness/` (excluded from the
  built package via `.Rbuildignore`).

Full details in NEWS.md.

## Test environments

* Local Windows 11, R 4.5.2 (x86_64-w64-mingw32), `R CMD check --as-cran`
  run directly on the built source tarball (`OLSengine_1.2.0.tar.gz`), not
  just `devtools::check()` on the source tree.

## R CMD check results

0 errors | 0 warnings | 1 note

* `checking for future file timestamps ... NOTE` / `unable to verify
  current time`. This package's working copy lives in a Dropbox-synced
  folder; I verified with `Get-ChildItem` that no file in the source tree
  has a modification timestamp later than the current system clock, so
  this is not a stale or mis-dated file in the package. The check step
  itself reports it was unable to verify the current time against an
  external reference, which points to a local network/clock-verification
  hiccup in this environment rather than an issue with the package
  contents. This is a known class of false positive for packages developed
  in cloud-synced folders and is not expected to reproduce on CRAN's own
  build servers.

## Downstream dependencies

None. This package has no reverse dependencies on CRAN.
