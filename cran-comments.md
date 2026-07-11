## Submission summary

This is an update of a package already on CRAN (currently 1.0.0), bumping the
version to 1.1.0.

New in this release:

* Three new estimation engines reachable through the existing `paper_engine()`
  entry point: panel data (fixed/random effects with Hausman test),
  instrumental variables (2SLS), and difference-in-differences.
* Matching plot types in `plot_engine()` for the three new engines.
* One new example dataset (`academic_salaries`).
* Expanded vignette and documentation covering the new engines.

No exported function signatures were removed or changed in a
backwards-incompatible way; the new engines are only reachable through new,
optional arguments to `paper_engine()` and `plot_engine()`. No new package
dependencies were added (`Imports`/`Suggests` are unchanged from 1.0.0).

See NEWS.md for the full list of changes.

## Test environments

* Local Windows 11, R 4.5.2 (x86_64-w64-mingw32), `R CMD check --as-cran`
  via `devtools::check()`.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

None. This package has no reverse dependencies on CRAN.
