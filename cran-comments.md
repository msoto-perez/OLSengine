## Submission summary

This is an update of a package already on CRAN (currently 1.0.0), bumping the
version to 1.1.0. This is a major update: it adds three new estimation
engines (panel data, instrumental variables, and difference-in-differences)
reachable through the existing `paper_engine()` entry point.

New in this release:

* Three new estimation engines: panel data (fixed/random effects with
  Hausman test), instrumental variables (2SLS), and difference-in-differences.
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
* win-builder (R-devel), via `devtools::check_win_devel()`.

## R CMD check results

1 NOTE on both local and win-builder checks:

* Possibly misspelled words in DESCRIPTION: "Hausman", "SLS". Both are valid
  technical terms (the Hausman specification test, and 2SLS / two-stage least
  squares) and are spelled correctly.

An initial win-builder check also flagged a broken URL (404) in
`man/academic_salaries.Rd`, pointing to a since-moved John Fox companion
website. This has been corrected to the current URL and no longer appears.

## Downstream dependencies

None. This package has no reverse dependencies on CRAN.
