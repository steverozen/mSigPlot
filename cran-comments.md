## Submission

This is the first CRAN submission of mSigPlot. The package provides
publication-quality ggplot2 plotting functions for mutational
signatures and mutational spectra across ten channel counts: SBS
(96, 192, 288, 1536), DBS (78, 136, 144), and indel (83, 89, 166, 476).
`plot_guess()` / `plot_guess_pdf()` dispatch to the appropriate
per-channel-count function by inspecting `nrow(catalog)`.

## Test environments

Checked on GitHub Actions across a five-entry matrix:

* macos-latest, R release
* windows-latest, R release
* ubuntu-22.04, R devel
* ubuntu-latest, R release
* ubuntu-latest, R oldrel-1

All jobs run `R CMD check --as-cran` (via
`r-lib/actions/check-r-package@v2`).

## R CMD check results

All five GitHub Actions jobs passed.

A local `R CMD check --as-cran` on the built tarball with the CRAN
incoming-feasibility check enabled returns:

    0 errors | 0 warnings | 1 note

The single NOTE is the expected "New submission" flag.

## Downstream dependencies

This is a new package; there are no downstream dependencies on CRAN.
