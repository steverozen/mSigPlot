## Submission

Resubmission of 2.0.37 as 2.0.38 following CRAN reviewer feedback:

* Removed single-quotes around SBS and DBS in the `Description` field
  of `DESCRIPTION`.
* Added literature references to the `Description` field.
* Changed `\dontrun{}` to `\donttest{}` in examples for `plot_SBS288()`
  and `plot_guess_pdf()`.

The package provides publication-quality ggplot2 plotting functions
for mutational signatures and mutational spectra across ten channel
counts: SBS (96, 192, 288, 1536), DBS (78, 136, 144), and indel (83,
89, 166, 476). `plot_guess()` / `plot_guess_pdf()` dispatch to the
appropriate per-channel-count function by inspecting `nrow(catalog)`.

## Test environments

Checked on GitHub Actions across a five-entry matrix:

* macos-latest, R release
* windows-latest, R release
* ubuntu-22.04, R devel
* ubuntu-latest, R release
* ubuntu-latest, R oldrel-1

All jobs run `R CMD check --as-cran` (via
`r-lib/actions/check-r-package@v2`) with
`_R_CHECK_CRAN_INCOMING_`, `_R_CHECK_CRAN_INCOMING_REMOTE_`,
`_R_CHECK_CRAN_INCOMING_CHECK_FILE_URIS_`, and
`_R_CHECK_CRAN_INCOMING_USE_ASPELL_` all set to `true`, to match
CRAN's pretest configuration.

## R CMD check results

All five GitHub Actions jobs passed with 1 NOTE each:
https://github.com/steverozen/mSigPlot/actions/runs/25197750959

The single NOTE on every platform is the expected "New submission" flag
from the CRAN incoming-feasibility check. No misspelled words, no
invalid file URIs.

A local `R CMD check --as-cran` with the CRAN incoming-feasibility
checks enabled returns:

    R-stable: 0 errors | 0 warnings | 2 notes
    R-devel:  0 errors | 0 warnings | 1 note

The R-stable run adds one additional NOTE ("unable to verify current
time") that reflects a local network restriction; it does not appear on
CI or on CRAN's machines.

## Downstream dependencies

This is a new package; there are no downstream dependencies on CRAN.
