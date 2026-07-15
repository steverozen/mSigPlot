## Submission

Resubmission as 2.0.41 following the 2.0.38 CRAN pretest cycle.
Changes since 2.0.38 (all documented in `NEWS.md`):

* 2.0.39: Consolidated stapled-row-name documentation onto the shared
  `@param catalog` description across bar-plot functions; accepted both
  `Del(C):R(6,9)` and `Del(C):R(6,)` in `normalize_catalog()` for ID89
  to avoid breaking calls with older data.
* 2.0.40: Renamed internal helper `type_476_indel_type()` to
  `catalog_and_label_order_476()` (file rename included) and updated
  callers and docs; corrected 476-type row-name labels.
* 2.0.41: Added error-checking for scalar and non-numeric values of
  the `ymax` / `ylim` argument in `plot_ID*` functions
  (`R/check_ylim.R`) and matching tests. Fixed two prose typos
  (`agains` -> `against` in the vignette, `Implemeted` -> `Implemented`
  in `NEWS.md`).

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

Also checked locally under R 4.6.0 (stable) and R-devel on Zorin OS
18.1 with `devtools::check(cran = TRUE, incoming = TRUE, remote = TRUE)`
and system `aspell` installed.

## R CMD check results

All five GitHub Actions jobs passed:
<https://github.com/steverozen/mSigPlot/actions/runs/29381414098>

Each of the five jobs reports 1 NOTE with identical content:

    * checking CRAN incoming feasibility ... NOTE
    Maintainer: 'Steven Rozen <steverozen@pm.me>'
    No suitable spell-checker program found

This NOTE reflects the fact that the `r-lib/actions` GitHub runners do
not install `aspell`, so the incoming-feasibility spell check could
not run there. On the local check with `aspell` present, no misspelled
words are reported and no other NOTEs, WARNINGs, or ERRORs are
raised.

Local `R CMD check --as-cran` with incoming feasibility enabled:

    R-stable (4.6.0): 0 errors | 0 warnings | 0 notes
    R-devel:          0 errors | 0 warnings | 0 notes

## Downstream dependencies

There are no downstream dependencies on CRAN.
