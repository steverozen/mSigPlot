## Submission

Resubmission of 2.0.36 as 2.0.37. The pretest of 2.0.36 emitted one
NOTE flagging three "Possibly invalid file URIs" in `README.md`
(`man/plot_SBS12.Rd`, `man/plot_DBS144.Rd`, `man/plot_ID166.Rd`).
Those links have been replaced with in-page anchors to new gallery
sections (SBS12, DBS144, ID166), each with an example figure, matching
the treatment of the other plot functions in the README. No code
changes.

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

All five GitHub Actions jobs passed:
https://github.com/steverozen/mSigPlot/actions/runs/24932112133

A local `R CMD check --as-cran` under R-devel (r89955, the same
revision CRAN's pretest used) with the CRAN incoming-feasibility
checks enabled returns:

    0 errors | 0 warnings | 1 note

The single NOTE is the expected "New submission" flag. The R-devel
pretest of 2.0.36 also flagged three invalid file URIs in `README.md`;
those have been fixed in 2.0.37 and no longer appear in either local
R-devel or CI output.

## Downstream dependencies

This is a new package; there are no downstream dependencies on CRAN.
