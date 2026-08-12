## Submission

New release 2.0.42, following the accepted 2.0.41.

This is a small correctness and documentation release:

* Corrected the top bar label on `plot_ID89()` plots. The second block
  was labelled "Del 1 T (2-4)" but covers deletions of one T in
  repeats of length 1 through 4, so it now reads "Del 1 T (1-4)".
  Regression hashes and visual reference images were re-blessed to
  match.
* Wrapped the `plot_DBS144()` and `plot_ID166()` examples in
  `\donttest{}` so the merged `bar_plots` help page keeps its example
  run time under the 5 second threshold. Both examples are still
  checked under `--run-donttest`.
* Documentation and developer-tooling changes only otherwise (see
  `NEWS.md`): the two visual regression systems used by the package
  are now described in `CLAUDE.md` and `tests/visual/README.md`.

There are no user-visible API changes and no changes to function
signatures or return values.

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
`r-lib/actions/check-r-package@v2`) with `_R_CHECK_CRAN_INCOMING_`,
`_R_CHECK_CRAN_INCOMING_REMOTE_`, and
`_R_CHECK_CRAN_INCOMING_CHECK_FILE_URIS_` set to `true`, to match
CRAN's pretest configuration.

`_R_CHECK_CRAN_INCOMING_USE_ASPELL_` is deliberately not set on the
runners. They have no system spell-checker installed, so enabling it
only produced a "No suitable spell-checker program found" NOTE on
every job (as reported in the 2.0.41 submission) rather than any
actual spelling coverage. Spelling is instead checked locally with
`devtools::spell_check()` and system `aspell` present, which reports
only domain vocabulary (mutational-signature terminology such as
"trinucleotide", "indel", "intergenic", "microhomology") and no
misspellings.

Also checked locally under R 4.6.0 (stable) and R-devel
(2026-04-23 r89955) on Zorin OS 18.1 with
`devtools::check(cran = TRUE, incoming = TRUE, remote = TRUE)`.

## R CMD check results

All five GitHub Actions jobs passed with no NOTEs, WARNINGs, or
ERRORs:
<https://github.com/steverozen/mSigPlot/actions/runs/31560645890>

Each of the five jobs reports:

    Status: OK

This is an improvement on the 2.0.41 submission, where every runner
reported a "No suitable spell-checker program found" NOTE. See the
note on `_R_CHECK_CRAN_INCOMING_USE_ASPELL_` above.

Local `R CMD check --as-cran` with incoming feasibility enabled:

    R-stable (4.6.0): 0 errors | 0 warnings | 0 notes
    R-devel:          0 errors | 0 warnings | 0 notes

## Downstream dependencies

There are no downstream dependencies on CRAN.
