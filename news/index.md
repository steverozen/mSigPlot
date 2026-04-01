# Changelog

## mSigPlot 1.0.8

- Updated error handling and clarified documentation regarding error
  handling.

## mSigPlot 1.0.7

- First cut at rationalizing function names and arguments. Functions
  like plot_83, plot_89 now superseded by plot_ID83, plot_ID89, etc. Old
  functions with old args lists kept for backward compatibility.

## mSigPlot 1.0.6

- `plot_89`: automatically adjust `setyaxis` upward when it is less than
  1.1 times the highest bar, preventing bars from being clipped by the
  y-axis limit.

## mSigPlot 1.0.5

- Added runnable `@examples` to all exported functions for CRAN
  compliance.
- Added `.claude-trace` and `tests/visual` to `.Rbuildignore`.
- Removed all references to ICAMS package from source and tests.
- `R CMD check` passes with 0 errors, 0 warnings, 0 notes.

## mSigPlot 1.0.3

- Fixed y-axis tick labels showing integers instead of decimals for
  proportion/signature catalogs. Affected all bar-chart plot functions.

## mSigPlot 1.0.2

- All bar-chart plot functions now support negative values (e.g. for
  plotting signature differences). Affected functions: `plot_SBS96`,
  `plot_SBS192`, `plot_SBS12`, `plot_SBS288`, `plot_DBS78`,
  `plot_DBS144`, `plot_83`, `plot_89`, `plot_476`, `plot_ID166`.

## mSigPlot 1.0.1

- Adjusted ggrepel arrow heads in plot_476 etc.
- More testing infrastructure

## mSigPlot 1.0.0

- Initial release.
- Plot functions for 10 catalog types: SBS96, SBS192, SBS12, SBS1536,
  DBS78, DBS136, DBS144, ID83, ID89, ID166, ID476.
- Auto-dispatch via
  [`plot_guess()`](https://steverozen.github.io/mSigPlot/reference/guess_plots.md)
  and
  [`plot_guess_pdf()`](https://steverozen.github.io/mSigPlot/reference/guess_plots.md).
- Multi-sample PDF export with 5 plots per page.
- Row name validation and automatic reordering.
