# Changelog

## mSigPlot 1.0.1

- Adjusted ggrepel arrow heads in plot_476 etc.
- More testing infrastructure

## mSigPlot 1.0.0

- Initial release.
- Plot functions for 10 catalog types: SBS96, SBS192, SBS12, SBS1536,
  DBS78, DBS136, DBS144, ID83, ID89, ID166, ID476.
- Auto-dispatch via
  [`plot_guess()`](https://steverozen.github.io/mSigPlot/reference/plot_guess.md)
  and
  [`plot_guess_pdf()`](https://steverozen.github.io/mSigPlot/reference/plot_guess_pdf.md).
- Multi-sample PDF export with 5 plots per page.
- Row name validation and automatic reordering.
