# mSigPlot 2.0.36

* Minor adjustments to plot_ID89 x axis text

# mSigPlot 2.0.35

* Moved `id89_figlabels()` from `R/plot_ID89.R` to its own file
  (`R/id89_figlabels.R`) and exported it.
* Multi-base ID89 figure labels no longer contain colons
  (e.g. `Del(2,):R(5,)` is now rendered as `L(2, )R(5+)` instead of
  `L(2, ):R(5+)`). Re-blessed regression hashes and visual reference
  PNGs for `plot_ID89` / `plot_ID89_peaks`, and regenerated the
  README `example_ID89.png`.

# mSigPlot 2.0.34

* Removed the `stop_at_9` argument from `plot_ID89()`, `plot_ID476()`, and
  `plot_ID476_right()`. The package now always uses the "open" `(N+)`
  repeat-tract label style; users who needed the old `(N-9)` capped style
  should recognise that their upstream indel classifications may have been
  trimmed at 9. In `plot_ID89()` the flag is kept as an internal constant
  (`stop_at_9 <- FALSE`); in the two ID476 variants it was unused and has
  been dropped cleanly.

# mSigPlot 2.0.33

* Refactored `plot_ID89()`: the internal `IndelType` column now derives from
  `catalog_row_order()$ID89` instead of an inline 89-element literal, giving
  a single source of truth for channel names.
* New internal helper `id89_figlabels()` derives x-axis tick labels
  algorithmically from the canonical `IndelType` strings. Open-ended repeat
  tracts are rendered as `(N+)` (e.g. `T(8+)`, `C(7+)`, `R(5+)`) rather than
  the former `(8,9)` / `(7,9)` notation; sub-block bracket labels also use
  the `(N+)` style.
* Removed the unused `Indel3` column from `plot_ID89()` and from
  `type_476_indel_type()` (never read).
* Changed `stop_at_9` default in `plot_ID89()` from `TRUE` to `FALSE`; the
  plot data is unchanged, but `(8+)` now communicates that the user must
  know their upstream trimming.
* Added a peak-label regression test (`plot_ID89 with peak labels`) and a
  corresponding `plot_ID89_peaks` entry in `tests/visual/`.
* Re-blessed regression hashes and visual reference PNGs for `plot_ID89`
  and `plot_ID89_peaks` to match the new label style.

# mSigPlot 2.0.2

* Unified `plot_title_cex` default at `1.0` and `axis_*_cex` defaults
  (`axis_text_x_cex = 0.5`, `axis_title_x_cex = 0.8`, `axis_text_y_cex = 0.7`,
  `axis_title_y_cex = 0.8`) across all bar-plot functions.
* New `title_outside_plot = FALSE` argument on every bar-plot function.
  By default the plot title is now drawn inside the panel (in the `plot_ID83`
  style) via a shared `add_plot_title()` helper. Set `TRUE` for the
  `ggtitle()`-above-the-panel layout previously used by `plot_ID89`,
  `plot_ID476`, and `plot_ID476_right`.
* For `plot_SBS288`, `title_outside_plot = FALSE` prepends `plot_title` to
  each strand label ("Template", "Non-template", "Not-transcribed"); `TRUE`
  keeps the previous `patchwork::plot_annotation()` overall title.
* `plot_ID89`, `plot_ID476`, and `plot_ID476_right` now position the inside
  title in the gap between the tallest bar and the colored block strip.
* New `grid = FALSE` argument on every bar-plot function (added to
  `plot_SBS12`, `plot_DBS144`, `plot_ID89`, `plot_ID476`, `plot_ID476_right`).
  Default is now `FALSE` everywhere (previously `TRUE` in
  `plot_SBS96`, `plot_SBS192`, `plot_DBS78`, `plot_ID83`, `plot_ID166`).
* Added `development/regenerate_readme_figures.R` for rebuilding the
  `man/figures/example_*.png` set after visual changes.

# mSigPlot 2.0.0

* Remove all deprecated functionality

# mSigPlot 1.0.12

* More cleanup for consistency
* Added peak labels to all bar-plot like plots

# mSigPlot 1.0.11

* Removed `show_extra_top_bar` parameter from `plot_ID89()` and `plot_ID89_pdf()`
* Removed associated blocks3 (extra "Del"/"Ins" tier) code from `plot_ID89()`
* `plot_89()` retains `show_extra_top_bar` for backward compatibility but ignores it

# mSigPlot 1.0.10

* Added `plot_ID83_pdf()` and `plot_ID89_pdf()` calling the new-style plot functions
* Removed legacy `plot_83_pdf()` and `plot_89_pdf()`

# mSigPlot 1.0.9

* refactor the "legacy" plots (e.g. plot_89)
* More updates to documentation

# mSigPlot 1.0.8

* Updated error handling and clarified documentation regarding error handling.

# mSigPlot 1.0.7

* First cut at rationalizing function names and arguments. Functions 
  like plot_83, plot_89 now superseded by plot_ID83, plot_ID89, etc.
  Old functions with old args lists kept for backward compatibility.

# mSigPlot 1.0.6

* `plot_89`: automatically adjust `setyaxis` upward when it is less than
  1.1 times the highest bar, preventing bars from being clipped by the
  y-axis limit.

# mSigPlot 1.0.5

* Added runnable `@examples` to all exported functions for CRAN compliance.
* Added `.claude-trace` and `tests/visual` to `.Rbuildignore`.
* Removed all references to ICAMS package from source and tests.
* `R CMD check` passes with 0 errors, 0 warnings, 0 notes.

# mSigPlot 1.0.3

* Fixed y-axis tick labels showing integers instead of decimals for
  proportion/signature catalogs. Affected all bar-chart plot functions.

# mSigPlot 1.0.2

* All bar-chart plot functions now support negative values (e.g. for
  plotting signature differences). Affected functions: `plot_SBS96`,
  `plot_SBS192`, `plot_SBS12`, `plot_SBS288`, `plot_DBS78`, `plot_DBS144`,
  `plot_83`, `plot_89`, `plot_476`, `plot_ID166`.

# mSigPlot 1.0.1

* Adjusted ggrepel arrow heads in plot_476 etc.
* More testing infrastructure

# mSigPlot 1.0.0

* Initial release.
* Plot functions for 10 catalog types: SBS96, SBS192, SBS12, SBS1536,
  DBS78, DBS136, DBS144, ID83, ID89, ID166, ID476.
* Auto-dispatch via `plot_guess()` and `plot_guess_pdf()`.
* Multi-sample PDF export with 5 plots per page.
* Row name validation and automatic reordering.
