Potential Bugs

  1. plot_SBS12 binomial test uses 2-element vector (R/plot_SBS12.R:113) — binom.test(x = mat[, type]) passes a 2-element vector instead of explicit x and n. Works
   but semantics are fragile and depend on row ordering.
  2. plot_guess_pdf + SBS288 will crash — plot_SBS288 returns a patchwork object, but plot_catalog_pdf expects ggplot objects for grid.arrange. Calling
  plot_guess_pdf() with a 288-row catalog will fail at runtime.
  3. Zero-max catalog breaks plot_ID476/plot_ID476_right — max(muts_basis_melt$freq) is used as a divisor for block positioning. If all values are zero, blocks
  collapse to zero height.

  API Consistency

  4. Missing grid parameter in plot_DBS144, plot_SBS12, plot_ID89, plot_ID476, plot_ID476_right.
  5. Missing upper parameter in plot_DBS144, plot_SBS12, plot_ID476, plot_ID476_right.
  6. plot_ID476/plot_ID476_right still use rel() for theme text sizing — unlike other functions that use cex * base_size. The _cex parameters work differently from
   the same-named parameters in other functions.
  7. Inconsistent y-axis label strings — "counts" vs "Counts" vs "Count"; "proportion" vs "Proportion" vs "counts proportion".
  8. plot_ID89 uses ggtitle() while most other functions use annotate("text") for the sample name. Same for plot_ID476/plot_ID476_right.

  Test Coverage Gaps

  9. No tests for y_axis_type_attr propagation — the recent bug fix should have regression tests verifying attr(catalog, "y_axis_type_attr") <- "density" produces
  "mut/million" labels.
  10. density and density.signature catalog types never exercised in tests.
  11. plot_SBS12 strand bias path (with abundance parameter) is completely untested.
  12. add_peak_labels() untested for most functions — only plot_ID476 tests num_peak_labels > 0.

  Architecture

  13. plot_ID89 has 216 lines of inline data (lines 61-277) defining indel_type_4_figurelabel. Should be extracted like type_476_indel_type().
  14. plot_ID476 and plot_ID476_right share ~80% code — data prep, block definitions, label handling are duplicated. A shared internal helper could eliminate this.
  15. dplyr pipe only used in plot_SBS1536 — the %>% + arrange/desc could be replaced with base R order(), potentially allowing dplyr to be dropped from Imports
  entirely (since plot_ID476/plot_ID476_right use namespace-qualified dplyr::filter etc.).