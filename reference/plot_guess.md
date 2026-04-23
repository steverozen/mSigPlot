# plot_guess, plot_guess_pdf

Automatically select the appropriate plotting function based on the
number of rows in the catalog.

## Usage

``` r
plot_guess(catalog, ...)

plot_guess_pdf(catalog, filename, ...)
```

## Arguments

- catalog:

  Numeric vector, single-column data.frame, matrix, tibble, or
  data.table. The number of rows (or length) determines which plotting
  function is used:

  - 1536 rows: calls
    [`plot_SBS1536()`](https://steverozen.github.io/mSigPlot/reference/heatmap_plots.md)

  - 476 rows: calls
    [`plot_ID476()`](https://steverozen.github.io/mSigPlot/reference/bar_plots.md)

  - 288 rows: calls
    [`plot_SBS288()`](https://steverozen.github.io/mSigPlot/reference/bar_plots.md)

  - 192 rows: calls
    [`plot_SBS192()`](https://steverozen.github.io/mSigPlot/reference/bar_plots.md)

  - 166 rows: calls
    [`plot_ID166()`](https://steverozen.github.io/mSigPlot/reference/bar_plots.md)

  - 144 rows: calls
    [`plot_DBS144()`](https://steverozen.github.io/mSigPlot/reference/bar_plots.md)

  - 136 rows: calls
    [`plot_DBS136()`](https://steverozen.github.io/mSigPlot/reference/heatmap_plots.md)

  - 96 rows: calls
    [`plot_SBS96()`](https://steverozen.github.io/mSigPlot/reference/bar_plots.md)

  - 89 rows: calls
    [`plot_ID89()`](https://steverozen.github.io/mSigPlot/reference/bar_plots.md)

  - 83 rows: calls
    [`plot_ID83()`](https://steverozen.github.io/mSigPlot/reference/bar_plots.md)

  - 78 rows: calls
    [`plot_DBS78()`](https://steverozen.github.io/mSigPlot/reference/bar_plots.md)

  The column names of `catalog` are used as plot titles.

- ...:

  Additional arguments passed to the underlying plotting function.

- filename:

  Character. Path to the output PDF file (`plot_guess_pdf` only).

## Value

`plot_guess()` returns a plot object. The exact class depends on the
dispatched function: most return a ggplot object; SBS288, DBS136, and
SBS1536 return patchwork objects. Note: adding ggplot2 layers with `+`
to a patchwork object (e.g. `+ ggtitle()`) applies only to the last
sub-plot, not the composite; use
[`patchwork::plot_annotation()`](https://patchwork.data-imaginist.com/reference/plot_annotation.html)
instead. `plot_guess_pdf()` returns NULL invisibly (called for side
effect of creating a PDF file).

## Details

`plot_guess()` plots a single sample. `plot_guess_pdf()` creates a
multi-page PDF file containing plots for multiple samples, arranged with
5 samples per page (except heatmap types: 1 per page). Uses Cairo for
high-quality PDF rendering.

## See also

[`plot_SBS96()`](https://steverozen.github.io/mSigPlot/reference/bar_plots.md),
[`plot_SBS192()`](https://steverozen.github.io/mSigPlot/reference/bar_plots.md),
[`plot_SBS288()`](https://steverozen.github.io/mSigPlot/reference/bar_plots.md),
[`plot_SBS1536()`](https://steverozen.github.io/mSigPlot/reference/heatmap_plots.md),
[`plot_DBS78()`](https://steverozen.github.io/mSigPlot/reference/bar_plots.md),
[`plot_DBS136()`](https://steverozen.github.io/mSigPlot/reference/heatmap_plots.md),
[`plot_DBS144()`](https://steverozen.github.io/mSigPlot/reference/bar_plots.md),
[`plot_ID166()`](https://steverozen.github.io/mSigPlot/reference/bar_plots.md),
[`plot_ID476()`](https://steverozen.github.io/mSigPlot/reference/bar_plots.md),
[`plot_ID89()`](https://steverozen.github.io/mSigPlot/reference/bar_plots.md),
[`plot_ID83()`](https://steverozen.github.io/mSigPlot/reference/bar_plots.md)

## Examples

``` r
# Auto-detect a 96-channel catalog and dispatch to plot_SBS96
set.seed(1)
sig <- runif(96)
sig <- sig / sum(sig)
names(sig) <- catalog_row_order()$SBS96
plot_guess(sig, plot_title = "Auto-detected SBS96")


if (FALSE) { # \dontrun{
sig <- matrix(runif(96 * 3), nrow = 96)
rownames(sig) <- catalog_row_order()$SBS96
colnames(sig) <- paste0("Sig", 1:3)
plot_guess_pdf(sig, filename = "auto_detected.pdf")
} # }
```
