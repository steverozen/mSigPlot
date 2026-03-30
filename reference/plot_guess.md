# Plot mutational signature catalog with automatic channel detection

Automatically selects the appropriate plotting function based on the
number of rows in the catalog.

## Usage

``` r
plot_guess(catalog, ...)
```

## Arguments

- catalog:

  Numeric vector, single-column data.frame, matrix, tibble, or
  data.table. The number of rows (or length) determines which plotting
  function is used:

  - 1536 rows: calls
    [`plot_SBS1536()`](https://steverozen.github.io/mSigPlot/reference/plot_SBS1536.md)

  - 476 rows: calls
    [`plot_476()`](https://steverozen.github.io/mSigPlot/reference/plot_476.md)

  - 192 rows: calls
    [`plot_SBS192()`](https://steverozen.github.io/mSigPlot/reference/plot_SBS192.md)

  - 166 rows: calls
    [`plot_ID166()`](https://steverozen.github.io/mSigPlot/reference/plot_ID166.md)

  - 144 rows: calls
    [`plot_DBS144()`](https://steverozen.github.io/mSigPlot/reference/plot_DBS144.md)

  - 136 rows: calls
    [`plot_DBS136()`](https://steverozen.github.io/mSigPlot/reference/plot_DBS136.md)

  - 96 rows: calls
    [`plot_SBS96()`](https://steverozen.github.io/mSigPlot/reference/plot_SBS96.md)

  - 89 rows: calls
    [`plot_89()`](https://steverozen.github.io/mSigPlot/reference/plot_89.md)

  - 83 rows: calls
    [`plot_83()`](https://steverozen.github.io/mSigPlot/reference/plot_83.md)

  - 78 rows: calls
    [`plot_DBS78()`](https://steverozen.github.io/mSigPlot/reference/plot_DBS78.md)

- ...:

  Additional arguments passed to the underlying plotting function.

## Value

A ggplot object from the appropriate plotting function.

## See also

[`plot_SBS96()`](https://steverozen.github.io/mSigPlot/reference/plot_SBS96.md),
[`plot_SBS192()`](https://steverozen.github.io/mSigPlot/reference/plot_SBS192.md),
[`plot_SBS1536()`](https://steverozen.github.io/mSigPlot/reference/plot_SBS1536.md),
[`plot_DBS78()`](https://steverozen.github.io/mSigPlot/reference/plot_DBS78.md),
[`plot_DBS136()`](https://steverozen.github.io/mSigPlot/reference/plot_DBS136.md),
[`plot_DBS144()`](https://steverozen.github.io/mSigPlot/reference/plot_DBS144.md),
[`plot_ID166()`](https://steverozen.github.io/mSigPlot/reference/plot_ID166.md),
[`plot_476()`](https://steverozen.github.io/mSigPlot/reference/plot_476.md),
[`plot_89()`](https://steverozen.github.io/mSigPlot/reference/plot_89.md),
[`plot_83()`](https://steverozen.github.io/mSigPlot/reference/plot_83.md)

## Examples

``` r
# Auto-detect a 96-channel catalog and dispatch to plot_SBS96
set.seed(1)
sig <- runif(96)
sig <- sig / sum(sig)
names(sig) <- catalog_row_order()$SBS96
plot_guess(sig, plot_title = "Auto-detected SBS96")

```
