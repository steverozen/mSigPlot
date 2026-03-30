# Plot an SBS288 catalog as a 3-panel stacked SBS96 plot

Takes a 288-row catalog (3 strand categories x 96 SBS channels) and
produces a vertically stacked 3-panel SBS96 plot with a shared y-axis
maximum. Row name format: `T:A[C>A]T`, `U:A[C>A]T`, `N:A[C>A]T` where
`T:` is template (transcribed), `U:` is non-template (untranscribed),
and `N:` is not-transcribed (intergenic).

## Usage

``` r
plot_SBS288(catalog, plot_title = NULL, ...)
```

## Arguments

- catalog:

  Numeric vector, single-column data.frame, matrix, tibble, or
  data.table with 288 rows. Row names must have 2-character strand
  prefixes (`T:`, `U:`, `N:`).

- plot_title:

  Character. Optional overall title for the combined plot.

- ...:

  Additional arguments passed to
  [`plot_SBS96()`](https://steverozen.github.io/mSigPlot/reference/plot_SBS96.md).

## Value

A `patchwork` object (printable and compatible with `ggsave()`).

## Examples

``` r
set.seed(1)
sbs96_names <- catalog_row_order()$SBS96
rnames <- c(paste0("T:", sbs96_names),
            paste0("U:", sbs96_names),
            paste0("N:", sbs96_names))
sig <- runif(288)
sig <- sig / sum(sig)
names(sig) <- rnames
plot_SBS288(sig, plot_title = "Example SBS288")
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.

```
