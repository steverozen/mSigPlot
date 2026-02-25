# Plot an SBS288 catalog as a 3-panel stacked SBS96 plot

Takes a 288-row catalog (3 strand categories x 96 SBS channels) and
produces a vertically stacked 3-panel SBS96 plot with a shared y-axis
maximum. Row name format: `T:A[C>A]T`, `U:A[C>A]T`, `N:A[C>A]T` where
`T:` is template (transcribed), `U:` is non-template (untranscribed),
and `N:` is not-transcribed (intergenic).

## Usage

``` r
plot_SBS288_test(catalog, plot_title = NULL, ...)
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
