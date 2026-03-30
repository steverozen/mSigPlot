# Plot a DBS136 catalog as a 10-panel heatmap using ggplot2

Creates 10 small 4x4 heatmaps (one per dinucleotide reference class: AC,
AT, GC, CC, CG, CT, TA, TC, TG, TT) using `geom_tile()` with a
white-to-forestgreen color gradient. Includes a maxima-per-class
summary.

## Usage

``` r
plot_DBS136(
  catalog,
  plot_title = NULL,
  base_size = 11,
  title_cex = 1.2,
  axis_label_cex = 0.8,
  panel_label_cex = 1
)
```

## Arguments

- catalog:

  Numeric vector, single-column data.frame, matrix, tibble, or
  data.table.

- plot_title:

  Character. Title displayed at the top.

- base_size:

  Numeric. Base font size in points.

- title_cex:

  Numeric. Multiplier for the plot title size.

- axis_label_cex:

  Numeric. Multiplier for axis base labels.

- panel_label_cex:

  Numeric. Multiplier for panel mutation type labels.

## Value

A ggplot object (assembled via gridExtra).

## Examples

``` r
set.seed(1)
sig <- runif(136)
sig <- sig / sum(sig)
names(sig) <- catalog_row_order()$DBS136
plot_DBS136(sig, plot_title = "Example DBS136 heatmap")

```
