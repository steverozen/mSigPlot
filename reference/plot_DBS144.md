# Plot a DBS144 strand-bias catalog using ggplot2

Creates a 20-bar chart (10 pairs) from a 144-row DBS catalog, collapsing
by dinucleotide class and splitting by transcribed/untranscribed strand.
Uses 132 of 144 entries (12 self-complementary types are omitted).

## Usage

``` r
plot_DBS144(
  catalog,
  plot_title = NULL,
  ylabels = TRUE,
  ylim = NULL,
  base_size = 11,
  title_cex = 0.8,
  x_label_cex = 1,
  axis_title_cex = 1,
  axis_text_cex = 0.8
)
```

## Arguments

- catalog:

  Numeric vector, single-column data.frame, matrix, tibble, or
  data.table.

- plot_title:

  Character. Title displayed above the plot.

- ylabels:

  Logical, draw y-axis labels.

- ylim:

  Optional y-axis limits.

- base_size:

  Numeric. Base font size in points.

- title_cex:

  Numeric. Multiplier for the plot title size.

- x_label_cex:

  Numeric. Multiplier for x-axis labels.

- axis_title_cex:

  Numeric. Multiplier for the y-axis title size.

- axis_text_cex:

  Numeric. Multiplier for the y-axis tick label size.

## Value

A ggplot object.

## Examples

``` r
set.seed(1)
sig <- runif(144)
sig <- sig / sum(sig)
names(sig) <- catalog_row_order()$DBS144
plot_DBS144(sig, plot_title = "Example DBS144 signature")

```
