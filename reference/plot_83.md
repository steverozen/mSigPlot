# Plot 83-type indel catalog using ggplot2 and return the ggplot object

Plot 83-type indel catalog using ggplot2 and return the ggplot object

## Usage

``` r
plot_83(
  catalog,
  plot_title = NULL,
  grid = TRUE,
  upper = TRUE,
  xlabels = TRUE,
  ylabels = TRUE,
  ylim = NULL,
  base_size = 11,
  title_cex = 0.8,
  count_label_cex = 0.9,
  block_label_cex = 0.65,
  class_label_cex = 0.8,
  x_label_cex = 0.5,
  bottom_label_cex = 0.65,
  axis_title_cex = 1,
  axis_text_cex = 0.8,
  show_counts = NULL
)
```

## Arguments

- catalog:

  Numeric vector, single-column data.frame, matrix, tibble, or
  data.table.

- plot_title:

  Character. Title displayed above the plot.

- grid:

  Logical, draw grid lines.

- upper:

  Logical, draw category labels above bars.

- xlabels:

  Logical, draw x-axis labels.

- ylabels:

  Logical, draw y-axis labels.

- ylim:

  Optional y-axis limits.

- base_size:

  Numeric. Base font size in points for ggplot2's theme. All text
  element sizes scale relative to this value.

- title_cex:

  Numeric. Multiplier for the plot title (sample name) size.

- count_label_cex:

  Numeric. Multiplier for the per-class mutation count labels.

- block_label_cex:

  Numeric. Multiplier for the upper colored category block labels (C, T,
  2, 3, ...).

- class_label_cex:

  Numeric. Multiplier for the major class labels (e.g. "1bp deletion").

- x_label_cex:

  Numeric. Multiplier for the x-axis channel labels (repeat/homopolymer
  lengths).

- bottom_label_cex:

  Numeric. Multiplier for the bottom category description labels (e.g.
  "Homopolymer length").

- axis_title_cex:

  Numeric. Multiplier for the y-axis title size.

- axis_text_cex:

  Numeric. Multiplier for the y-axis tick label size.

- show_counts:

  Logical or NULL. If `TRUE`, always display per-class mutation count
  labels. If `FALSE`, never display them. If `NULL` (the default),
  display them only when the catalog contains counts (sum \> 1.1).

## Value

A ggplot object.

## Examples

``` r
set.seed(1)
sig <- runif(83)
sig <- sig / sum(sig)
names(sig) <- catalog_row_order()$ID
plot_83(sig, plot_title = "Example ID83 signature")

```
