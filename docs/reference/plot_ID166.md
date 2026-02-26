# Plot an ID166 genic/intergenic indel catalog using ggplot2

Creates a paired barplot showing 83 indel categories split into genic
(black) and intergenic (grey) regions. Rows 1-83 are genic, rows 84-166
are intergenic.

## Usage

``` r
plot_ID166(
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

  Logical, draw background color wash and grid lines.

- upper:

  Logical, draw category labels above bars.

- xlabels:

  Logical, draw x-axis labels.

- ylabels:

  Logical, draw y-axis labels.

- ylim:

  Optional y-axis limits.

- base_size:

  Numeric. Base font size in points.

- title_cex:

  Numeric. Multiplier for the plot title size.

- count_label_cex:

  Numeric. Multiplier for per-class count labels.

- block_label_cex:

  Numeric. Multiplier for upper category block labels.

- class_label_cex:

  Numeric. Multiplier for major class labels.

- x_label_cex:

  Numeric. Multiplier for x-axis labels.

- bottom_label_cex:

  Numeric. Multiplier for bottom category labels.

- axis_title_cex:

  Numeric. Multiplier for the y-axis title size.

- axis_text_cex:

  Numeric. Multiplier for the y-axis tick label size.

- show_counts:

  Logical or NULL. Auto-detect if NULL.

## Value

A ggplot object.
