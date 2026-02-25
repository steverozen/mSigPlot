# Plot a DBS78 catalog using ggplot2

Creates a 78-bar chart showing doublet base substitution counts or
proportions, organized into 10 dinucleotide classes (AC, AT, CC, CG, CT,
GC, TA, TC, TG, TT).

## Usage

``` r
plot_DBS78(
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
  class_label_cex = 0.7,
  x_label_cex = 0.5,
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

  Logical, draw colored class rectangles and labels above bars.

- xlabels:

  Logical, draw x-axis dinucleotide labels.

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

- class_label_cex:

  Numeric. Multiplier for major class labels.

- x_label_cex:

  Numeric. Multiplier for x-axis labels.

- axis_title_cex:

  Numeric. Multiplier for the y-axis title size.

- axis_text_cex:

  Numeric. Multiplier for the y-axis tick label size.

- show_counts:

  Logical or NULL. Auto-detect if NULL.

## Value

A ggplot object.
