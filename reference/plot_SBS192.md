# Plot a full SBS192 catalog using ggplot2

Creates a 192-bar chart showing single base substitutions on transcribed
and untranscribed strands. Bars are paired (transcribed/untranscribed)
side-by-side, with 6 colored class backgrounds.

## Usage

``` r
plot_SBS192(
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
  class_label_cex = 0.9,
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

  Logical, draw x-axis context labels.

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
