# Plot 476-channel indel profile

Creates a bar plot visualization of a 476-channel indel mutational
signature for a single sample. The plot shows mutation counts or
proportions across all 476 indel categories with color-coded category
blocks and flanking base annotations. Includes smart peak labeling using
ggrepel.

## Usage

``` r
plot_476(
  catalog,
  block_text_cex = 0.78,
  plot_title = NULL,
  num_labels = 3,
  ggrepel_cex = 0.52,
  label_threshold_denominator = 7,
  vline_labels = c(),
  simplify_labels = TRUE,
  base_size = 11,
  title_text_cex = 1,
  x_axis_tick_label_cex = 0.8,
  y_axis_tick_label_cex = 0.7,
  x_title_cex = 0.7,
  y_title_cex = 0.9,
  plot_complex = FALSE,
  show_counts = NULL,
  count_label_cex = 0.52,
  block_text_size = NULL,
  ggrepel_size = NULL,
  title_text_size = NULL,
  x_axis_tick_label_size = NULL,
  y_axis_tick_label_size = NULL,
  x_title_size = NULL,
  y_title_size = NULL,
  count_label_size = NULL,
  text_size = NULL,
  label_size = NULL
)
```

## Arguments

- catalog:

  Numeric vector, single-column data.frame, matrix, tibble, or
  data.table.

- block_text_cex:

  Numeric. Size of category block labels (e.g. "Del 1bp C"), as a
  fraction of `base_size`.

- plot_title:

  Character. Title displayed above the plot.

- num_labels:

  Integer. Number of top peaks to label per category block. Set to 0 or
  NULL to disable labels.

- ggrepel_cex:

  Numeric. Size of ggrepel peak labels, as a fraction of `base_size`.

- label_threshold_denominator:

  Numeric. Peaks with values less than max/label_threshold_denominator
  are not labeled.

- vline_labels:

  Character vector. IndelType labels at which to draw vertical reference
  lines. For example, `c("A[Del(C):R1]A", "G[Del(C):R1]A")`.

- simplify_labels:

  Logical. If TRUE, simplifies peak labels by removing the indel type
  prefix.

- base_size:

  Base font size for ggplot2's theme. All text sizes scale relative to
  this value.

- title_text_cex:

  Numeric. Size of the plot title text, relative to `base_size`.

- x_axis_tick_label_cex:

  Numeric. Size of x-axis tick labels, relative to `base_size`.

- y_axis_tick_label_cex:

  Numeric. Size of y-axis tick labels, relative to `base_size`.

- x_title_cex:

  Numeric. Size of x-axis title, relative to `base_size`.

- y_title_cex:

  Numeric. Size of y-axis title, relative to `base_size`.

- plot_complex:

  Logical. If TRUE, include the 5 Complex indel channels.

- show_counts:

  Logical or NULL. If `TRUE`, always display per-class mutation count
  labels. If `FALSE`, never display them. If `NULL` (the default),
  display them only when the catalog contains counts (sum \> 1.1).

- count_label_cex:

  Numeric. Size of per-class count labels, as a fraction of `base_size`.

- block_text_size:

  Deprecated. Use `block_text_cex` instead.

- ggrepel_size:

  Deprecated. Use `ggrepel_cex` instead.

- title_text_size:

  Deprecated. Use `title_text_cex` instead.

- x_axis_tick_label_size:

  Deprecated. Use `x_axis_tick_label_cex` instead.

- y_axis_tick_label_size:

  Deprecated. Use `y_axis_tick_label_cex` instead.

- x_title_size:

  Deprecated. Use `x_title_cex` instead.

- y_title_size:

  Deprecated. Use `y_title_cex` instead.

- count_label_size:

  Deprecated. Use `count_label_cex` instead.

- text_size:

  Deprecated. Use `block_text_cex` instead.

- label_size:

  Deprecated. Use `ggrepel_cex` instead.

## Value

A ggplot2 object containing the 476-channel indel profile plot.
