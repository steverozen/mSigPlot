# Export 476-channel indel profiles to PDF

Creates a multi-page PDF file containing 476-channel indel profile plots
for multiple samples. Plots are arranged with 5 samples per page. Uses
Cairo for high-quality PDF rendering.

## Usage

``` r
plot_476_pdf(
  catalog,
  filename,
  num_labels = 4,
  simplify_labels = FALSE,
  label_threshold_denominator = 7,
  vline_labels = c(),
  base_size = 11,
  block_text_cex = 0.78,
  ggrepel_cex = 0.52,
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
  count_label_size = NULL
)
```

## Arguments

- catalog:

  A matrix or data frame with 476 rows (indel types) and one column per
  sample. Column names are used as plot titles.

- filename:

  Character. Path to the output PDF file.

- num_labels:

  Integer. Number of top peaks to label per category block.

- simplify_labels:

  Logical. If TRUE, simplifies peak labels by removing the indel type
  prefix.

- label_threshold_denominator:

  Numeric. Peaks with values less than max/label_threshold_denominator
  are not labeled.

- vline_labels:

  Character vector. IndelType labels at which to draw vertical reference
  lines.

- base_size:

  Base font size for ggplot2's theme. All text sizes scale relative to
  this value.

- block_text_cex:

  Numeric. Size of category block labels, as a fraction of `base_size`.

- ggrepel_cex:

  Numeric. Size of ggrepel peak labels, as a fraction of `base_size`.

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

## Value

NULL. Called for side effect of creating a PDF file.
