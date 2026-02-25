# Export 83-channel indel profiles to PDF

Creates a multi-page PDF file containing 83-channel indel profile plots
for multiple samples. Plots are arranged with 5 samples per page. Uses
Cairo for high-quality PDF rendering.

## Usage

``` r
plot_83_pdf(
  catalog,
  filename,
  grid = TRUE,
  upper = TRUE,
  xlabels = TRUE,
  ylabels = TRUE,
  ylim = NULL,
  base_size = 11,
  title_cex = 0.8,
  count_label_cex = 0.6,
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

  A matrix or data frame with 83 rows (indel types) and one column per
  sample. Column names are used as plot titles.

- filename:

  Character. Path to the output PDF file.

- grid:

  Logical. Draw grid lines.

- upper:

  Logical. Draw category labels above bars.

- xlabels:

  Logical. Draw x-axis labels.

- ylabels:

  Logical. Draw y-axis labels.

- ylim:

  Optional y-axis limits.

- base_size:

  Numeric. Base font size in points. All text scales relative to this
  value.

- title_cex:

  Numeric. Multiplier for the plot title size.

- count_label_cex:

  Numeric. Multiplier for the per-class count labels.

- block_label_cex:

  Numeric. Multiplier for the upper colored category block labels.

- class_label_cex:

  Numeric. Multiplier for the major class labels.

- x_label_cex:

  Numeric. Multiplier for the x-axis channel labels.

- bottom_label_cex:

  Numeric. Multiplier for the bottom category description labels.

- axis_title_cex:

  Numeric. Multiplier for the y-axis title size.

- axis_text_cex:

  Numeric. Multiplier for the y-axis tick label size.

- show_counts:

  Logical or NULL. If `TRUE`, always display per-class mutation count
  labels. If `FALSE`, never display them. If `NULL` (the default),
  display them only when the catalog contains counts (sum \> 1.1).

## Value

Invisibly returns `NULL`. Called for side effect of creating PDF file.
