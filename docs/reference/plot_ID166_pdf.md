# Export ID166 genic/intergenic profiles to PDF

Creates a multi-page PDF file containing ID166 paired barplots for
multiple samples. Plots are arranged with 5 samples per page.

## Usage

``` r
plot_ID166_pdf(
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

  A matrix or data frame with 166 rows and one column per sample.

- filename:

  Character. Path to the output PDF file.

- grid:

  Logical. Draw background color wash and grid lines.

- upper:

  Logical. Draw category labels above bars.

- xlabels:

  Logical. Draw x-axis labels.

- ylabels:

  Logical. Draw y-axis labels.

- ylim:

  Optional y-axis limits.

- base_size:

  Numeric. Base font size in points.

- title_cex:

  Numeric. Multiplier for the plot title size.

- count_label_cex:

  Numeric. Multiplier for per-class count labels.

- block_label_cex:

  Numeric. Multiplier for category block labels.

- class_label_cex:

  Numeric. Multiplier for major class labels.

- x_label_cex:

  Numeric. Multiplier for x-axis labels.

- bottom_label_cex:

  Numeric. Multiplier for bottom labels.

- axis_title_cex:

  Numeric. Multiplier for the y-axis title size.

- axis_text_cex:

  Numeric. Multiplier for the y-axis tick label size.

- show_counts:

  Logical or NULL. Auto-detect if NULL.

## Value

Invisibly returns `NULL`.
