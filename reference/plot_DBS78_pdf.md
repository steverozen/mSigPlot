# Export DBS78 profiles to PDF

Creates a multi-page PDF file containing DBS78 profile plots for
multiple samples. Plots are arranged with 5 samples per page.

## Usage

``` r
plot_DBS78_pdf(
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
  class_label_cex = 0.7,
  x_label_cex = 0.5,
  axis_title_cex = 1,
  axis_text_cex = 0.8,
  show_counts = NULL
)
```

## Arguments

- catalog:

  A matrix or data frame with 78 rows and one column per sample.

- filename:

  Character. Path to the output PDF file.

- grid:

  Logical. Draw grid lines.

- upper:

  Logical. Draw class labels above bars.

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

  Numeric. Multiplier for the per-class count labels.

- class_label_cex:

  Numeric. Multiplier for the major class labels.

- x_label_cex:

  Numeric. Multiplier for the x-axis labels.

- axis_title_cex:

  Numeric. Multiplier for the y-axis title size.

- axis_text_cex:

  Numeric. Multiplier for the y-axis tick label size.

- show_counts:

  Logical or NULL. Auto-detect if NULL.

## Value

Invisibly returns `NULL`.
