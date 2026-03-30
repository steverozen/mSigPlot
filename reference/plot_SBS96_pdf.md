# Export SBS96 profiles to PDF

Creates a multi-page PDF file containing SBS96 profile plots for
multiple samples. Plots are arranged with 5 samples per page.

## Usage

``` r
plot_SBS96_pdf(
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
  class_label_cex = 1.1,
  x_label_cex = 0.7,
  axis_title_cex = 1,
  axis_text_cex = 0.8,
  show_counts = NULL
)
```

## Arguments

- catalog:

  A matrix or data frame with 96 rows and one column per sample.

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

  Numeric. Multiplier for the x-axis base labels.

- axis_title_cex:

  Numeric. Multiplier for the y-axis title size.

- axis_text_cex:

  Numeric. Multiplier for the y-axis tick label size.

- show_counts:

  Logical or NULL. Auto-detect if NULL.

## Value

Invisibly returns `NULL`. Called for side effect of creating PDF file.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a multi-column catalog and export to PDF
sig <- matrix(runif(96 * 3), nrow = 96)
rownames(sig) <- catalog_row_order()$SBS96
colnames(sig) <- paste0("Sample", 1:3)
plot_SBS96_pdf(sig, filename = "sbs96_signatures.pdf")
} # }
```
