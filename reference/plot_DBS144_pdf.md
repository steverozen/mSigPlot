# Export DBS144 strand bias profiles to PDF

Creates a multi-page PDF file containing DBS144 strand bias plots for
multiple samples. Plots are arranged with 5 samples per page.

## Usage

``` r
plot_DBS144_pdf(
  catalog,
  filename,
  ylabels = TRUE,
  ylim = NULL,
  base_size = 11,
  title_cex = 0.8,
  x_label_cex = 1,
  axis_title_cex = 1,
  axis_text_cex = 0.8
)
```

## Arguments

- catalog:

  A matrix or data frame with 144 rows and one column per sample.

- filename:

  Character. Path to the output PDF file.

- ylabels:

  Logical. Draw y-axis labels.

- ylim:

  Optional y-axis limits.

- base_size:

  Numeric. Base font size in points.

- title_cex:

  Numeric. Multiplier for the plot title size.

- x_label_cex:

  Numeric. Multiplier for x-axis labels.

- axis_title_cex:

  Numeric. Multiplier for the y-axis title size.

- axis_text_cex:

  Numeric. Multiplier for the y-axis tick label size.

## Value

Invisibly returns `NULL`.

## Examples

``` r
if (FALSE) { # \dontrun{
sig <- matrix(runif(144 * 3), nrow = 144)
rownames(sig) <- catalog_row_order()$DBS144
plot_DBS144_pdf(sig, filename = "dbs144.pdf")
} # }
```
