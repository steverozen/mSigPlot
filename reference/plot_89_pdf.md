# Export 89-channel indel profiles to PDF

Creates a multi-page PDF file containing 89-channel indel profile plots
for multiple samples. Plots are arranged with 5 samples per page.

## Usage

``` r
plot_89_pdf(
  catalog,
  text_cex = 3,
  top_bar_text_cex = text_cex,
  title_text_cex = 1,
  filename,
  show_x_axis_text = TRUE,
  show_top_bar = TRUE,
  show_counts = NULL,
  count_label_cex = 1.03,
  text_size = NULL,
  top_bar_text_size = NULL,
  title_text_size = NULL,
  count_label_size = NULL
)
```

## Arguments

- catalog:

  A matrix or data frame with 89 rows (indel types) and one column per
  sample. Column names are used as plot titles.

- text_cex:

  Numeric. Size of text labels in the plot.

- top_bar_text_cex:

  Numeric. Size of the labels in the colored top bar.

- title_text_cex:

  Numeric. Size of the plot title text, relative to `base_size`.

- filename:

  Character. Path to the output PDF file.

- show_x_axis_text:

  Logical. If `TRUE`, display x-axis tick labels.

- show_top_bar:

  Logical. If `TRUE`, display the category bar above the plot.

- show_counts:

  Logical or NULL. If `TRUE`, always display per-class mutation count
  labels. If `FALSE`, never display them. If `NULL` (the default),
  display them only when the catalog contains counts (sum \> 1.1).

- count_label_cex:

  Numeric. Size of per-class count labels, as a fraction of `base_size`.

- text_size:

  Deprecated. Use `text_cex` instead.

- top_bar_text_size:

  Deprecated. Use `top_bar_text_cex` instead.

- title_text_size:

  Deprecated. Use `title_text_cex` instead.

- count_label_size:

  Deprecated. Use `count_label_cex` instead.

## Value

NULL. Called for side effect of creating a PDF file.

## Examples

``` r
if (FALSE) { # \dontrun{
sig <- matrix(runif(89 * 3), nrow = 89)
plot_89_pdf(sig, filename = "id89.pdf")
} # }
```
