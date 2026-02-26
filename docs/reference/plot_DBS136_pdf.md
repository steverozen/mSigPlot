# Export DBS136 heatmaps to PDF

Creates a multi-page PDF file containing DBS136 10-panel heatmap plots,
one sample per page.

## Usage

``` r
plot_DBS136_pdf(
  catalog,
  filename,
  base_size = 11,
  title_cex = 1.2,
  axis_label_cex = 0.8,
  panel_label_cex = 1
)
```

## Arguments

- catalog:

  A matrix or data frame with 136 rows and one column per sample.

- filename:

  Character. Path to the output PDF file.

- base_size:

  Numeric. Base font size in points.

- title_cex:

  Numeric. Multiplier for the plot title size.

- axis_label_cex:

  Numeric. Multiplier for axis base labels.

- panel_label_cex:

  Numeric. Multiplier for panel mutation type labels.

## Value

Invisibly returns `NULL`.
