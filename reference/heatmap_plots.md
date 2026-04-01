# Heatmap plots for mutational signatures

Plot functions for SBS and DBS mutational signature catalogs as
heatmaps. All functions return ggplot2 objects.

## Usage

``` r
plot_DBS136(
  catalog,
  plot_title = NULL,
  base_size = 11,
  plot_title_cex = 1.2,
  axis_text_cex = 0.8,
  strip_text_cex = 1
)

plot_DBS136_pdf(
  catalog,
  filename,
  base_size = 11,
  plot_title_cex = 1.2,
  axis_text_cex = 0.8,
  strip_text_cex = 1
)

plot_SBS1536(
  catalog,
  plot_title = NULL,
  base_size = 11,
  plot_title_cex = 1.2,
  axis_text_cex = 0.8,
  strip_text_cex = 1
)

plot_SBS1536_pdf(
  catalog,
  filename,
  base_size = 11,
  plot_title_cex = 1.2,
  axis_text_cex = 0.8,
  strip_text_cex = 1
)
```

## Arguments

- catalog:

  Numeric vector, single-column data.frame, matrix, tibble, or
  data.table.

- plot_title:

  Character. Title displayed above the plot.

- base_size:

  Numeric. Base font size in points.

- plot_title_cex:

  Numeric. Multiplier for the plot title size.

- axis_text_cex:

  Numeric. Multiplier for axis label size.

- strip_text_cex:

  Numeric. Multiplier for panel/facet label size.

- filename:

  Character. Path to the output PDF file (\\pdf functions only).

## Value

A ggplot2 object (plot functions) or NULL (\\pdf functions, called for
side effect of creating a PDF file).

## Details

Functions in this family:

- `plot_SBS1536`: SBS pentanucleotide context (1536 channels)

- `plot_DBS136`: DBS heatmap (136 channels, 10 4x4 panels)

Each has a corresponding `_pdf()` variant for multi-sample PDF export.

## Examples

``` r
set.seed(1)
sig <- runif(136)
sig <- sig / sum(sig)
names(sig) <- catalog_row_order()$DBS136
plot_DBS136(sig, plot_title = "Example DBS136")


set.seed(1)
sig <- runif(1536)
sig <- sig / sum(sig)
names(sig) <- catalog_row_order()$SBS1536
plot_SBS1536(sig, plot_title = "Example SBS1536")

```
