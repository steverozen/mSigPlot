# Plot an SBS1536 catalog as a 2x3 heatmap grid using ggplot2

Creates a 2x3 grid of 16x16 heatmaps (one per mutation type: C\>A, C\>G,
C\>T, T\>A, T\>C, T\>G) using `geom_tile()` with a white-to-forestgreen
color gradient. Axis labels show colored bases (A, C, G, T).

## Usage

``` r
plot_SBS1536(
  catalog,
  plot_title = NULL,
  base_size = 11,
  title_cex = 1.2,
  axis_label_cex = 0.8,
  panel_label_cex = 1
)
```

## Arguments

- catalog:

  Numeric vector, single-column data.frame, matrix, tibble, or
  data.table.

- plot_title:

  Character. Title displayed at the top center.

- base_size:

  Numeric. Base font size in points.

- title_cex:

  Numeric. Multiplier for the plot title size.

- axis_label_cex:

  Numeric. Multiplier for axis base labels.

- panel_label_cex:

  Numeric. Multiplier for panel mutation type labels.

## Value

A ggplot object (assembled via patchwork or gridExtra).
