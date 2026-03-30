# Plot 89-channel indel profile

Creates a bar plot visualization of an 89-channel indel mutational
signature for a single sample. The plot shows mutation counts or
proportions across all 89 indel categories with color-coded category
blocks.

## Usage

``` r
plot_89(
  catalog,
  text_cex = 3,
  top_bar_text_cex = text_cex,
  title_text_cex = 1,
  plot_title = NULL,
  setyaxis = NULL,
  ylabel = NULL,
  base_size = 11,
  x_axis_tick_label_cex = 0.6,
  y_axis_tick_label_cex = 0.8,
  x_title_cex = 0.9,
  y_title_cex = 0.9,
  show_x_axis_text = TRUE,
  show_top_bar = TRUE,
  show_extra_top_bar = FALSE,
  plot_complex = FALSE,
  show_counts = NULL,
  count_label_cex = 1.03,
  text_size = NULL,
  top_bar_text_size = NULL,
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

  Numeric vector, single-column data.frame, matrix, tibble, or
  data.table.

- text_cex:

  Numeric. Size of text labels in the plot. Used as default for
  `top_bar_text_cex`.

- top_bar_text_cex:

  Numeric. Size of labels in the colored top bars (both the category bar
  and the extra summary bar).

- title_text_cex:

  Numeric. Size of the plot title text, relative to `base_size`.

- plot_title:

  Character. Title displayed above the plot.

- setyaxis:

  Numeric or NULL. If provided, sets a fixed y-axis maximum. If NULL,
  y-axis scales automatically to the data.

- ylabel:

  Character. Label for the y-axis.

- base_size:

  Base font size for ggplot2's `theme_classic()`.

- x_axis_tick_label_cex:

  Numeric. Size of x-axis tick labels, relative to `base_size`.

- y_axis_tick_label_cex:

  Numeric. Size of y-axis tick labels, relative to `base_size`.

- x_title_cex:

  Numeric. Size of x-axis title, relative to `base_size`.

- y_title_cex:

  Numeric. Size of y-axis title, relative to `base_size`.

- show_x_axis_text:

  Logical. If `TRUE`, display x-axis tick labels.

- show_top_bar:

  Logical. If `TRUE`, display the category bar above the plot (e.g.,
  "Del 1bp C", "Ins 1bp T").

- show_extra_top_bar:

  Logical. If `TRUE`, display the extra summary bar above the category
  bar (e.g., "Del", "Ins"). Only shown if `show_top_bar` is also `TRUE`.
  This is really for backward compatibility.

- plot_complex:

  Logical. If `TRUE`, include the Complex indel channel.

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

A ggplot2 object containing the 89-channel indel profile plot.

## Examples

``` r
set.seed(1)
sig <- runif(89)
sig <- sig / sum(sig)
plot_89(sig, plot_title = "Example ID89 signature")

```
