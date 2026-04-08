# Add peak labels with arrows to a ggplot

Adds
[`ggrepel::geom_text_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)
labels with closed arrowhead segments pointing from labels to the top
bars in a bar plot. Used to identify dominant channels.

## Usage

``` r
add_peak_labels(
  plot,
  df,
  x_col,
  y_col,
  label_col,
  num_peak_labels = 0,
  peak_label_cex = 0.7,
  base_size = 11,
  arrow_length = 0.01,
  label_threshold_denominator = 7
)
```

## Arguments

- plot:

  A ggplot object to add labels to.

- df:

  Data frame containing the plot data.

- x_col:

  Character name of the x-position column in `df`.

- y_col:

  Character name of the y-value column in `df`.

- label_col:

  Character name of the label column in `df`.

- num_peak_labels:

  Integer number of top bars to label (0 = none).

- peak_label_cex:

  Numeric multiplier for label text size.

- base_size:

  Numeric base font size.

- arrow_length:

  Numeric arrow length in npc units.

- label_threshold_denominator:

  Numeric; only bars with value \> max / denominator are candidates for
  labeling.

## Value

The ggplot object with labels added (or unchanged if num_peak_labels ==
0).
