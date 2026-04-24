# Add a plot title, inside the panel (annotate) or above it (ggtitle)

If `title_outside_plot` is FALSE, places `plot_title` inside the plot
area via `annotate("text", ...)` at `y = ymax * y_frac` — the
`plot_ID83` style. If TRUE, places it above the plot via
`ggtitle() + theme(plot.title = ...)`. Title size is always
`plot_title_cex * base_size` (points), normalized to mm for `annotate`.

## Usage

``` r
add_plot_title(
  p,
  plot_title,
  title_outside_plot,
  plot_title_cex,
  base_size,
  ymax,
  x = 1,
  y_frac = 7.4/8,
  hjust = 0
)
```

## Arguments

- p:

  A ggplot object.

- plot_title:

  Character title (NULL or empty = no title added).

- title_outside_plot:

  Logical. FALSE = inside, TRUE = above.

- plot_title_cex:

  Numeric size multiplier.

- base_size:

  Numeric base font size in points.

- ymax:

  Numeric. Top of the plotting region used to position the inside title.
  Ignored when `title_outside_plot = TRUE`.

- x:

  Numeric x-coordinate for the inside title.

- y_frac:

  Numeric. Inside title sits at `ymax * y_frac`.

- hjust:

  Horizontal justification of the inside title.

## Value

The ggplot object with the title added.
