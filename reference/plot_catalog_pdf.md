# Generic multi-sample PDF export for bar plots

Creates a multi-page PDF with plots arranged in a grid. Each page
contains up to `plots_per_page` plots in a single column.

## Usage

``` r
plot_catalog_pdf(
  catalog,
  filename,
  plot_fn,
  plots_per_page = 5,
  width = 12,
  height = 14,
  ...
)
```

## Arguments

- catalog:

  Multi-column matrix or data.frame of catalogs.

- filename:

  Character path to output PDF file.

- plot_fn:

  The single-sample plotting function to call for each column.

- plots_per_page:

  Integer number of plots per page.

- width:

  Numeric PDF page width in inches.

- height:

  Numeric PDF page height in inches.

- ...:

  Additional arguments passed to `plot_fn`.

## Value

NULL invisibly.
