# Detect y-axis type from data and attributes

Determines the y-axis type for a catalog. Checks the `y_axis_type_attr`
attribute first; if absent, infers from data values and optional ylim.

## Usage

``` r
detect_y_axis_type(values, y_axis_type_attr = NULL, ylim = NULL)
```

## Arguments

- values:

  Numeric vector of catalog values.

- y_axis_type_attr:

  The `y_axis_type_attr` attribute from the catalog, or NULL.

- ylim:

  Optional y-axis limits vector.

## Value

Character string: one of `"counts"`, `"proportion"`,
`"muts_per_million"`, or `"density_proportion"`.
