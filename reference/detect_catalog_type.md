# Detect catalog type from data and attributes

Determines whether the catalog represents counts, counts.signature,
density, or density.signature. Checks the `catalog.type` attribute
first; if absent, infers from data values and optional ylim.

## Usage

``` r
detect_catalog_type(values, catalog_type_attr = NULL, ylim = NULL)
```

## Arguments

- values:

  Numeric vector of catalog values.

- catalog_type_attr:

  The `catalog.type` attribute from the catalog, or NULL.

- ylim:

  Optional y-axis limits vector.

## Value

Character string: one of "counts", "counts.signature", "density", or
"density.signature".
