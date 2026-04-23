# Normalize catalog input to a 1-column data.frame

Internal helper that coerces a catalog from any supported input type
(numeric vector, matrix, data.table, tibble, data.frame) to a 1-column
data.frame with validated row names. Stapled SBS row names (e.g.
`A[C>A]A` for SBS96, `T:A[C>A]A` for SBS288) are automatically converted
to compact format before validation.

## Usage

``` r
normalize_catalog(
  catalog,
  expected_nrow,
  canonical_order = NULL,
  type_label = ""
)
```

## Arguments

- catalog:

  Input catalog in any supported format.

- expected_nrow:

  Integer. Expected number of rows/elements.

- canonical_order:

  Character vector of canonical row names, or NULL to skip row name
  validation.

- type_label:

  Character label for error/warning messages (e.g. "SBS96").

## Value

A 1-column data.frame, or NULL if row names do not match canonical
order.
