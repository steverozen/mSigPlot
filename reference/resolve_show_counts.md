# Resolve show_counts parameter

If `show_counts` is NULL (auto), returns TRUE when catalog_type is
"counts", FALSE otherwise. If explicitly set, returns the value
unchanged.

## Usage

``` r
resolve_show_counts(show_counts, catalog_type)
```

## Arguments

- show_counts:

  NULL, TRUE, or FALSE.

- catalog_type:

  Character catalog type string.

## Value

Logical.
