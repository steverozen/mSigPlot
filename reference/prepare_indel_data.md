# Prepare indel data for plotting

Merges a catalog vector with an indel type table, producing a
long-format data frame ready for ggplot. Used by `plot_ID476`,
`plot_ID476_right`, and `plot_ID89`.

## Usage

``` r
prepare_indel_data(catalog, type_table)
```

## Arguments

- catalog:

  Named numeric vector of indel channel values.

- type_table:

  Data frame with at least an `IndelType` column and classification
  columns (e.g. `Indel`, `Figlabel`).

## Value

Data frame with columns from `type_table` plus `Sample` and `freq`.
