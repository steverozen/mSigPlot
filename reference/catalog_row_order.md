# Return catalog row orders for all supported catalog types

Returns a named list containing the canonical row ordering for each
catalog type. These are used for validation and ordering of mutation
catalogs.

## Usage

``` r
catalog_row_order()
```

## Value

A named list with elements: SBS96, SBS192, SBS1536, DBS78, DBS136,
DBS144, ID (83-category COSMIC indels), ID166, ID89, ID476.

## Examples

``` r
cro <- catalog_row_order()
head(cro$SBS96)
#> [1] "ACAA" "ACCA" "ACGA" "ACTA" "CCAA" "CCCA"
length(cro$DBS78)
#> [1] 78
```
