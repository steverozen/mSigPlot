# Return the 476-channel indel type classification table

Constructs a data frame mapping 476 indel channels to their type, class,
and figure label. The result is cached after the first call.

## Usage

``` r
type_476_indel_type()
```

## Value

A 476-row data frame with columns: IndelType, Indel, Figlabel.
