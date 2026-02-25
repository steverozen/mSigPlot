# Return reorder vector for DBS144 plotting

Returns the 132-element character vector used to reorder DBS144 catalog
rows for paired strand display. Only 132 of 144 entries are included;
the 12 self-complementary DBS types that lack strand distinction are
omitted.

## Usage

``` r
reorder_DBS144_for_plotting()
```

## Value

A 132-element character vector of DBS144 row names in plotting order.
