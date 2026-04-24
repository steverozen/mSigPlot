# Derive ID89 figure-axis labels from canonical IndelType strings

Rewrites the canonical ID89 channel names from
`catalog_row_order()$ID89` into the compact tick-label style used on the
x-axis of
[`plot_ID89()`](https://steverozen.github.io/mSigPlot/reference/bar_plots.md).

## Usage

``` r
id89_figlabels(indel_types)
```

## Arguments

- indel_types:

  Character vector of ID89 IndelType strings, typically
  `catalog_row_order()$ID89`.

## Value

A character vector of the same length as `indel_types`, giving the
figure-label form of each channel name.

## Details

Single-base channels (matching `Del(C)`, `Del(T)`, `Ins(C)`, `Ins(T)`):
the `Del(..)`/`Ins(..)` wrapper and the `:R` prefix are dropped, and any
open-ended repeat range `(N,)` is rewritten as `(N+)`.

Multi-base channels (longer Del/Ins/MH rows): leading `Del(`/`Ins(` is
rewritten to `L(`, a leading open-ended length `(N,)` is space-padded to
`(N, )`, a trailing `R(N,)` becomes `R(N+)`, a trailing `M(N,)` is
space-padded to `M(N, )`, and all colons are removed.

The `"Complex"` channel is returned unchanged.

## Examples

``` r
id89_figlabels(c(
  "[Del(C):R1]A",
  "Del(C):R(6,9)",
  "Del(2,4):R1",
  "Del(2,):U(1,2):R(5,)",
  "Del(6,):M(4,)",
  "Complex"
))
#> [1] "[C1]A"            "C(6+)"            "L(2,4)R1"         "L(2+)U(1,2)R(5+)"
#> [5] "L(6+)M(4+)"       "Complex"         
```
