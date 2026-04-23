# Convert stapled SBS96 row names to compact format

Converts names like `"A[C>A]T"` to `"ACTA"` (compact
`<5' base><ref><3' base><alt>` format).

## Usage

``` r
unstaple_SBS96_rownames(stapled_names)
```

## Arguments

- stapled_names:

  Character vector of stapled mutation type identifiers (e.g.
  `"A[C>A]T"`).

## Value

Character vector of compact 4-letter identifiers.
