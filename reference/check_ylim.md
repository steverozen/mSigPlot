# Validate a user-supplied `ylim` argument

Matches ggplot2's scale-limits contract: `ylim` must be `NULL` or a
length-2 numeric vector `c(ymin, ymax)`. A scalar (e.g. `100`) is
rejected, the same way `ggplot2::scale_y_continuous(limits = 100)`
rejects it.

## Usage

``` r
check_ylim(ylim, arg_name = "ylim")
```

## Arguments

- ylim:

  The value supplied by the caller.

- arg_name:

  Character. Argument name used in the error message; defaults to
  `"ylim"`.

## Value

`ylim`, invisibly, if valid. Errors otherwise.
