# Export mutational profiles to PDF with automatic type detection

Creates a multi-page PDF file containing mutational profile plots for
multiple samples. Automatically detects the catalog type based on the
number of rows. Plots are arranged with 5 samples per page (except for
heatmap types which use 1 per page). Uses Cairo for high-quality PDF
rendering.

## Usage

``` r
plot_guess_pdf(catalog, filename, ...)
```

## Arguments

- catalog:

  A matrix or data frame with rows matching one of the supported catalog
  types (96, 192, 1536, 78, 136, 144, 83, 89, 166, or 476 rows) and one
  column per sample. Column names are used as plot titles.

- filename:

  Character. Path to the output PDF file.

- ...:

  Additional arguments passed to
  [`plot_guess()`](https://steverozen.github.io/mSigPlot/reference/plot_guess.md)
  and the underlying plotting function.

## Value

NULL. Called for side effect of creating a PDF file.

## See also

[`plot_guess()`](https://steverozen.github.io/mSigPlot/reference/plot_guess.md),
[`plot_SBS96_pdf()`](https://steverozen.github.io/mSigPlot/reference/plot_SBS96_pdf.md),
[`plot_SBS192_pdf()`](https://steverozen.github.io/mSigPlot/reference/plot_SBS192_pdf.md),
[`plot_SBS1536_pdf()`](https://steverozen.github.io/mSigPlot/reference/plot_SBS1536_pdf.md),
[`plot_DBS78_pdf()`](https://steverozen.github.io/mSigPlot/reference/plot_DBS78_pdf.md),
[`plot_DBS136_pdf()`](https://steverozen.github.io/mSigPlot/reference/plot_DBS136_pdf.md),
[`plot_DBS144_pdf()`](https://steverozen.github.io/mSigPlot/reference/plot_DBS144_pdf.md),
[`plot_ID166_pdf()`](https://steverozen.github.io/mSigPlot/reference/plot_ID166_pdf.md),
[`plot_476_pdf()`](https://steverozen.github.io/mSigPlot/reference/plot_476_pdf.md),
[`plot_89_pdf()`](https://steverozen.github.io/mSigPlot/reference/plot_89_pdf.md)

## Examples

``` r
if (FALSE) { # \dontrun{
sig <- matrix(runif(96 * 3), nrow = 96)
rownames(sig) <- catalog_row_order()$SBS96
colnames(sig) <- paste0("Sig", 1:3)
plot_guess_pdf(sig, filename = "auto_detected.pdf")
} # }
```
