#' plot_guess, plot_guess_pdf
#'
#' Automatically select the appropriate plotting function based on the number
#' of rows in the catalog.
#'
#' `plot_guess()` plots a single sample. `plot_guess_pdf()` creates a
#' multi-page PDF file containing plots for multiple samples, arranged with
#' 5 samples per page (except heatmap types: 1 per page). Uses Cairo for
#' high-quality PDF rendering.
#'
#' @param catalog Numeric vector, single-column data.frame, matrix, tibble,
#'   or data.table. The number of rows (or length) determines which plotting
#'   function is used:
#'   - 1536 rows: calls `plot_SBS1536()`
#'   - 476 rows: calls `plot_ID476()`
#'   - 192 rows: calls `plot_SBS192()`
#'   - 166 rows: calls `plot_ID166()`
#'   - 144 rows: calls `plot_DBS144()`
#'   - 136 rows: calls `plot_DBS136()`
#'   - 96 rows: calls `plot_SBS96()`
#'   - 89 rows: calls `plot_ID89()`
#'   - 83 rows: calls `plot_ID83()`
#'   - 78 rows: calls `plot_DBS78()`
#'
#'   The column names of `catalog` are used as plot titles.
#' @param filename Character. Path to the output PDF file (`plot_guess_pdf` only).
#' @param ... Additional arguments passed to the underlying plotting function.
#'
#' @return `plot_guess()` returns a ggplot object. `plot_guess_pdf()` returns
#'   NULL invisibly (called for side effect of creating a PDF file).
#'
#' @examples
#' # Auto-detect a 96-channel catalog and dispatch to plot_SBS96
#' set.seed(1)
#' sig <- runif(96)
#' sig <- sig / sum(sig)
#' names(sig) <- catalog_row_order()$SBS96
#' plot_guess(sig, plot_title = "Auto-detected SBS96")
#'
#' @seealso [plot_SBS96()], [plot_SBS192()], [plot_SBS1536()], [plot_DBS78()],
#'   [plot_DBS136()], [plot_DBS144()], [plot_ID166()], [plot_ID476()], [plot_ID89()],
#'   [plot_ID83()]
#'
#' @name plot_guess
NULL

# Dispatch table mapping row count to plot function
.plot_dispatch <- list(
  "1536" = "plot_SBS1536",
  "476"  = "plot_ID476",
  "288"  = "plot_SBS288",
  "192"  = "plot_SBS192",
  "166"  = "plot_ID166",
  "144"  = "plot_DBS144",
  "136"  = "plot_DBS136",
  "96"   = "plot_SBS96",
  "89"   = "plot_ID89",
  "83"   = "plot_ID83",
  "78"   = "plot_DBS78"
)

#' @rdname plot_guess
#' @export
plot_guess <- function(catalog, ...) {
  n_rows <- if (is.numeric(catalog) && !is.matrix(catalog)) {
    length(catalog)
  } else {
    nrow(catalog)
  }
  fn_name <- .plot_dispatch[[as.character(n_rows)]]
  if (is.null(fn_name)) stop("Unexpected number of rows: ", n_rows)
  match.fun(fn_name)(catalog, ...)
}
