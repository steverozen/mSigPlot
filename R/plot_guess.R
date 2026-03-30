#' Plot mutational signature catalog with automatic channel detection
#'
#' Automatically selects the appropriate plotting function based on the number
#' of rows in the catalog.
#'
#' @param catalog Numeric vector, single-column data.frame, matrix, tibble,
#'   or data.table. The number of rows (or length) determines which plotting
#'   function is used:
#'   - 1536 rows: calls `plot_SBS1536()`
#'   - 476 rows: calls `plot_476()`
#'   - 192 rows: calls `plot_SBS192()`
#'   - 166 rows: calls `plot_ID166()`
#'   - 144 rows: calls `plot_DBS144()`
#'   - 136 rows: calls `plot_DBS136()`
#'   - 96 rows: calls `plot_SBS96()`
#'   - 89 rows: calls `plot_89()`
#'   - 83 rows: calls `plot_83()`
#'   - 78 rows: calls `plot_DBS78()`
#' @param ... Additional arguments passed to the underlying plotting function.
#'
#' @return A ggplot object from the appropriate plotting function.
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
#'   [plot_DBS136()], [plot_DBS144()], [plot_ID166()], [plot_476()], [plot_89()],
#'   [plot_83()]
#'
#' @export
plot_guess = function(catalog, ...) {
  if (is.numeric(catalog) && !is.matrix(catalog)) {
    n_rows = length(catalog)
  } else {
    n_rows = nrow(catalog)
  }
  if (n_rows == 1536) {
    plot_SBS1536(catalog, ...)
  } else if (n_rows == 476) {
    plot_476(catalog, ...)
  } else if (n_rows == 192) {
    plot_SBS192(catalog, ...)
  } else if (n_rows == 166) {
    plot_ID166(catalog, ...)
  } else if (n_rows == 144) {
    plot_DBS144(catalog, ...)
  } else if (n_rows == 136) {
    plot_DBS136(catalog, ...)
  } else if (n_rows == 96) {
    plot_SBS96(catalog, ...)
  } else if (n_rows == 89) {
    plot_89(catalog, ...)
  } else if (n_rows == 83) {
    plot_83(catalog, ...)
  } else if (n_rows == 78) {
    plot_DBS78(catalog, ...)
  } else {
    stop("Unexpected number of rows: ", n_rows)
  }
}
