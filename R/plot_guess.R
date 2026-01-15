#' Plot mutational signature catalog with automatic channel detection
#'
#' Automatically selects the appropriate plotting function based on the number
#' of rows in the catalog: 476-channel, 89-channel, or 83-channel.
#'
#' @param catalog A data frame or matrix containing mutational signature data.
#'   The number of rows determines which plotting function is used:
#'   - 476 rows: calls `plot_476()`
#'   - 89 rows: calls `plot_89()`
#'   - 83 rows: calls `plot_83()`
#' @param ... Additional arguments passed to the underlying plotting function.
#'
#' @return A ggplot object from the appropriate plotting function.
#'
#' @examples
#' \dontrun{
#' # Automatically detect and plot a 476-channel catalog
#' plot_guess(my_catalog_476)
#'
#' # Automatically detect and plot an 89-channel catalog
#' plot_guess(my_catalog_89, title = "Sample Signature")
#' }
#'
#' @seealso [plot_476()], [plot_89()], [plot_83()]
#' @export
plot_guess = function(catalog, ...) {
  n_rows = nrow(catalog)
  if (n_rows == 476) {
    plot_476(catalog, ...)
  } else if (n_rows == 89) {
    # For 89-type, prefer 476-type plotting if we later support conversion
    # For now, use plot_89
    plot_89(catalog, ...)
  } else if (n_rows == 83) {
    plot_83(catalog, ...)
  } else {
    stop("Unexpected number of rows: ", n_rows)
  }
}
