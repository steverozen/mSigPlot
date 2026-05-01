#' @rdname plot_guess
#'
#' @examples
#' \donttest{
#' sig <- matrix(runif(96 * 3), nrow = 96)
#' rownames(sig) <- catalog_row_order()$SBS96
#' colnames(sig) <- paste0("Sig", 1:3)
#' plot_guess_pdf(sig, filename = tempfile(fileext = ".pdf"))
#' }
#'
#' @export
plot_guess_pdf <- function(catalog, filename, ...) {
  n_rows <- nrow(catalog)
  if (is.null(.plot_dispatch[[as.character(n_rows)]]))
    stop("Unsupported catalog size: ", n_rows, " rows")

  plot_catalog_pdf(catalog, filename, plot_guess,
                   width = 8.2677, height = 14.61613, ...)
}
