#' @rdname bar_plots
#' @export
plot_ID476_right_pdf <- function(catalog, filename, ...) {
  if (is.null(normalize_catalog(catalog[, 1, drop = FALSE], 476,
                                catalog_row_order()$ID476, "ID476")))
    stop("Invalid ID476 catalog")

  plot_catalog_pdf(catalog, filename, plot_ID476_right, ...)
}
