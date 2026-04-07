#' @rdname bar_plots
#' @export
plot_ID83_pdf <- function(catalog, filename, ...) {
  if (is.null(normalize_catalog(catalog[, 1, drop = FALSE], 83,
                                catalog_row_order()$ID, "ID83")))
    stop("Invalid ID83 catalog")

  plot_catalog_pdf(catalog, filename, plot_ID83, ...)
}
