#' @rdname bar_plots
#' @export
plot_ID89_pdf <- function(catalog, filename, ...) {
  if (is.null(normalize_catalog(catalog[, 1, drop = FALSE], 89,
                                catalog_row_order()$ID89, "ID89")))
    stop("Invalid ID89 catalog")

  plot_catalog_pdf(catalog, filename, plot_ID89,
                   width = 8.2677, height = 14.61613, ...)
}
