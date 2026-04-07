#' @rdname bar_plots
#' @export
plot_DBS78_pdf <- function(catalog, filename, ...) {
  if (is.null(normalize_catalog(catalog[, 1, drop = FALSE], 78,
                                catalog_row_order()$DBS78, "DBS78")))
    stop("Invalid DBS78 catalog")

  plot_catalog_pdf(catalog, filename, plot_DBS78, ...)
}
