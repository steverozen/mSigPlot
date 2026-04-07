#' @rdname bar_plots
#' @export
plot_SBS192_pdf <- function(catalog, filename, ...) {
  if (is.null(normalize_catalog(catalog[, 1, drop = FALSE], 192,
                                catalog_row_order()$SBS192, "SBS192")))
    stop("Invalid SBS192 catalog")

  plot_catalog_pdf(catalog, filename, plot_SBS192, ...)
}
