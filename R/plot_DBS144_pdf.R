#' @rdname bar_plots
#' @export
plot_DBS144_pdf <- function(catalog, filename, ...) {
  if (is.null(normalize_catalog(catalog[, 1, drop = FALSE], 144,
                                catalog_row_order()$DBS144, "DBS144")))
    stop("Invalid DBS144 catalog")

  plot_catalog_pdf(catalog, filename, plot_DBS144, ...)
}
