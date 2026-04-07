#' @rdname bar_plots
#' @export
plot_SBS96_pdf <- function(catalog, filename, ...) {
  if (is.null(normalize_catalog(catalog[, 1, drop = FALSE], 96,
                                catalog_row_order()$SBS96, "SBS96")))
    stop("Invalid SBS96 catalog")

  plot_catalog_pdf(catalog, filename, plot_SBS96, ...)
}
