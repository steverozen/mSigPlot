#' @rdname bar_plots
#' @export
plot_ID166_pdf <- function(catalog, filename, ...) {
  if (is.null(normalize_catalog(catalog[, 1, drop = FALSE], 166,
                                catalog_row_order()$ID166, "ID166")))
    stop("Invalid ID166 catalog")

  plot_catalog_pdf(catalog, filename, plot_ID166, ...)
}
