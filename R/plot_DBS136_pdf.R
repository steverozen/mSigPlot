#' @rdname heatmap_plots
#' @export
plot_DBS136_pdf <- function(catalog, filename, ...) {
  if (is.null(normalize_catalog(catalog[, 1, drop = FALSE], 136,
                                catalog_row_order()$DBS136, "DBS136")))
    stop("Invalid DBS136 catalog")

  plot_catalog_pdf(catalog, filename, plot_DBS136,
                   plots_per_page = 1, width = 8.27, height = 11.69, ...)
}
