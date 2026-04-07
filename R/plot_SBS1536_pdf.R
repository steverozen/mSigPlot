#' @rdname heatmap_plots
#' @export
plot_SBS1536_pdf <- function(catalog, filename, ...) {
  if (is.null(normalize_catalog(catalog[, 1, drop = FALSE], 1536,
                                catalog_row_order()$SBS1536, "SBS1536")))
    stop("Invalid SBS1536 catalog")

  plot_catalog_pdf(catalog, filename, plot_SBS1536,
                   plots_per_page = 1, width = 11.69, height = 9.27, ...)
}
