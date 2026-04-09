#' @rdname bar_plots
#' @export
plot_SBS288_pdf <- function(catalog, filename, ...) {
  if (is.null(normalize_catalog(catalog[, 1, drop = FALSE], 288,
                                catalog_row_order()$SBS288, "SBS288")))
    stop("Invalid SBS288 catalog")

  plot_catalog_pdf(catalog, filename, plot_SBS288,
                   plots_per_page = 1, width = 12, height = 14, ...)
}
