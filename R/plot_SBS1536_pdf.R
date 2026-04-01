#' @rdname heatmap_plots
#' @export
#'
#' @importFrom Cairo CairoPDF
#' @importFrom grDevices dev.off
plot_SBS1536_pdf <- function(
  catalog,
  filename,
  base_size = 11,
  plot_title_cex = 1.2,
  axis_text_cex = 0.8,
  strip_text_cex = 1.0
) {
  if (is.null(normalize_catalog(catalog[, 1, drop = FALSE], 1536,
                                catalog_row_order()$SBS1536, "SBS1536")))
    stop("Invalid SBS1536 catalog")

  n_samples <- ncol(catalog)

  Cairo::CairoPDF(file = filename, width = 11.69, height = 9.27)

  for (i in 1:n_samples) {
    sample_catalog <- catalog[, i, drop = FALSE]
    plot_SBS1536(
      catalog = sample_catalog,
      plot_title = colnames(catalog)[i],
      base_size = base_size,
      plot_title_cex = plot_title_cex,
      axis_text_cex = axis_text_cex,
      strip_text_cex = strip_text_cex
    )
  }

  grDevices::dev.off()
  invisible(NULL)
}
