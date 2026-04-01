#' @rdname heatmap_plots
#' @export
#'
#' @importFrom Cairo CairoPDF
#' @importFrom grDevices dev.off
plot_DBS136_pdf <- function(
  catalog,
  filename,
  base_size = 11,
  plot_title_cex = 1.2,
  axis_text_cex = 0.8,
  strip_text_cex = 1.0
) {
  if (is.null(normalize_catalog(catalog[, 1, drop = FALSE], 136,
                                catalog_row_order()$DBS136, "DBS136")))
    stop("Invalid DBS136 catalog")

  n_samples <- ncol(catalog)

  Cairo::CairoPDF(file = filename, width = 8.27, height = 11.69)

  for (i in 1:n_samples) {
    sample_catalog <- catalog[, i, drop = FALSE]
    plot_DBS136(
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
