#' Export DBS136 heatmaps to PDF
#'
#' Creates a multi-page PDF file containing DBS136 10-panel heatmap plots,
#' one sample per page.
#'
#' @param catalog A matrix or data frame with 136 rows and one column per sample.
#' @param filename Character. Path to the output PDF file.
#' @param base_size Numeric. Base font size in points.
#' @param title_cex Numeric. Multiplier for the plot title size.
#' @param axis_label_cex Numeric. Multiplier for axis base labels.
#' @param panel_label_cex Numeric. Multiplier for panel mutation type labels.
#'
#' @return Invisibly returns `NULL`.
#'
#' @examples
#' \dontrun{
#' sig <- matrix(runif(136 * 2), nrow = 136)
#' rownames(sig) <- catalog_row_order()$DBS136
#' plot_DBS136_pdf(sig, filename = "dbs136.pdf")
#' }
#'
#' @export
#'
#' @importFrom Cairo CairoPDF
#' @importFrom grDevices dev.off
plot_DBS136_pdf <- function(
  catalog,
  filename,
  base_size = 11,
  title_cex = 1.2,
  axis_label_cex = 0.8,
  panel_label_cex = 1.0
) {
  stopifnot(nrow(catalog) == 136)

  n_samples <- ncol(catalog)

  Cairo::CairoPDF(file = filename, width = 8.27, height = 11.69)

  for (i in 1:n_samples) {
    sample_catalog <- catalog[, i, drop = FALSE]
    plot_DBS136(
      catalog = sample_catalog,
      plot_title = colnames(catalog)[i],
      base_size = base_size,
      title_cex = title_cex,
      axis_label_cex = axis_label_cex,
      panel_label_cex = panel_label_cex
    )
  }

  grDevices::dev.off()
  invisible(NULL)
}
