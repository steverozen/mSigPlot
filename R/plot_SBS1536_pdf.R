#' Export SBS1536 heatmaps to PDF
#'
#' Creates a multi-page PDF file containing SBS1536 heatmap plots, one
#' sample per page.
#'
#' @param catalog A matrix or data frame with 1536 rows and one column per sample.
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
#' sig <- matrix(runif(1536 * 2), nrow = 1536)
#' rownames(sig) <- catalog_row_order()$SBS1536
#' plot_SBS1536_pdf(sig, filename = "sbs1536.pdf")
#' }
#'
#' @export
#'
#' @importFrom Cairo CairoPDF
#' @importFrom grDevices dev.off
plot_SBS1536_pdf <- function(
  catalog,
  filename,
  base_size = 11,
  title_cex = 1.2,
  axis_label_cex = 0.8,
  panel_label_cex = 1.0
) {
  stopifnot(nrow(catalog) == 1536)

  n_samples <- ncol(catalog)

  Cairo::CairoPDF(file = filename, width = 11.69, height = 9.27)

  for (i in 1:n_samples) {
    sample_catalog <- catalog[, i, drop = FALSE]
    plot_SBS1536(
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
