#' Export DBS78 profiles to PDF
#'
#' Creates a multi-page PDF file containing DBS78 profile plots for multiple
#' samples. Plots are arranged with 5 samples per page.
#'
#' @param catalog A matrix or data frame with 78 rows and one column per sample.
#' @param filename Character. Path to the output PDF file.
#' @param grid Logical. Draw grid lines.
#' @param upper Logical. Draw class labels above bars.
#' @param xlabels Logical. Draw x-axis labels.
#' @param ylabels Logical. Draw y-axis labels.
#' @param ylim Optional y-axis limits.
#' @param base_size Numeric. Base font size in points.
#' @param title_cex Numeric. Multiplier for the plot title size.
#' @param count_label_cex Numeric. Multiplier for the per-class count labels.
#' @param class_label_cex Numeric. Multiplier for the major class labels.
#' @param x_label_cex Numeric. Multiplier for the x-axis labels.
#' @param axis_title_cex Numeric. Multiplier for the y-axis title size.
#' @param axis_text_cex Numeric. Multiplier for the y-axis tick label size.
#' @param show_counts Logical or NULL. Auto-detect if NULL.
#'
#' @return Invisibly returns `NULL`.
#'
#' @examples
#' \dontrun{
#' sig <- matrix(runif(78 * 3), nrow = 78)
#' rownames(sig) <- catalog_row_order()$DBS78
#' plot_DBS78_pdf(sig, filename = "dbs78.pdf")
#' }
#'
#' @export
#'
#' @importFrom gridExtra grid.arrange
#' @importFrom Cairo CairoPDF
#' @importFrom grDevices dev.off
plot_DBS78_pdf <- function(
  catalog,
  filename,
  grid = TRUE,
  upper = TRUE,
  xlabels = TRUE,
  ylabels = TRUE,
  ylim = NULL,
  base_size = 11,
  title_cex = 0.8,
  count_label_cex = 0.6,
  class_label_cex = 0.7,
  x_label_cex = 0.5,
  axis_title_cex = 1.0,
  axis_text_cex = 0.8,
  show_counts = NULL
) {
  stopifnot(nrow(catalog) == 78)

  n_samples <- ncol(catalog)
  plots_per_page <- 5

  Cairo::CairoPDF(file = filename, width = 12, height = 14)

  for (i in seq(1, n_samples, by = plots_per_page)) {
    end_idx <- min(i + plots_per_page - 1, n_samples)
    page_plots <- list()

    for (j in i:end_idx) {
      sample_catalog <- catalog[, j, drop = FALSE]
      p <- plot_DBS78(
        catalog = sample_catalog,
        plot_title = colnames(catalog)[j],
        grid = grid,
        upper = upper,
        xlabels = xlabels,
        ylabels = ylabels,
        ylim = ylim,
        base_size = base_size,
        title_cex = title_cex,
        count_label_cex = count_label_cex,
        class_label_cex = class_label_cex,
        x_label_cex = x_label_cex,
        axis_title_cex = axis_title_cex,
        axis_text_cex = axis_text_cex,
        show_counts = show_counts
      )
      page_plots[[length(page_plots) + 1]] <- p
    }

    while (length(page_plots) < plots_per_page) {
      page_plots[[length(page_plots) + 1]] <- ggplot2::ggplot() +
        ggplot2::theme_void()
    }

    gridExtra::grid.arrange(
      grobs = page_plots,
      ncol = 1,
      nrow = plots_per_page
    )
  }

  grDevices::dev.off()
  invisible(NULL)
}
