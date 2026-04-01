#' Export ID166 genic/intergenic profiles to PDF
#'
#' Creates a multi-page PDF file containing ID166 paired barplots for
#' multiple samples. Plots are arranged with 5 samples per page.
#'
#' @param catalog A matrix or data frame with 166 rows and one column per sample.
#' @param filename Character. Path to the output PDF file.
#' @param grid Logical. Draw background color wash and grid lines.
#' @param upper Logical. Draw category labels above bars.
#' @param xlabels Logical. Draw x-axis labels.
#' @param ylabels Logical. Draw y-axis labels.
#' @param ylim Optional y-axis limits.
#' @param base_size Numeric. Base font size in points.
#' @param plot_title_cex Numeric. Multiplier for the plot title size.
#' @param count_label_cex Numeric. Multiplier for per-class count labels.
#' @param block_label_cex Numeric. Multiplier for category block labels.
#' @param class_label_cex Numeric. Multiplier for major class labels.
#' @param axis_text_x_cex Numeric. Multiplier for x-axis labels.
#' @param bottom_label_cex Numeric. Multiplier for bottom labels.
#' @param axis_title_x_cex Numeric. Multiplier for the x-axis title size. Currently has no effect in this function.
#' @param axis_title_y_cex Numeric. Multiplier for the y-axis title size.
#' @param axis_text_y_cex Numeric. Multiplier for the y-axis tick label size.
#' @param show_counts Logical or NULL. Auto-detect if NULL.
#'
#' @return Invisibly returns `NULL`.
#'
#' @examples
#' \dontrun{
#' sig <- matrix(runif(166 * 3), nrow = 166)
#' rownames(sig) <- catalog_row_order()$ID166
#' plot_ID166_pdf(sig, filename = "id166.pdf")
#' }
#'
#' @export
#'
#' @importFrom gridExtra grid.arrange
#' @importFrom Cairo CairoPDF
#' @importFrom grDevices dev.off
plot_ID166_pdf <- function(
  catalog,
  filename,
  grid = TRUE,
  upper = TRUE,
  xlabels = TRUE,
  ylabels = TRUE,
  ylim = NULL,
  base_size = 11,
  plot_title_cex = 0.8,
  count_label_cex = 0.6,
  block_label_cex = 0.65,
  class_label_cex = 0.8,
  axis_text_x_cex = 0.5,
  bottom_label_cex = 0.65,
  axis_title_x_cex = 1.0,
  axis_title_y_cex = 1.0,
  axis_text_y_cex = 0.8,
  show_counts = NULL
) {
  stopifnot(nrow(catalog) == 166)

  n_samples <- ncol(catalog)
  plots_per_page <- 5

  Cairo::CairoPDF(file = filename, width = 12, height = 14)

  for (i in seq(1, n_samples, by = plots_per_page)) {
    end_idx <- min(i + plots_per_page - 1, n_samples)
    page_plots <- list()

    for (j in i:end_idx) {
      sample_catalog <- catalog[, j, drop = FALSE]
      p <- plot_ID166(
        catalog = sample_catalog,
        plot_title = colnames(catalog)[j],
        grid = grid,
        upper = upper,
        xlabels = xlabels,
        ylabels = ylabels,
        ylim = ylim,
        base_size = base_size,
        plot_title_cex = plot_title_cex,
        count_label_cex = count_label_cex,
        block_label_cex = block_label_cex,
        class_label_cex = class_label_cex,
        axis_text_x_cex = axis_text_x_cex,
        bottom_label_cex = bottom_label_cex,
        axis_title_x_cex = axis_title_x_cex,
        axis_title_y_cex = axis_title_y_cex,
        axis_text_y_cex = axis_text_y_cex,
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
