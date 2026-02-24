#' Export DBS144 strand bias profiles to PDF
#'
#' Creates a multi-page PDF file containing DBS144 strand bias plots for
#' multiple samples. Plots are arranged with 5 samples per page.
#'
#' @param catalog A matrix or data frame with 144 rows and one column per sample.
#' @param filename Character. Path to the output PDF file.
#' @param ylabels Logical. Draw y-axis labels.
#' @param ylim Optional y-axis limits.
#' @param base_size Numeric. Base font size in points.
#' @param title_cex Numeric. Multiplier for the plot title size.
#' @param x_label_cex Numeric. Multiplier for x-axis labels.
#' @param axis_title_cex Numeric. Multiplier for the y-axis title size.
#' @param axis_text_cex Numeric. Multiplier for the y-axis tick label size.
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
#'
#' @importFrom gridExtra grid.arrange
#' @importFrom Cairo CairoPDF
#' @importFrom grDevices dev.off
plot_DBS144_pdf <- function(
  catalog,
  filename,
  ylabels = TRUE,
  ylim = NULL,
  base_size = 11,
  title_cex = 0.8,
  x_label_cex = 1.0,
  axis_title_cex = 1.0,
  axis_text_cex = 0.8
) {
  stopifnot(nrow(catalog) == 144)

  n_samples <- ncol(catalog)
  plots_per_page <- 5

  Cairo::CairoPDF(file = filename, width = 12, height = 14)

  for (i in seq(1, n_samples, by = plots_per_page)) {
    end_idx <- min(i + plots_per_page - 1, n_samples)
    page_plots <- list()

    for (j in i:end_idx) {
      sample_catalog <- catalog[, j, drop = FALSE]
      p <- plot_DBS144(
        catalog = sample_catalog,
        plot_title = colnames(catalog)[j],
        ylabels = ylabels,
        ylim = ylim,
        base_size = base_size,
        title_cex = title_cex,
        x_label_cex = x_label_cex,
        axis_title_cex = axis_title_cex,
        axis_text_cex = axis_text_cex
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
