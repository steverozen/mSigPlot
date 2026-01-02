#' Export 83-channel indel profiles to PDF
#'
#' Creates a multi-page PDF file containing 83-channel indel profile plots
#' for multiple samples. Plots are arranged with 5 samples per page. Uses
#' Cairo for high-quality PDF rendering.
#'
#' @param catalog A matrix or data frame with 83 rows (indel types) and
#'   one column per sample. Column names are used as plot titles.
#' @param filename Character. Path to the output PDF file.
#' @param text_size Numeric. Size of text labels in the plot. Default is 3.
#' @param grid Logical. Draw grid lines. Default is `TRUE`.
#' @param upper Logical. Draw category labels above bars. Default is `TRUE`.
#' @param xlabels Logical. Draw x-axis labels. Default is `TRUE`.
#' @param ylabels Logical. Draw y-axis labels. Default is `TRUE`.
#' @param ylim Optional y-axis limits. Default is `NULL`.
#' @param base_size Base font size for ggplot2's `theme_classic()`. Default is 11.
#'
#' @return Invisibly returns `NULL`. Called for side effect of creating PDF file.
#'
#' @export
#'
#' @importFrom gridExtra grid.arrange
#' @importFrom Cairo CairoPDF
#' @importFrom grDevices dev.off
plot_83_pdf <- function(
  catalog,
  filename,
  text_size = 3,
  grid = TRUE,
  upper = TRUE,
  xlabels = TRUE,
  ylabels = TRUE,
  ylim = NULL,
  base_size = 11
) {
  stopifnot(nrow(catalog) == 83)

  n_samples <- ncol(catalog)
  plots_per_page <- 5

  Cairo::CairoPDF(file = filename, width = 12, height = 14)

  for (i in seq(1, n_samples, by = plots_per_page)) {
    end_idx <- min(i + plots_per_page - 1, n_samples)
    page_plots <- list()

    for (j in i:end_idx) {
      sample_catalog <- catalog[, j, drop = FALSE]
      p <- plot_83(
        catalog = sample_catalog,
        text_size = text_size,
        plot_title = colnames(catalog)[j],
        grid = grid,
        upper = upper,
        xlabels = xlabels,
        ylabels = ylabels,
        ylim = ylim,
        base_size = base_size
      )
      page_plots[[length(page_plots) + 1]] <- p
    }

    # Pad with empty plots if needed
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
