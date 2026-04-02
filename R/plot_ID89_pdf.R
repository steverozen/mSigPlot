#' @rdname bar_plots
#' @export
#'
#' @importFrom gridExtra grid.arrange
#' @importFrom Cairo CairoPDF
#' @importFrom grDevices dev.off
plot_ID89_pdf <- function(
  catalog,
  filename,
  upper = FALSE,
  xlabels = TRUE,
  ylim = NULL,
  base_size = 11,
  plot_title_cex = 1.0,
  count_label_cex = 1.03,
  block_label_cex = 3,
  class_label_cex = block_label_cex,
  axis_text_x_cex = 0.7,
  axis_title_x_cex = 0.9,
  axis_title_y_cex = 0.9,
  axis_text_y_cex = 0.7,
  show_counts = NULL,
  ylabel = NULL,
  plot_complex = FALSE
) {
  if (is.null(normalize_catalog(catalog[, 1, drop = FALSE], 89,
                                catalog_row_order()$ID89, "ID89")))
    stop("Invalid ID89 catalog")

  n_samples <- ncol(catalog)
  plots_per_page <- 5

  Cairo::CairoPDF(file = filename, width = 8.2677, height = 14.61613)

  for (i in seq(1, n_samples, by = plots_per_page)) {
    end_idx <- min(i + plots_per_page - 1, n_samples)
    page_plots <- list()

    for (j in i:end_idx) {
      sample_catalog <- catalog[, j, drop = FALSE]
      p <- plot_ID89(
        catalog = sample_catalog,
        plot_title = colnames(catalog)[j],
        upper = upper,
        xlabels = xlabels,
        ylim = ylim,
        base_size = base_size,
        plot_title_cex = plot_title_cex,
        count_label_cex = count_label_cex,
        block_label_cex = block_label_cex,
        class_label_cex = class_label_cex,
        axis_text_x_cex = axis_text_x_cex,
        axis_title_x_cex = axis_title_x_cex,
        axis_title_y_cex = axis_title_y_cex,
        axis_text_y_cex = axis_text_y_cex,
        show_counts = show_counts,
        ylabel = ylabel,
        plot_complex = plot_complex
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
