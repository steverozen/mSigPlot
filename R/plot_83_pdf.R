#' @rdname legacy_bar_plots
#' @export
#'
#' @importFrom gridExtra grid.arrange
#' @importFrom Cairo CairoPDF
#' @importFrom grDevices dev.off
plot_83_pdf <- function(
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
  block_label_cex = 0.65,
  class_label_cex = 0.8,
  x_label_cex = 0.5,
  bottom_label_cex = 0.65,
  axis_title_cex = 1.0,
  axis_text_cex = 0.8,
  show_counts = NULL
) {
  if (is.null(normalize_catalog(catalog[, 1, drop = FALSE], 83,
                                catalog_row_order()$ID, "ID83")))
    stop("Invalid ID83 catalog")

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
        plot_title = colnames(catalog)[j],
        grid = grid,
        upper = upper,
        xlabels = xlabels,
        ylabels = ylabels,
        ylim = ylim,
        base_size = base_size,
        title_cex = title_cex,
        count_label_cex = count_label_cex,
        block_label_cex = block_label_cex,
        class_label_cex = class_label_cex,
        x_label_cex = x_label_cex,
        bottom_label_cex = bottom_label_cex,
        axis_title_cex = axis_title_cex,
        axis_text_cex = axis_text_cex,
        show_counts = show_counts
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
