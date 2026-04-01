#' @rdname bar_chart_plots
#' @export
#'
#' @importFrom gridExtra grid.arrange
#' @importFrom Cairo CairoPDF
#' @importFrom grDevices dev.off
plot_SBS12_pdf <- function(
  catalog,
  filename,
  abundance = NULL,
  ylabels = TRUE,
  ylim = NULL,
  base_size = 11,
  plot_title_cex = 0.8,
  axis_text_x_cex = 1.0,
  axis_title_x_cex = 1.0,
  axis_title_y_cex = 1.0,
  axis_text_y_cex = 0.8
) {
  if (is.null(normalize_catalog(catalog[, 1, drop = FALSE], 192,
                                catalog_row_order()$SBS192, "SBS192")))
    stop("Invalid SBS192 catalog")

  n_samples <- ncol(catalog)
  plots_per_page <- 5

  Cairo::CairoPDF(file = filename, width = 12, height = 14)

  for (i in seq(1, n_samples, by = plots_per_page)) {
    end_idx <- min(i + plots_per_page - 1, n_samples)
    page_plots <- list()

    for (j in i:end_idx) {
      sample_catalog <- catalog[, j, drop = FALSE]
      p <- plot_SBS12(
        catalog = sample_catalog,
        plot_title = colnames(catalog)[j],
        abundance = abundance,
        ylabels = ylabels,
        ylim = ylim,
        base_size = base_size,
        plot_title_cex = plot_title_cex,
        axis_text_x_cex = axis_text_x_cex,
        axis_title_x_cex = axis_title_x_cex,
        axis_title_y_cex = axis_title_y_cex,
        axis_text_y_cex = axis_text_y_cex
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
