#' @rdname bar_chart_plots
#'
#' @examples
#' set.seed(1)
#' sig <- runif(83)
#' sig <- sig / sum(sig)
#' names(sig) <- catalog_row_order()$ID
#' plot_ID83(sig, plot_title = "Example ID83")
#'
#' @export
plot_ID83 <- function(
  catalog,
  plot_title = NULL,
  grid = TRUE,
  upper = TRUE,
  xlabels = TRUE,
  ylabels = TRUE,
  ylim = NULL,
  base_size = 11,
  plot_title_cex = 0.8,
  count_label_cex = 0.9,
  block_label_cex = 0.65,
  class_label_cex = 0.8,
  axis_text_x_cex = 0.5,
  bottom_label_cex = 0.65,
  axis_title_x_cex = 1.0,
  axis_title_y_cex = 1.0,
  axis_text_y_cex = 0.8,
  show_counts = NULL
) {
  plot_83(
    catalog = catalog,
    plot_title = plot_title,
    grid = grid,
    upper = upper,
    xlabels = xlabels,
    ylabels = ylabels,
    ylim = ylim,
    base_size = base_size,
    title_cex = plot_title_cex,
    count_label_cex = count_label_cex,
    block_label_cex = block_label_cex,
    class_label_cex = class_label_cex,
    x_label_cex = axis_text_x_cex,
    bottom_label_cex = bottom_label_cex,
    axis_title_cex = axis_title_y_cex,
    axis_text_cex = axis_text_y_cex,
    show_counts = show_counts
  )
}

#' @rdname bar_chart_plots
#' @export
#'
#' @import Cairo
#' @importFrom grDevices dev.off cairo_pdf
plot_ID83_pdf <- function(
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
  plot_list <- lapply(1:ncol(catalog), function(i) {
    plot_ID83(
      catalog = catalog[, i, drop = FALSE],
      plot_title = colnames(catalog)[i],
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
  })
  plots_per_page <- 5
  total_pages <- ceiling(length(plot_list) / plots_per_page)
  Cairo::CairoPDF(file = filename, width = 12, height = 14)
  for (page in 1:total_pages) {
    start_index <- (page - 1) * plots_per_page + 1
    end_index <- min(page * plots_per_page, length(plot_list))
    plots_on_page <- plot_list[start_index:end_index]
    do.call(gridExtra::grid.arrange, c(plots_on_page, nrow = 5, ncol = 1))
  }
  dev.off()
}
