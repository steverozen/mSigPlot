#' @rdname bar_chart_plots
#'
#' @examples
#' set.seed(1)
#' sig <- runif(89)
#' sig <- sig / sum(sig)
#' names(sig) <- catalog_row_order()$ID89
#' plot_ID89(sig, plot_title = "Example ID89")
#'
#' @export
plot_ID89 <- function(
  catalog,
  plot_title = NULL,
  upper = TRUE,
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
  show_extra_top_bar = upper,
  plot_complex = FALSE
) {
  plot_89(
    catalog = catalog,
    plot_title = plot_title,
    show_top_bar = upper,
    show_x_axis_text = xlabels,
    setyaxis = ylim,
    base_size = base_size,
    title_text_cex = plot_title_cex,
    count_label_cex = count_label_cex,
    text_cex = block_label_cex,
    top_bar_text_cex = class_label_cex,
    x_axis_tick_label_cex = axis_text_x_cex,
    y_axis_tick_label_cex = axis_text_y_cex,
    x_title_cex = axis_title_x_cex,
    y_title_cex = axis_title_y_cex,
    show_counts = show_counts,
    ylabel = ylabel,
    show_extra_top_bar = show_extra_top_bar,
    plot_complex = plot_complex
  )
}

#' @rdname bar_chart_plots
#' @export
#'
#' @import Cairo
#' @importFrom grDevices dev.off cairo_pdf
plot_ID89_pdf <- function(
  catalog,
  filename,
  upper = TRUE,
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
  show_extra_top_bar = upper,
  plot_complex = FALSE
) {
  plot_list <- lapply(1:ncol(catalog), function(i) {
    plot_ID89(
      catalog = catalog[, i],
      plot_title = colnames(catalog)[i],
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
      show_extra_top_bar = show_extra_top_bar,
      plot_complex = plot_complex
    )
  })
  plots_per_page <- 5
  total_pages <- ceiling(length(plot_list) / plots_per_page)
  cairo_pdf(filename, width = 8.2677, height = 14.61613)
  for (page in 1:total_pages) {
    start_index <- (page - 1) * plots_per_page + 1
    end_index <- min(page * plots_per_page, length(plot_list))
    plots_on_page <- plot_list[start_index:end_index]
    do.call(gridExtra::grid.arrange, c(plots_on_page, nrow = 5, ncol = 1))
  }
  dev.off()
}
