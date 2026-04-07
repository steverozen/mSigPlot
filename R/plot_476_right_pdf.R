#' @rdname legacy_bar_plots
#' @export
#'
#' @import Cairo
#' @importFrom grDevices dev.off cairo_pdf
plot_476_right_pdf <- function(
  catalog,
  filename,
  block_text_cex = 1.0,
  num_peak_labels = 3,
  peak_label_cex = 0.7,
  label_threshold_denominator = 7,
  vline_labels = c(),
  simplify_labels = TRUE,
  base_size = 11,
  title_text_cex = 1.0,
  x_axis_tick_label_cex = 0.7,
  y_axis_tick_label_cex = 0.7,
  x_title_cex = 0.9,
  y_title_cex = 0.9,
  plot_complex = FALSE,
  show_counts = NULL,
  count_label_cex = 1.03,
  stop_at_9 = TRUE
) {
  plot_list <- lapply(1:ncol(catalog), function(i) {
    plot_476_right(
      catalog = catalog[, i],
      block_text_cex = block_text_cex,
      plot_title = colnames(catalog)[i],
      num_peak_labels = num_peak_labels,
      peak_label_cex = peak_label_cex,
      label_threshold_denominator = label_threshold_denominator,
      vline_labels = vline_labels,
      simplify_labels = simplify_labels,
      base_size = base_size,
      title_text_cex = title_text_cex,
      x_axis_tick_label_cex = x_axis_tick_label_cex,
      y_axis_tick_label_cex = y_axis_tick_label_cex,
      x_title_cex = x_title_cex,
      y_title_cex = y_title_cex,
      plot_complex = plot_complex,
      show_counts = show_counts,
      count_label_cex = count_label_cex,
      stop_at_9 = stop_at_9
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
