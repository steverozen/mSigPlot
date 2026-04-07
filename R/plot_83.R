#' @rdname legacy_bar_plots
#' @examples
#' set.seed(1)
#' sig <- runif(83)
#' sig <- sig / sum(sig)
#' names(sig) <- catalog_row_order()$ID
#' plot_83(sig, plot_title = "Example ID83 signature")
#'
#' @export
plot_83 <- function(
  catalog,
  plot_title = NULL,
  grid = TRUE,
  upper = TRUE,
  xlabels = TRUE,
  ylabels = TRUE,
  ylim = NULL,
  base_size = 11,
  title_cex = 0.8,
  count_label_cex = 0.9,
  block_label_cex = 0.65,
  class_label_cex = 0.8,
  x_label_cex = 0.5,
  bottom_label_cex = 0.65,
  axis_title_cex = 1.0,
  axis_text_cex = 0.8,
  show_counts = NULL,
  num_peak_labels = 0,
  peak_label_cex = 0.7
) {
  plot_ID83(
    catalog = catalog,
    plot_title = plot_title,
    grid = grid,
    upper = upper,
    show_axis_text_x = xlabels,
    show_axis_text_y = ylabels,
    show_axis_title_y = ylabels,
    ylim = ylim,
    base_size = base_size,
    plot_title_cex = title_cex,
    count_label_cex = count_label_cex,
    block_label_cex = block_label_cex,
    class_label_cex = class_label_cex,
    axis_text_x_cex = x_label_cex,
    bottom_label_cex = bottom_label_cex,
    axis_title_y_cex = axis_title_cex,
    axis_text_y_cex = axis_text_cex,
    show_counts = show_counts,
    num_peak_labels = num_peak_labels,
    peak_label_cex = peak_label_cex
  )
}
