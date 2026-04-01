#' Export right portion of 476-channel indel profiles to PDF
#'
#' Creates a multi-page PDF file containing the right 134 channels
#' (positions 343-476) of 476-channel indel profile plots for multiple
#' samples. Plots are arranged with 5 samples per page. Uses Cairo for
#' high-quality PDF rendering.
#'
#' @param catalog A matrix or data frame with 476 rows (indel types) and
#'   one column per sample. Column names are used as plot titles.
#' @param filename Character. Path to the output PDF file.
#' @param block_text_cex Numeric. Size of category block labels, as a fraction
#'   of `base_size`.
#' @param num_labels Integer. Number of top peaks to label per category block.
#' @param ggrepel_cex Numeric. Size of ggrepel peak labels, as a fraction of
#'   `base_size`.
#' @param label_threshold_denominator Numeric. Peaks with values less than
#'   max/label_threshold_denominator are not labeled.
#' @param vline_labels Character vector. IndelType labels at which to draw
#'   vertical reference lines.
#' @param simplify_labels Logical. If TRUE, simplifies peak labels by removing
#'   the indel type prefix.
#' @param base_size Base font size for ggplot2's theme. All text sizes scale
#'   relative to this value.
#' @param title_text_cex Numeric. Size of the plot title text, relative to `base_size`.
#' @param x_axis_tick_label_cex Numeric. Size of x-axis tick labels, relative to `base_size`.
#' @param y_axis_tick_label_cex Numeric. Size of y-axis tick labels, relative to `base_size`.
#' @param x_title_cex Numeric. Size of x-axis title, relative to `base_size`.
#' @param y_title_cex Numeric. Size of y-axis title, relative to `base_size`.
#' @param plot_complex Logical. If TRUE, include the 5 Complex indel channels.
#' @param show_x_labels Logical. If `TRUE`, display the Figlabel for each
#'   channel as a rotated x-axis tick label.
#' @param show_counts Logical or NULL. If `TRUE`, always display per-class
#'   mutation count labels. If `FALSE`, never display them. If `NULL`
#'   (the default), display them only when the catalog contains counts
#'   (sum > 1.1).
#' @param count_label_cex Numeric. Size of per-class count labels, as a
#'   fraction of `base_size`.
#'
#' @return NULL. Called for side effect of creating a PDF file.
#'
#' @examples
#' \dontrun{
#' sig <- matrix(runif(476 * 3), nrow = 476)
#' rownames(sig) <- catalog_row_order()$ID476
#' colnames(sig) <- paste0("Sig", 1:3)
#' plot_476_right_pdf(sig, filename = "id476_right.pdf")
#' }
#'
#' @export
#'
#' @import Cairo
#' @importFrom grDevices dev.off cairo_pdf
plot_476_right_pdf <- function(
  catalog,
  filename,
  block_text_cex = 1.0,
  num_labels = 3,
  ggrepel_cex = 0.7,
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
  show_x_labels = FALSE,
  show_counts = NULL,
  count_label_cex = 1.03
) {
  plot_list <- lapply(1:ncol(catalog), function(i) {
    plot_476_right(
      catalog = catalog[, i],
      block_text_cex = block_text_cex,
      plot_title = colnames(catalog)[i],
      num_labels = num_labels,
      ggrepel_cex = ggrepel_cex,
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
      show_x_labels = show_x_labels,
      show_counts = show_counts,
      count_label_cex = count_label_cex
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
