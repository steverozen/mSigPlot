#' Export 476-channel indel profiles to PDF
#'
#' Creates a multi-page PDF file containing 476-channel indel profile plots
#' for multiple samples. Plots are arranged with 5 samples per page. Uses
#' Cairo for high-quality PDF rendering.
#'
#' @param catalog A matrix or data frame with 476 rows (indel types) and
#'   one column per sample. Column names are used as plot titles.
#' @param filename Character. Path to the output PDF file.
#' @param num_labels Integer. Number of top peaks to label per category block.
#' @param simplify_labels Logical. If TRUE, simplifies peak labels by removing
#'   the indel type prefix.
#' @param label_threshold_denominator Numeric. Peaks with values less than
#'   max/label_threshold_denominator are not labeled.
#' @param vline_labels Character vector. IndelType labels at which to draw
#'   vertical reference lines.
#' @param base_size Base font size for ggplot2's theme. All text sizes scale
#'   relative to this value.
#' @param block_text_size Numeric. Size of category block labels, as a fraction
#'   of `base_size`.
#' @param ggrepel_size Numeric. Size of ggrepel peak labels, as a fraction of
#'   `base_size`.
#' @param title_text_size Numeric. Relative size of the plot title text, passed to `rel()`.
#' @param x_axis_tick_label_size Numeric. Relative size of x-axis tick labels, passed to `rel()`.
#' @param y_axis_tick_label_size Numeric. Relative size of y-axis tick labels, passed to `rel()`.
#' @param x_title_size Numeric. Relative size of x-axis title, passed to `rel()`.
#' @param y_title_size Numeric. Relative size of y-axis title, passed to `rel()`.
#' @param plot_complex Logical. If TRUE, include the 5 Complex indel channels.
#' @param show_counts Logical or NULL. If `TRUE`, always display per-class
#'   mutation count labels. If `FALSE`, never display them. If `NULL`
#'   (the default), display them only when the catalog contains counts
#'   (sum > 1.1).
#' @param count_label_size Numeric. Size of per-class count labels, as a
#'   fraction of `base_size`.
#'
#' @return NULL. Called for side effect of creating a PDF file.
#'
#' @export
#'
#' @import Cairo
#' @importFrom grDevices dev.off cairo_pdf
plot_476_pdf <- function(
  catalog,
  filename,
  num_labels = 4,
  simplify_labels = FALSE,
  label_threshold_denominator = 7,
  vline_labels = c(),
  base_size = 11,
  block_text_size = 0.78,
  ggrepel_size = 0.52,
  title_text_size = 1.0,
  x_axis_tick_label_size = 0.8,
  y_axis_tick_label_size = 0.7,
  x_title_size = 0.7,
  y_title_size = 0.9,
  plot_complex = FALSE,
  show_counts = NULL,
  count_label_size = 0.52
) {
  plot_list <- lapply(1:ncol(catalog), function(i) {
    plot_476(
      catalog = catalog[, i],
      block_text_size = block_text_size,
      plot_title = colnames(catalog)[i],
      num_labels = num_labels,
      ggrepel_size = ggrepel_size,
      simplify_labels = simplify_labels,
      label_threshold_denominator = label_threshold_denominator,
      vline_labels = vline_labels,
      base_size = base_size,
      title_text_size = title_text_size,
      x_axis_tick_label_size = x_axis_tick_label_size,
      y_axis_tick_label_size = y_axis_tick_label_size,
      x_title_size = x_title_size,
      y_title_size = y_title_size,
      plot_complex = plot_complex,
      show_counts = show_counts,
      count_label_size = count_label_size
    )
  })
  plots_per_page <- 5

  # Total number of pages
  total_pages <- ceiling(length(plot_list) / plots_per_page)

  # Open a PDF device
  cairo_pdf(filename, width = 8.2677, height = 14.61613)

  # Loop through pages and save each group of plots to the same PDF
  for (page in 1:total_pages) {
    # Get the plots for the current page
    start_index <- (page - 1) * plots_per_page + 1
    end_index <- min(page * plots_per_page, length(plot_list))
    plots_on_page <- plot_list[start_index:end_index]

    # Arrange the plots in a grid (2 rows x 4 columns)
    do.call(gridExtra::grid.arrange, c(plots_on_page, nrow = 5, ncol = 1))
  }

  # Close the PDF device
  dev.off()
}
