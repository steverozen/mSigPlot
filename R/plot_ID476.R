#' Plot 476-channel indel profile (ggplot2-consistent interface)
#'
#' Wrapper around [plot_476()] using parameter names consistent with
#' ggplot2 theme element naming conventions.
#'
#' @param catalog Numeric vector, single-column data.frame, matrix, tibble,
#'   or data.table with 476 rows.
#' @param plot_title Character. Title displayed above the plot.
#' @param base_size Base font size for ggplot2's theme.
#' @param plot_title_cex Numeric. Size of the plot title text, relative to
#'   `base_size`. Maps to `title_text_cex` in [plot_476()].
#' @param count_label_cex Numeric. Size of per-class count labels, as a
#'   fraction of `base_size`.
#' @param class_label_cex Numeric. Size of category block labels
#'   (e.g. "Del 1bp C", "Ins 1bp T"). Maps to `block_text_cex` in
#'   [plot_476()].
#' @param axis_text_x_cex Numeric. Size of x-axis tick labels. Maps to
#'   `x_axis_tick_label_cex` in [plot_476()].
#' @param axis_title_x_cex Numeric. Size of x-axis title, relative to
#'   `base_size`. Maps to `x_title_cex` in [plot_476()].
#' @param axis_title_y_cex Numeric. Size of y-axis title, relative to
#'   `base_size`. Maps to `y_title_cex` in [plot_476()].
#' @param axis_text_y_cex Numeric. Size of y-axis tick labels, relative to
#'   `base_size`. Maps to `y_axis_tick_label_cex` in [plot_476()].
#' @param show_counts Logical or NULL. If `TRUE`, always display per-class
#'   mutation count labels. If `FALSE`, never display them. If `NULL`
#'   (the default), display them only when the catalog contains counts
#'   (sum > 1.1).
#' @param num_labels Integer. Number of top peaks to label per category block.
#' @param ggrepel_cex Numeric. Size of ggrepel peak labels, as a fraction
#'   of `base_size`.
#' @param label_threshold_denominator Numeric. Peaks with values less than
#'   max/label_threshold_denominator are not labeled.
#' @param vline_labels Character vector. IndelType labels at which to draw
#'   vertical reference lines.
#' @param simplify_labels Logical. If TRUE, simplifies peak labels by removing
#'   the indel type prefix.
#' @param plot_complex Logical. If `TRUE`, include the 5 Complex indel channels.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' set.seed(1)
#' sig <- runif(476)
#' sig <- sig / sum(sig)
#' names(sig) <- catalog_row_order()$ID476
#' plot_ID476(sig, plot_title = "Example ID476")
#'
#' @seealso [plot_476()]
#' @export
plot_ID476 <- function(
  catalog,
  plot_title = NULL,
  base_size = 11,
  plot_title_cex = 1.0,
  count_label_cex = 0.52,
  class_label_cex = 0.78,
  axis_text_x_cex = 0.8,
  axis_title_x_cex = 0.7,
  axis_title_y_cex = 0.9,
  axis_text_y_cex = 0.7,
  show_counts = NULL,
  num_labels = 4,
  ggrepel_cex = 0.52,
  label_threshold_denominator = 7,
  vline_labels = c(),
  simplify_labels = FALSE,
  plot_complex = FALSE
) {
  plot_476(
    catalog = catalog,
    plot_title = plot_title,
    base_size = base_size,
    title_text_cex = plot_title_cex,
    count_label_cex = count_label_cex,
    block_text_cex = class_label_cex,
    x_axis_tick_label_cex = axis_text_x_cex,
    y_axis_tick_label_cex = axis_text_y_cex,
    x_title_cex = axis_title_x_cex,
    y_title_cex = axis_title_y_cex,
    show_counts = show_counts,
    num_labels = num_labels,
    ggrepel_cex = ggrepel_cex,
    label_threshold_denominator = label_threshold_denominator,
    vline_labels = vline_labels,
    simplify_labels = simplify_labels,
    plot_complex = plot_complex
  )
}

#' Export 476-channel indel profiles to PDF (ggplot2-consistent interface)
#'
#' Wrapper around [plot_ID476()] for multi-sample PDF export. Uses parameter
#' names consistent with ggplot2 theme element naming conventions. Plots
#' are arranged with 5 samples per page.
#'
#' @param catalog A matrix or data frame with 476 rows (indel types) and
#'   one column per sample. Column names are used as plot titles.
#' @param filename Character. Path to the output PDF file.
#' @inheritParams plot_ID476
#'
#' @return NULL. Called for side effect of creating a PDF file.
#'
#' @examples
#' \dontrun{
#' sig <- matrix(runif(476 * 3), nrow = 476)
#' rownames(sig) <- catalog_row_order()$ID476
#' colnames(sig) <- paste0("Sig", 1:3)
#' plot_ID476_pdf(sig, filename = "id476.pdf")
#' }
#'
#' @seealso [plot_476_pdf()], [plot_ID476()]
#' @export
#'
#' @import Cairo
#' @importFrom grDevices dev.off cairo_pdf
plot_ID476_pdf <- function(
  catalog,
  filename,
  base_size = 11,
  plot_title_cex = 1.0,
  count_label_cex = 0.52,
  class_label_cex = 0.78,
  axis_text_x_cex = 0.8,
  axis_title_x_cex = 0.7,
  axis_title_y_cex = 0.9,
  axis_text_y_cex = 0.7,
  show_counts = NULL,
  num_labels = 4,
  ggrepel_cex = 0.52,
  label_threshold_denominator = 7,
  vline_labels = c(),
  simplify_labels = FALSE,
  plot_complex = FALSE
) {
  plot_list <- lapply(1:ncol(catalog), function(i) {
    plot_ID476(
      catalog = catalog[, i],
      plot_title = colnames(catalog)[i],
      base_size = base_size,
      plot_title_cex = plot_title_cex,
      count_label_cex = count_label_cex,
      class_label_cex = class_label_cex,
      axis_text_x_cex = axis_text_x_cex,
      axis_title_x_cex = axis_title_x_cex,
      axis_title_y_cex = axis_title_y_cex,
      axis_text_y_cex = axis_text_y_cex,
      show_counts = show_counts,
      num_labels = num_labels,
      ggrepel_cex = ggrepel_cex,
      label_threshold_denominator = label_threshold_denominator,
      vline_labels = vline_labels,
      simplify_labels = simplify_labels,
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
