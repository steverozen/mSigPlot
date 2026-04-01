#' Plot 89-channel indel profile (ggplot2-consistent interface)
#'
#' Wrapper around [plot_89()] using parameter names consistent with
#' ggplot2 theme element naming conventions.
#'
#' @param catalog Numeric vector, single-column data.frame, matrix, tibble,
#'   or data.table with 89 rows.
#' @param plot_title Character. Title displayed above the plot.
#' @param upper Logical. If `TRUE`, display the category bar above the plot.
#'   Maps to `show_top_bar` in [plot_89()].
#' @param xlabels Logical. If `TRUE`, display x-axis tick labels.
#'   Maps to `show_x_axis_text` in [plot_89()].
#' @param ylim Numeric or NULL. If provided, sets a fixed y-axis maximum.
#'   Maps to `setyaxis` in [plot_89()].
#' @param base_size Base font size for ggplot2's theme.
#' @param plot_title_cex Numeric. Size of the plot title text, relative to
#'   `base_size`. Maps to `title_text_cex` in [plot_89()].
#' @param count_label_cex Numeric. Size of per-class count labels, as a
#'   fraction of `base_size`.
#' @param block_label_cex Numeric. Size of the lower category block labels
#'   (e.g. "Del 1 C", "Ins 1 T"). Maps to `text_cex` in [plot_89()].
#' @param class_label_cex Numeric. Size of the upper summary class labels
#'   (e.g. "Del", "Ins"). Maps to `top_bar_text_cex` in [plot_89()].
#' @param axis_text_x_cex Numeric. Size of x-axis tick labels. Maps to
#'   `x_axis_tick_label_cex` in [plot_89()].
#' @param axis_title_x_cex Numeric. Size of x-axis title, relative to
#'   `base_size`. Maps to `x_title_cex` in [plot_89()].
#' @param axis_title_y_cex Numeric. Size of y-axis title, relative to
#'   `base_size`. Maps to `y_title_cex` in [plot_89()].
#' @param axis_text_y_cex Numeric. Size of y-axis tick labels, relative to
#'   `base_size`. Maps to `y_axis_tick_label_cex` in [plot_89()].
#' @param show_counts Logical or NULL. If `TRUE`, always display per-class
#'   mutation count labels. If `FALSE`, never display them. If `NULL`
#'   (the default), display them only when the catalog contains counts
#'   (sum > 1.1).
#' @param ylabel Character or NULL. Custom y-axis label.
#' @param show_extra_top_bar Logical. If `TRUE`, display the extra summary
#'   bar above the category bar.
#' @param plot_complex Logical. If `TRUE`, include the Complex indel channel.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' set.seed(1)
#' sig <- runif(89)
#' sig <- sig / sum(sig)
#' names(sig) <- catalog_row_order()$ID89
#' plot_ID89(sig, plot_title = "Example ID89")
#'
#' @seealso [plot_89()]
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

#' Export 89-channel indel profiles to PDF (ggplot2-consistent interface)
#'
#' Wrapper around [plot_ID89()] for multi-sample PDF export. Uses parameter
#' names consistent with ggplot2 theme element naming conventions. Plots
#' are arranged with 5 samples per page.
#'
#' @param catalog A matrix or data frame with 89 rows (indel types) and
#'   one column per sample. Column names are used as plot titles.
#' @param filename Character. Path to the output PDF file.
#' @inheritParams plot_ID89
#'
#' @return NULL. Called for side effect of creating a PDF file.
#'
#' @examples
#' \dontrun{
#' sig <- matrix(runif(89 * 3), nrow = 89)
#' rownames(sig) <- catalog_row_order()$ID89
#' colnames(sig) <- paste0("Sig", 1:3)
#' plot_ID89_pdf(sig, filename = "id89.pdf")
#' }
#'
#' @seealso [plot_89_pdf()], [plot_ID89()]
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
