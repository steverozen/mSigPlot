#' Plot 83-channel indel profile (ggplot2-consistent interface)
#'
#' Wrapper around [plot_83()] using parameter names consistent with
#' ggplot2 theme element naming conventions.
#'
#' @param catalog Numeric vector, single-column data.frame, matrix, tibble,
#'   or data.table with 83 rows.
#' @param plot_title Character. Title displayed above the plot.
#' @param grid Logical, draw grid lines.
#' @param upper Logical, draw colored class rectangles and labels above bars.
#' @param xlabels Logical, draw x-axis labels.
#' @param ylabels Logical, draw y-axis labels.
#' @param ylim Optional y-axis limits.
#' @param base_size Numeric. Base font size in points.
#' @param plot_title_cex Numeric. Multiplier for the plot title size.
#' @param count_label_cex Numeric. Multiplier for per-class count labels.
#' @param block_label_cex Numeric. Multiplier for the upper colored category
#'   block labels.
#' @param class_label_cex Numeric. Multiplier for the major class labels.
#' @param axis_text_x_cex Numeric. Multiplier for x-axis channel labels.
#' @param bottom_label_cex Numeric. Multiplier for bottom category description
#'   labels.
#' @param axis_title_x_cex Numeric. Multiplier for x-axis title size.
#'   Currently has no effect in this function.
#' @param axis_title_y_cex Numeric. Multiplier for the y-axis title size.
#' @param axis_text_y_cex Numeric. Multiplier for the y-axis tick label size.
#' @param show_counts Logical or NULL. If `TRUE`, always display per-class
#'   mutation count labels. If `FALSE`, never display them. If `NULL`
#'   (the default), display them only when the catalog contains counts
#'   (sum > 1.1).
#'
#' @return A ggplot object.
#'
#' @examples
#' set.seed(1)
#' sig <- runif(83)
#' sig <- sig / sum(sig)
#' names(sig) <- catalog_row_order()$ID
#' plot_ID83(sig, plot_title = "Example ID83")
#'
#' @seealso [plot_83()]
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

#' Export 83-channel indel profiles to PDF (ggplot2-consistent interface)
#'
#' Wrapper around [plot_ID83()] for multi-sample PDF export. Uses parameter
#' names consistent with ggplot2 theme element naming conventions. Plots
#' are arranged with 5 samples per page.
#'
#' @param catalog A matrix or data frame with 83 rows (indel types) and
#'   one column per sample. Column names are used as plot titles.
#' @param filename Character. Path to the output PDF file.
#' @inheritParams plot_ID83
#'
#' @return NULL. Called for side effect of creating a PDF file.
#'
#' @examples
#' \dontrun{
#' sig <- matrix(runif(83 * 3), nrow = 83)
#' rownames(sig) <- catalog_row_order()$ID
#' colnames(sig) <- paste0("Sig", 1:3)
#' plot_ID83_pdf(sig, filename = "id83.pdf")
#' }
#'
#' @seealso [plot_83_pdf()], [plot_ID83()]
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
