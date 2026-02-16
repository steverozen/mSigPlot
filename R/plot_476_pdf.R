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
#' @param block_text_cex Numeric. Size of category block labels, as a fraction
#'   of `base_size`.
#' @param ggrepel_cex Numeric. Size of ggrepel peak labels, as a fraction of
#'   `base_size`.
#' @param title_text_cex Numeric. Size of the plot title text, relative to `base_size`.
#' @param x_axis_tick_label_cex Numeric. Size of x-axis tick labels, relative to `base_size`.
#' @param y_axis_tick_label_cex Numeric. Size of y-axis tick labels, relative to `base_size`.
#' @param x_title_cex Numeric. Size of x-axis title, relative to `base_size`.
#' @param y_title_cex Numeric. Size of y-axis title, relative to `base_size`.
#' @param plot_complex Logical. If TRUE, include the 5 Complex indel channels.
#' @param show_counts Logical or NULL. If `TRUE`, always display per-class
#'   mutation count labels. If `FALSE`, never display them. If `NULL`
#'   (the default), display them only when the catalog contains counts
#'   (sum > 1.1).
#' @param count_label_cex Numeric. Size of per-class count labels, as a
#'   fraction of `base_size`.
#' @param block_text_size Deprecated. Use `block_text_cex` instead.
#' @param ggrepel_size Deprecated. Use `ggrepel_cex` instead.
#' @param title_text_size Deprecated. Use `title_text_cex` instead.
#' @param x_axis_tick_label_size Deprecated. Use `x_axis_tick_label_cex` instead.
#' @param y_axis_tick_label_size Deprecated. Use `y_axis_tick_label_cex` instead.
#' @param x_title_size Deprecated. Use `x_title_cex` instead.
#' @param y_title_size Deprecated. Use `y_title_cex` instead.
#' @param count_label_size Deprecated. Use `count_label_cex` instead.
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
  block_text_cex = 0.78,
  ggrepel_cex = 0.52,
  title_text_cex = 1.0,
  x_axis_tick_label_cex = 0.8,
  y_axis_tick_label_cex = 0.7,
  x_title_cex = 0.7,
  y_title_cex = 0.9,
  plot_complex = FALSE,
  show_counts = NULL,
  count_label_cex = 0.52,
  block_text_size = NULL,
  ggrepel_size = NULL,
  title_text_size = NULL,
  x_axis_tick_label_size = NULL,
  y_axis_tick_label_size = NULL,
  x_title_size = NULL,
  y_title_size = NULL,
  count_label_size = NULL
) {
  # === start dealing with deprecated
  if (!is.null(block_text_size)) {
    if (!missing(block_text_cex)) {
      stop("Cannot specify both 'block_text_size' and 'block_text_cex'.")
    }
    warning("'block_text_size' is deprecated. Use 'block_text_cex' instead.")
    block_text_cex <- block_text_size
  }
  if (!is.null(ggrepel_size)) {
    if (!missing(ggrepel_cex)) {
      stop("Cannot specify both 'ggrepel_size' and 'ggrepel_cex'.")
    }
    warning("'ggrepel_size' is deprecated. Use 'ggrepel_cex' instead.")
    ggrepel_cex <- ggrepel_size
  }
  if (!is.null(title_text_size)) {
    if (!missing(title_text_cex)) {
      stop("Cannot specify both 'title_text_size' and 'title_text_cex'.")
    }
    warning("'title_text_size' is deprecated. Use 'title_text_cex' instead.")
    title_text_cex <- title_text_size
  }
  if (!is.null(x_axis_tick_label_size)) {
    if (!missing(x_axis_tick_label_cex)) {
      stop("Cannot specify both 'x_axis_tick_label_size' and 'x_axis_tick_label_cex'.")
    }
    warning("'x_axis_tick_label_size' is deprecated. Use 'x_axis_tick_label_cex' instead.")
    x_axis_tick_label_cex <- x_axis_tick_label_size
  }
  if (!is.null(y_axis_tick_label_size)) {
    if (!missing(y_axis_tick_label_cex)) {
      stop("Cannot specify both 'y_axis_tick_label_size' and 'y_axis_tick_label_cex'.")
    }
    warning("'y_axis_tick_label_size' is deprecated. Use 'y_axis_tick_label_cex' instead.")
    y_axis_tick_label_cex <- y_axis_tick_label_size
  }
  if (!is.null(x_title_size)) {
    if (!missing(x_title_cex)) {
      stop("Cannot specify both 'x_title_size' and 'x_title_cex'.")
    }
    warning("'x_title_size' is deprecated. Use 'x_title_cex' instead.")
    x_title_cex <- x_title_size
  }
  if (!is.null(y_title_size)) {
    if (!missing(y_title_cex)) {
      stop("Cannot specify both 'y_title_size' and 'y_title_cex'.")
    }
    warning("'y_title_size' is deprecated. Use 'y_title_cex' instead.")
    y_title_cex <- y_title_size
  }
  if (!is.null(count_label_size)) {
    if (!missing(count_label_cex)) {
      stop("Cannot specify both 'count_label_size' and 'count_label_cex'.")
    }
    warning("'count_label_size' is deprecated. Use 'count_label_cex' instead.")
    count_label_cex <- count_label_size
  }
  # === end dealing with deprecated

  plot_list <- lapply(1:ncol(catalog), function(i) {
    plot_476(
      catalog = catalog[, i],
      block_text_cex = block_text_cex,
      plot_title = colnames(catalog)[i],
      num_labels = num_labels,
      ggrepel_cex = ggrepel_cex,
      simplify_labels = simplify_labels,
      label_threshold_denominator = label_threshold_denominator,
      vline_labels = vline_labels,
      base_size = base_size,
      title_text_cex = title_text_cex,
      x_axis_tick_label_cex = x_axis_tick_label_cex,
      y_axis_tick_label_cex = y_axis_tick_label_cex,
      x_title_cex = x_title_cex,
      y_title_cex = y_title_cex,
      plot_complex = plot_complex,
      show_counts = show_counts,
      count_label_cex = count_label_cex
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
