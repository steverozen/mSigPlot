#' plot_476, plot_476_pdf, plot_476_right, plot_476_right_pdf, plot_83, plot_89
#'
#' Original plot functions for indel mutational signature catalogs.
#' These are kept for backward compatibility. For new code, prefer
#' the ggplot2-consistent wrappers: [plot_ID83()], [plot_ID89()],
#' [plot_ID476()], [plot_ID476_right()].
#'
#' Functions in this family:
#' - `plot_83`: Indel COSMIC classification (83 channels)
#' - `plot_89`: Indel Koh classification (89 channels)
#' - `plot_476`: Indel with flanking context (476 channels)
#' - `plot_476_right`: Right portion of 476-channel profile (positions 343-476)
#'
#' Each has a corresponding `_pdf()` variant for multi-sample PDF export.
#'
#' @param catalog Numeric vector, single-column data.frame, matrix, tibble,
#'   or data.table. If there are row names (or for a vector, names), they
#'   will be checked against [catalog_row_order()].
#' @param plot_title Character. Title displayed above the plot.
#' @param filename Character. Path to the output PDF file (\_pdf functions only).
#' @param grid Logical, draw grid lines.
#' @param upper Logical, draw colored class rectangles and labels above bars.
#' @param xlabels Logical, draw x-axis labels.
#' @param ylabels Logical, draw y-axis labels.
#' @param ylim Optional y-axis limits.
#' @param base_size Numeric. Base font size in points.
#' @param title_cex Numeric. Multiplier for the plot title size.
#' @param count_label_cex Numeric. Multiplier for per-class count labels.
#' @param block_label_cex Numeric. Multiplier for colored category block labels.
#' @param class_label_cex Numeric. Multiplier for major class labels.
#' @param x_label_cex Numeric. Multiplier for x-axis labels.
#' @param bottom_label_cex Numeric. Multiplier for bottom category description labels.
#' @param axis_title_cex Numeric. Multiplier for the y-axis title size.
#' @param axis_text_cex Numeric. Multiplier for the y-axis tick label size.
#' @param show_counts Logical or NULL. If `TRUE`, always display per-class
#'   count labels. If `FALSE`, never display them. If `NULL` (the default),
#'   display them only when the catalog contains counts (sum > 1.1).
#' @param text_cex Numeric. Size of text labels in the plot (`plot_89` only).
#' @param top_bar_text_cex Numeric. Size of labels in the colored top bars (`plot_89` only).
#' @param title_text_cex Numeric. Size of the plot title text (`plot_89`, `plot_476`,
#'   `plot_476_right` only).
#' @param setyaxis Numeric or NULL. Fixed y-axis maximum (`plot_89` only).
#' @param ylabel Character or NULL. Custom y-axis label (`plot_89` only).
#' @param x_axis_tick_label_cex Numeric. Size of x-axis tick labels
#'   (`plot_89`, `plot_476`, `plot_476_right` only).
#' @param y_axis_tick_label_cex Numeric. Size of y-axis tick labels
#'   (`plot_89`, `plot_476`, `plot_476_right` only).
#' @param x_title_cex Numeric. Size of x-axis title
#'   (`plot_89`, `plot_476`, `plot_476_right` only).
#' @param y_title_cex Numeric. Size of y-axis title
#'   (`plot_89`, `plot_476`, `plot_476_right` only).
#' @param show_x_axis_text Logical. Display x-axis tick labels (`plot_89` only).
#' @param show_top_bar Logical. Display the category bar above the plot (`plot_89` only).
#' @param show_extra_top_bar Logical. Display the extra summary bar (`plot_89` only).
#' @param plot_complex Logical. Include Complex indel channels
#'   (`plot_89`, `plot_476`, `plot_476_right` only).
#' @param block_text_cex Numeric. Size of category block labels
#'   (`plot_476`, `plot_476_right` only).
#' @param num_peak_labels Integer. Number of top peaks to label
#'   (`plot_476`, `plot_476_right` only).
#' @param peak_label_cex Numeric. Size of ggrepel peak labels
#'   (`plot_476`, `plot_476_right` only).
#' @param label_threshold_denominator Numeric. Peaks below
#'   max/label_threshold_denominator are not labeled
#'   (`plot_476`, `plot_476_right` only).
#' @param vline_labels Character vector. IndelType labels for vertical
#'   reference lines (`plot_476`, `plot_476_right` only).
#' @param simplify_labels Logical. Simplify peak labels
#'   (`plot_476`, `plot_476_right` only).
#' @param text_size Deprecated. Use `text_cex` instead.
#' @param top_bar_text_size Deprecated. Use `top_bar_text_cex` instead.
#' @param title_text_size Deprecated. Use `title_text_cex` instead.
#' @param count_label_size Deprecated. Use `count_label_cex` instead.
#' @param x_axis_tick_label_size Deprecated. Use `x_axis_tick_label_cex` instead.
#' @param y_axis_tick_label_size Deprecated. Use `y_axis_tick_label_cex` instead.
#' @param x_title_size Deprecated. Use `x_title_cex` instead.
#' @param y_title_size Deprecated. Use `y_title_cex` instead.
#' @param block_text_size Deprecated. Use `block_text_cex` instead.
#' @param ggrepel_size Deprecated. Use `peak_label_cex` instead.
#' @param label_size Deprecated. Use `peak_label_cex` instead.
#'
#' @return Plot functions return a ggplot2 object, or NULL with a warning
#'   if the catalog is invalid (wrong size or row names). PDF functions
#'   return NULL invisibly (called for side effect of creating a PDF file),
#'   or stop with an error if the catalog is invalid.
#'
#' @name legacy_bar_plots
NULL

#' @rdname legacy_bar_plots
#'
#' @examples
#' set.seed(1)
#' sig <- runif(476)
#' sig <- sig / sum(sig)
#' names(sig) <- catalog_row_order()$ID476
#' plot_476(sig, plot_title = "Example ID476 signature")
#'
#' @export
plot_476 <- function(
  catalog,
  block_text_cex = 0.78,
  plot_title = NULL,
  num_peak_labels = 3,
  peak_label_cex = 0.52,
  label_threshold_denominator = 7,
  vline_labels = c(),
  simplify_labels = TRUE,
  base_size = 11,
  title_text_cex = 1.0,
  x_axis_tick_label_cex = 0.8,
  y_axis_tick_label_cex = 0.7,
  x_title_cex = 0.7,
  y_title_cex = 0.9,
  plot_complex = FALSE,
  show_counts = NULL,
  count_label_cex = 0.52,
  stop_at_9 = TRUE,
  block_text_size = NULL,
  ggrepel_size = NULL,
  title_text_size = NULL,
  x_axis_tick_label_size = NULL,
  y_axis_tick_label_size = NULL,
  x_title_size = NULL,
  y_title_size = NULL,
  count_label_size = NULL,
  text_size = NULL,
  label_size = NULL
) {
  # === start dealing with deprecated
  if (!is.null(text_size)) {
    if (!missing(block_text_cex)) {
      stop("Cannot specify both 'text_size' and 'block_text_cex'.")
    }
    warning("'text_size' is deprecated. Use 'block_text_cex' instead.")
    block_text_cex <- text_size
  }
  if (!is.null(label_size)) {
    if (!missing(peak_label_cex)) {
      stop("Cannot specify both 'label_size' and 'peak_label_cex'.")
    }
    warning("'label_size' is deprecated. Use 'peak_label_cex' instead.")
    peak_label_cex <- label_size
  }
  if (!is.null(block_text_size)) {
    if (!missing(block_text_cex)) {
      stop("Cannot specify both 'block_text_size' and 'block_text_cex'.")
    }
    warning("'block_text_size' is deprecated. Use 'block_text_cex' instead.")
    block_text_cex <- block_text_size
  }
  if (!is.null(ggrepel_size)) {
    if (!missing(peak_label_cex)) {
      stop("Cannot specify both 'ggrepel_size' and 'peak_label_cex'.")
    }
    warning("'ggrepel_size' is deprecated. Use 'peak_label_cex' instead.")
    peak_label_cex <- ggrepel_size
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

  plot_ID476(
    catalog = catalog,
    plot_title = plot_title,
    base_size = base_size,
    plot_title_cex = title_text_cex,
    count_label_cex = count_label_cex,
    class_label_cex = block_text_cex,
    axis_text_x_cex = x_axis_tick_label_cex,
    axis_title_x_cex = x_title_cex,
    axis_title_y_cex = y_title_cex,
    axis_text_y_cex = y_axis_tick_label_cex,
    show_counts = show_counts,
    num_peak_labels = num_peak_labels,
    peak_label_cex = peak_label_cex,
    label_threshold_denominator = label_threshold_denominator,
    vline_labels = vline_labels,
    simplify_labels = simplify_labels,
    plot_complex = plot_complex,
    stop_at_9 = stop_at_9
  )
}
