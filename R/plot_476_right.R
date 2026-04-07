#' @rdname legacy_bar_plots
#'
#' @examples
#' set.seed(1)
#' sig <- runif(476)
#' sig <- sig / sum(sig)
#' names(sig) <- catalog_row_order()$ID476
#' plot_476_right(sig, plot_title = "Example ID476 right panel")
#'
#' @export
plot_476_right <- function(
  catalog,
  block_text_cex = 1.0,
  plot_title = NULL,
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

  plot_ID476_right(
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
