#' @rdname legacy_bar_plots
#'
#' @examples
#' set.seed(1)
#' sig <- runif(89)
#' sig <- sig / sum(sig)
#' names(sig) <- catalog_row_order()$ID89
#' plot_89(sig, plot_title = "Example ID89 signature")
#'
#' @export
plot_89 <- function(
  catalog,
  text_cex = 3,
  top_bar_text_cex = text_cex,
  title_text_cex = 1.0,
  plot_title = NULL,
  setyaxis = NULL,
  ylabel = NULL,
  base_size = 11,
  x_axis_tick_label_cex = 0.6,
  y_axis_tick_label_cex = 0.8,
  x_title_cex = 0.9,
  y_title_cex = 0.9,
  show_x_axis_text = TRUE,
  show_top_bar = TRUE,
  show_extra_top_bar = FALSE,
  plot_complex = FALSE,
  show_counts = NULL,
  count_label_cex = 1.03,
  num_peak_labels = 0,
  peak_label_cex = 0.7,
  stop_at_9 = TRUE,
  text_size = NULL,
  top_bar_text_size = NULL,
  title_text_size = NULL,
  x_axis_tick_label_size = NULL,
  y_axis_tick_label_size = NULL,
  x_title_size = NULL,
  y_title_size = NULL,
  count_label_size = NULL
) {
  # === start dealing with deprecated
  if (!is.null(text_size)) {
    if (!missing(text_cex)) {
      stop("Cannot specify both 'text_size' and 'text_cex'.")
    }
    warning("'text_size' is deprecated. Use 'text_cex' instead.")
    text_cex <- text_size
  }
  if (!is.null(top_bar_text_size)) {
    if (!missing(top_bar_text_cex)) {
      stop("Cannot specify both 'top_bar_text_size' and 'top_bar_text_cex'.")
    }
    warning("'top_bar_text_size' is deprecated. Use 'top_bar_text_cex' instead.")
    top_bar_text_cex <- top_bar_text_size
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

  plot_ID89(
    catalog = catalog,
    plot_title = plot_title,
    upper = show_top_bar,
    show_axis_text_x = show_x_axis_text,
    ylim = setyaxis,
    base_size = base_size,
    plot_title_cex = title_text_cex,
    count_label_cex = count_label_cex,
    block_label_cex = text_cex,
    class_label_cex = top_bar_text_cex,
    axis_text_x_cex = x_axis_tick_label_cex,
    axis_title_x_cex = x_title_cex,
    axis_title_y_cex = y_title_cex,
    axis_text_y_cex = y_axis_tick_label_cex,
    show_counts = show_counts,
    ylab = if (is.null(ylabel)) TRUE else ylabel,
    plot_complex = plot_complex,
    num_peak_labels = num_peak_labels,
    peak_label_cex = peak_label_cex,
    stop_at_9 = stop_at_9
  )
}
