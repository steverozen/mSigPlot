#' @rdname legacy_plots
#' @export
#'
#' @import Cairo dplyr
#' @importFrom grDevices dev.off cairo_pdf
plot_89_pdf <- function(
  catalog,
  text_cex = 3,
  top_bar_text_cex = text_cex,
  title_text_cex = 1.0,
  filename,
  show_x_axis_text = TRUE,
  show_top_bar = TRUE,
  show_counts = NULL,
  count_label_cex = 1.03,
  text_size = NULL,
  top_bar_text_size = NULL,
  title_text_size = NULL,
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
  if (!is.null(count_label_size)) {
    if (!missing(count_label_cex)) {
      stop("Cannot specify both 'count_label_size' and 'count_label_cex'.")
    }
    warning("'count_label_size' is deprecated. Use 'count_label_cex' instead.")
    count_label_cex <- count_label_size
  }
  # === end dealing with deprecated

  plot_list <- lapply(1:ncol(catalog), function(i) {
    plot_89(
      catalog = catalog[, i],
      text_cex = text_cex,
      top_bar_text_cex = top_bar_text_cex,
      title_text_cex = title_text_cex,
      plot_title = colnames(catalog)[i],
      show_x_axis_text = show_x_axis_text,
      show_top_bar = show_top_bar,
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
