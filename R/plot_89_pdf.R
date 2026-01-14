#' Export 89-channel indel profiles to PDF
#'
#' Creates a multi-page PDF file containing 89-channel indel profile plots
#' for multiple samples. Plots are arranged with 5 samples per page.
#'
#' @param catalog A matrix or data frame with 89 rows (indel types) and
#'   one column per sample. Column names are used as plot titles.
#' @param text_size Numeric. Size of text labels in the plot.
#' @param top_bar_text_size Numeric shows the size of the labels in the colored top bar.
#' @param title_text_size Numeric. Relative size of the plot title text, passed to `rel()`.
#' @param filename Character. Path to the output PDF file.
#' @param show_x_axis_text Logical. If `TRUE`, display x-axis tick labels.
#' @param show_top_bar Logical. If `TRUE`, display the category bar above the
#'   plot.
#'
#' @return NULL. Called for side effect of creating a PDF file.
#'
#' @export
#'
#' @import Cairo dplyr
#' @importFrom grDevices dev.off cairo_pdf
plot_89_pdf <- function(
  catalog,
  text_size = 3,
  top_bar_text_size = text_size,
  title_text_size = 1.0,
  filename,
  show_x_axis_text = TRUE,
  show_top_bar = TRUE
) {
  plot_list <- lapply(1:ncol(catalog), function(i) {
    plot_89(
      catalog = catalog[, i],
      text_size = text_size,
      top_bar_text_size = top_bar_text_size,
      title_text_size = title_text_size,
      plot_title = colnames(catalog)[i],
      show_x_axis_text = show_x_axis_text,
      show_top_bar = show_top_bar
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
