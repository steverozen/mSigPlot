library(gridExtra)
library(Cairo)

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
#'   Default is 4.
#' @param simplify_labels Logical. If TRUE, simplifies peak labels by removing
#'   the indel type prefix. Default is FALSE.
#' @param label_threshold_denominator Numeric. Peaks with values less than
#'   max/label_threshold_denominator are not labeled. Default is 7.
#'
#' @return NULL. Called for side effect of creating a PDF file.
#'
#' @export
plot_476_pdf <- function(
  catalog,
  filename,
  num_labels = 4,
  simplify_labels = FALSE,
  label_threshold_denominator = 7
) {
  plot_list <- lapply(1:ncol(catalog), function(i) {
    plot_476(
      catalog = catalog[, i],
      text_size = 3,
      plot_title = colnames(catalog)[i],
      num_labels = num_labels,
      simplify_labels = simplify_labels,
      label_threshold_denominator = label_threshold_denominator
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
    do.call(grid.arrange, c(plots_on_page, nrow = 5, ncol = 1))
  }

  # Close the PDF device
  dev.off()
}
