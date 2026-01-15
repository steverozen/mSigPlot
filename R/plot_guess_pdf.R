#' Export indel profiles to PDF with automatic channel detection
#'
#' Creates a multi-page PDF file containing indel profile plots for multiple
#' samples. Automatically detects the channel scheme (476, 89, or 83) based
#' on the number of rows in the catalog. Plots are arranged with 5 samples
#' per page. Uses Cairo for high-quality PDF rendering.
#'
#' @param catalog A matrix or data frame with rows matching one of the
#'   supported channel schemes (476, 89, or 83 rows) and one column per sample.
#'   Column names are used as plot titles.
#' @param filename Character. Path to the output PDF file.
#' @param ... Additional arguments passed to `plot_guess()` and the underlying
#'   plotting function.
#'
#' @return NULL. Called for side effect of creating a PDF file.
#'
#' @seealso [plot_guess()], [plot_476_pdf()], [plot_89_pdf()]
#' @export
#'
#' @import Cairo
#' @importFrom grDevices dev.off cairo_pdf
plot_guess_pdf <- function(catalog, filename, ...) {
  plot_list <- lapply(1:ncol(catalog), function(i) {
    plot_guess(
      catalog = catalog[, i, drop = FALSE],
      plot_title = colnames(catalog)[i],
      ...
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
