#' Export mutational profiles to PDF with automatic type detection
#'
#' Creates a multi-page PDF file containing mutational profile plots for
#' multiple samples. Automatically detects the catalog type based on the
#' number of rows. Plots are arranged with 5 samples per page (except for
#' heatmap types which use 1 per page). Uses Cairo for high-quality PDF
#' rendering.
#'
#' @param catalog A matrix or data frame with rows matching one of the
#'   supported catalog types (96, 192, 1536, 78, 136, 144, 83, 89, 166,
#'   or 476 rows) and one column per sample.
#'   Column names are used as plot titles.
#' @param filename Character. Path to the output PDF file.
#' @param ... Additional arguments passed to `plot_guess()` and the underlying
#'   plotting function.
#'
#' @return NULL. Called for side effect of creating a PDF file.
#'
#' @seealso [plot_guess()], [plot_SBS96_pdf()], [plot_SBS192_pdf()],
#'   [plot_SBS1536_pdf()], [plot_DBS78_pdf()], [plot_DBS136_pdf()],
#'   [plot_DBS144_pdf()], [plot_ID166_pdf()], [plot_476_pdf()], [plot_89_pdf()]
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
