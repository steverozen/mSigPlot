#' @rdname plot_guess
#'
#' @examples
#' \dontrun{
#' sig <- matrix(runif(96 * 3), nrow = 96)
#' rownames(sig) <- catalog_row_order()$SBS96
#' colnames(sig) <- paste0("Sig", 1:3)
#' plot_guess_pdf(sig, filename = "auto_detected.pdf")
#' }
#'
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
