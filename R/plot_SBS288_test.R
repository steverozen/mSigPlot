#' Plot an SBS288 catalog as a 3-panel stacked SBS96 plot
#'
#' Takes a 288-row catalog (3 strand categories x 96 SBS channels) and produces
#' a vertically stacked 3-panel SBS96 plot with a shared y-axis maximum.
#' Row name format: `T:A[C>A]T`, `U:A[C>A]T`, `N:A[C>A]T` where `T:` is
#' template (transcribed), `U:` is non-template (untranscribed), and `N:` is
#' not-transcribed (intergenic).
#'
#' @param catalog Numeric vector, single-column data.frame, matrix, tibble,
#'   or data.table with 288 rows. Row names must have 2-character strand
#'   prefixes (`T:`, `U:`, `N:`).
#' @param plot_title Character. Optional overall title for the combined plot.
#' @param ... Additional arguments passed to [plot_SBS96()].
#'
#' @return A `patchwork` object (printable and compatible with `ggsave()`).
#'
#' @export
#'
#' @import patchwork
plot_SBS288_test <- function(catalog, plot_title = NULL, ...) {
  catalog <- normalize_catalog(catalog, 288, canonical_order = NULL)
  if (is.null(catalog)) return(NULL)

  rn <- rownames(catalog)

  # Split by 2-char strand prefix
  t_idx <- grep("^T:", rn)
  u_idx <- grep("^U:", rn)
  n_idx <- grep("^N:", rn)

  if (length(t_idx) + length(u_idx) + length(n_idx) != 288) {
    stop("Row names must all start with T:, U:, or N:")
  }

  # Build sub-catalogs, stripping the strand prefix
  make_sub <- function(idx) {
    sub_cat <- catalog[idx, 1, drop = FALSE]
    rownames(sub_cat) <- sub("^[TUN]:", "", rownames(sub_cat))
    sub_cat
  }

  cat_t <- make_sub(t_idx)
  cat_u <- make_sub(u_idx)
  cat_n <- make_sub(n_idx)

  # Shared y-axis max across all 3 panels
  global_max <- max(cat_t[, 1], cat_u[, 1], cat_n[, 1]) * 1.1

  p1 <- plot_SBS96(cat_t, plot_title = "Template",
                   ylim = c(0, global_max), ...)
  p2 <- plot_SBS96(cat_u, plot_title = "Non-template",
                   ylim = c(0, global_max), ...)
  p3 <- plot_SBS96(cat_n, plot_title = "Not-transcribed",
                   ylim = c(0, global_max), ...)

  combined <- patchwork::wrap_plots(p1, p2, p3, ncol = 1)

  if (!is.null(plot_title)) {
    combined <- combined + patchwork::plot_annotation(title = plot_title)
  }

  combined
}
