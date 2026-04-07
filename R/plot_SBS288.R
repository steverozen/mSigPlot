#' @rdname bar_plots
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' sig <- runif(288)
#' names(sig) <- catalog_row_order()$SBS288
#' plot_SBS288(sig, plot_title = "Example SBS288")
#' }
#'
#' @export
plot_SBS288 <- function(catalog, plot_title = NULL, ...) {
  # Convert stapled SBS288 row names like T:A[C>A]T to T:ACTA
  rn <- if (is.data.frame(catalog) || is.matrix(catalog)) {
    rownames(catalog)
  } else if (is.numeric(catalog)) {
    names(catalog)
  } else {
    NULL
  }
  if (!is.null(rn) &&
      all(grepl("^[TUN]:[ACGT]\\[[CT]>[ACGT]\\][ACGT]$", rn))) {
    # Strip prefix, unstaple, re-prefix
    prefix <- substr(rn, 1, 2)
    stapled <- substring(rn, 3)
    converted <- paste0(prefix, unstaple_SBS96_rownames(stapled))
    if (is.data.frame(catalog) || is.matrix(catalog)) {
      rownames(catalog) <- converted
    } else {
      names(catalog) <- converted
    }
  }

  catalog <- normalize_catalog(catalog, 288, catalog_row_order()$SBS288, "SBS288")
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

  # Shared y-axis range across all 3 panels
  global_max <- max(cat_t[, 1], cat_u[, 1], cat_n[, 1]) * 1.1
  global_min <- min(0, min(cat_t[, 1], cat_u[, 1], cat_n[, 1]))

  # Top panel: upper bars, no x-labels
  # Middle panel: no upper bars, no x-labels
  # Bottom panel: no upper bars, x-labels
  p1 <- plot_SBS96(cat_t, plot_title = "Template",
                   ylim = c(0, global_max),
                   upper = TRUE, show_axis_text_x = FALSE, ...)
  p2 <- plot_SBS96(cat_u, plot_title = "Non-template",
                   ylim = c(0, global_max),
                   upper = FALSE, show_axis_text_x = FALSE, ...)
  p3 <- plot_SBS96(cat_n, plot_title = "Not-transcribed",
                   ylim = c(0, global_max),
                   upper = FALSE, show_axis_text_x = TRUE, ...)

  # Force identical y-axis breaks across all panels (ggplot2 auto-selects

  # different breaks when panels have different physical heights)
  shared_breaks <- pretty(c(global_min, global_max), n = 4)
  is_counts <- sum(catalog[, 1], na.rm = TRUE) > 1.1
  shared_scale <- scale_y_continuous(
    breaks = shared_breaks,
    limits = c(min(0, global_min * 1.05), global_max),
    expand = c(0, 0),
    oob = scales::oob_keep,
    labels = if (is_counts) scales::label_number(accuracy = 1) else ggplot2::waiver()
  )
  p1 <- suppressMessages(p1 + shared_scale)
  p2 <- suppressMessages(p2 + shared_scale)
  p3 <- suppressMessages(p3 + shared_scale)

  # Height ratios: top panel gets space for upper bars,
  # bottom panel gets space for x-labels
  combined <- patchwork::wrap_plots(p1, p2, p3, ncol = 1,
                                    heights = c(1.2, 1, 1.4))

  if (!is.null(plot_title)) {
    combined <- combined + patchwork::plot_annotation(title = plot_title)
  }

  combined
}
