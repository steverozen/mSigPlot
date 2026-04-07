#' plot_DBS144, plot_DBS144_pdf, plot_DBS78, plot_DBS78_pdf, plot_ID166, plot_ID166_pdf, plot_ID476, plot_ID476_pdf, plot_ID476_right, plot_ID476_right_pdf, plot_ID83, plot_ID83_pdf, plot_ID89, plot_ID89_pdf, plot_SBS12, plot_SBS12_pdf, plot_SBS192, plot_SBS192_pdf, plot_SBS288, plot_SBS96, plot_SBS96_pdf
#'
#' Plot functions for SBS, DBS, and indel mutational signature catalogs
#' as bar charts. All functions return ggplot2 objects.
#'
#' Functions in this family:
#' - `plot_SBS96`, `plot_SBS192`: SBS trinucleotide context
#' - `plot_DBS78`: DBS dinucleotide substitutions
#' - `plot_DBS144`: DBS with transcription strand
#' - `plot_SBS12`: SBS strand bias summary (collapses 192 to 12 bars)
#' - `plot_SBS288`: SBS with three-strand context
#' - `plot_ID166`: Indel genic/intergenic (166 channels)
#' - `plot_ID83`: Indel COSMIC classification (83 channels)
#' - `plot_ID89`: Indel Koh classification (89 channels)
#' - `plot_ID476`, `plot_ID476_right`: Indel with flanking context (476 channels)
#'
#' Each has a corresponding `_pdf()` variant for multi-sample PDF export.
#'
#' @param catalog Numeric vector, single-column data.frame, matrix, tibble,
#'   or data.table. If there are row names (or for a vector, names), they
#'   will be checked against [catalog_row_order()].
#' @param plot_title Character. Title displayed above the plot.
#' @param filename Character. Path to the output PDF file (\_pdf functions only).
#' @param grid Logical, draw grid lines.
#' @param upper Logical, draw colored class rectangles and labels above bars.
#' @param ylim Optional y-axis limits.
#' @param base_size Numeric. Base font size in points.
#' @param plot_title_cex Numeric. Multiplier for the plot title size.
#' @param count_label_cex Numeric. Multiplier for per-class count labels.
#' @param class_label_cex Numeric. Multiplier for major class labels.
#' @param block_label_cex Numeric. Multiplier for colored category block labels
#'   (indel plots only).
#' @param bottom_label_cex Numeric. Multiplier for bottom category description
#'   labels (indel plots only).
#' @param axis_text_x_cex Numeric. Multiplier for x-axis labels.
#' @param axis_title_x_cex Numeric. Multiplier for x-axis title size.
#'   Currently has no effect in some functions.
#' @param axis_title_y_cex Numeric. Multiplier for the y-axis title size.
#' @param axis_text_y_cex Numeric. Multiplier for the y-axis tick label size.
#' @param show_axis_text_x Logical. If FALSE, hide x-axis tick labels.
#' @param show_axis_text_y Logical. If FALSE, hide y-axis tick labels.
#' @param show_axis_title_x Logical. If FALSE, hide the x-axis title.
#' @param show_axis_title_y Logical. If FALSE, hide the y-axis title.
#' @param xlabels Deprecated; use `show_axis_text_x` instead.
#' @param ylabels Deprecated; use `show_axis_text_y` and `show_axis_title_y`
#'   instead.
#' @param show_counts Logical or NULL. If `TRUE`, always display per-class
#'   count labels. If `FALSE`, never display them. If `NULL` (the default),
#'   display them only when the catalog contains counts (sum > 1.1).
#' @param abundance Numeric vector of per-class abundances for strand bias
#'   testing (`plot_SBS12` only).
#' @param ylab Y-axis label control (`plot_ID89` only). `TRUE` (default)
#'   auto-detects from data. A character string overrides the label.
#'   `NULL` or `FALSE` suppresses the y-axis title.
#' @param show_extra_top_bar Logical. Display an extra summary bar above the
#'   category bar (`plot_ID89` only).
#' @param plot_complex Logical. Include Complex indel channels
#'   (`plot_ID89`, `plot_ID476`, `plot_ID476_right` only).
#' @param num_labels Integer. Number of top peaks to label per category block
#'   (`plot_ID476`, `plot_ID476_right` only).
#' @param ggrepel_cex Numeric. Size of ggrepel peak labels
#'   (`plot_ID476`, `plot_ID476_right` only).
#' @param label_threshold_denominator Numeric. Peaks below
#'   max/label_threshold_denominator are not labeled
#'   (`plot_ID476`, `plot_ID476_right` only).
#' @param vline_labels Character vector. IndelType labels at which to draw
#'   vertical reference lines (`plot_ID476`, `plot_ID476_right` only).
#' @param simplify_labels Logical. Simplify peak labels by removing
#'   the indel type prefix (`plot_ID476`, `plot_ID476_right` only).
#' @param show_x_labels Logical. Display channel labels as rotated x-axis
#'   tick labels (`plot_ID476_right` only).
#' @param ... Additional arguments passed to `plot_SBS96()` (`plot_SBS288` only).
#'
#' @return Plot functions return a ggplot2 object, or NULL with a warning
#'   if the catalog is invalid (wrong size or row names). PDF functions
#'   return NULL invisibly (called for side effect of creating a PDF file),
#'   or stop with an error if the catalog is invalid.
#'
#' @name bar_plots
NULL

#' @rdname bar_plots
#'
#' @examples
#' set.seed(1)
#' sig <- runif(144)
#' sig <- sig / sum(sig)
#' names(sig) <- catalog_row_order()$DBS144
#' plot_DBS144(sig, plot_title = "Example DBS144 signature")
#'
#' @export
#'
#' @import ggplot2 dplyr
plot_DBS144 <- function(
  catalog,
  plot_title = NULL,
  show_axis_text_x = TRUE,
  show_axis_text_y = TRUE,
  show_axis_title_x = TRUE,
  show_axis_title_y = TRUE,
  ylabels = NULL,
  ylim = NULL,
  base_size = 11,
  plot_title_cex = 0.8,
  axis_text_x_cex = 1.0,
  axis_title_x_cex = 1.0,
  axis_title_y_cex = 1.0,
  axis_text_y_cex = 0.8
) {
  catalog <- normalize_catalog(catalog, 144, catalog_row_order()$DBS144, "DBS144")
  if (is.null(catalog)) return(NULL)
  if (is.null(plot_title)) plot_title <- colnames(catalog)[1] %||% ""

  axis_vis <- resolve_axis_params(
    show_axis_text_x, show_axis_text_y,
    show_axis_title_x, show_axis_title_y,
    ylabels = ylabels
  )
  show_axis_text_x <- axis_vis$show_axis_text_x
  show_axis_text_y <- axis_vis$show_axis_text_y
  show_axis_title_y <- axis_vis$show_axis_title_y

  base_mm <- base_mm(base_size)

  strand_col <- c("#394398", "#e83020")
  xlabel <- c("AC", "AT", "CC", "CG", "CT", "GC", "TA", "TC", "TG", "TT")

  # Reorder for plotting (132 entries in paired strand order)
  reorder <- reorder_DBS144_for_plotting()
  cat_reordered <- catalog[reorder, 1]

  # Detect catalog type
  catalog_type <- detect_catalog_type(catalog[, 1], attributes(catalog)$catalog.type)

  # Collapse 132 entries into 20 bars (10 classes x 2 strands)
  # Class boundaries within the 132-entry reordered vector:
  idx <- c(0, 18, 24, 42, 48, 66, 72, 78, 96, 114, 132)
  counts_strand <- numeric(20)
  for (i in 1:10) {
    counts_strand[2 * i - 1] <-
      sum(cat_reordered[seq(idx[i] + 1, idx[i + 1], by = 2)])
    counts_strand[2 * i] <-
      sum(cat_reordered[seq(idx[i] + 2, idx[i + 1], by = 2)])
  }

  if (catalog_type == "density") {
    counts_strand <- counts_strand * 1e6
    ylabel <- "mut/million"
    ymax <- max(counts_strand) * 1.3
  } else if (catalog_type == "counts") {
    ymax <- 4 * ceiling(max(max(counts_strand) * 1.3, 10) / 4)
    ylabel <- "counts"
  } else {
    ylabel <- ifelse(catalog_type == "counts.signature",
                     "counts proportion", "density proportion")
    ymax <- min(max(counts_strand) * 1.3, 1)
  }

  if (!is.null(ylim)) {
    ymax <- ylim[2]
  }
  ymin <- min(0, min(counts_strand))

  # Build data frame
  df <- data.frame(
    x = 1:20,
    value = counts_strand,
    strand = rep(c("Transcribed", "Untranscribed"), 10),
    stringsAsFactors = FALSE
  )
  bar_colors <- rep(strand_col, 10)

  # Build plot
  p <- ggplot(df, aes(x = x, y = value)) +
    geom_bar(stat = "identity", fill = bar_colors, width = 0.7) +
    scale_x_continuous(
      breaks = seq(1.5, 19.5, by = 2),
      labels = xlabel,
      limits = c(0, 21),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(min(0, ymin * 1.05), ymax),
      expand = c(0, 0),
      oob = scales::oob_keep,
      labels = if (ylabel == "counts") {
        scales::label_number(accuracy = 1)
      } else {
        ggplot2::waiver()
      }
    ) +
    coord_cartesian(
      ylim = c(min(-ymax * 0.05, ymin * 1.05), ymax * 1.1),
      clip = "off"
    ) +
    theme_classic(base_size = base_size) +
    theme(
      axis.title.y = element_text(size = axis_title_y_cex * base_size),
      axis.text.y = element_text(size = axis_text_y_cex * base_size),
      axis.text.x = element_text(size = axis_text_x_cex * base_size),
      plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
    )

  if (show_axis_title_y) {
    p <- p + ylab(ylabel)
  } else {
    p <- p + ylab(NULL)
  }
  if (!show_axis_text_y) {
    p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }
  p <- p + xlab(NULL)

  # Sample name
  p <- p +
    annotate(
      "text",
      x = 10, y = ymax,
      label = plot_title,
      hjust = 0,
      fontface = "bold",
      size = plot_title_cex * base_mm
    )

  # Legend
  p <- p +
    annotate("rect", xmin = 8, xmax = 8.5,
             ymin = ymax * 0.92, ymax = ymax * 0.97,
             fill = strand_col[1]) +
    annotate("rect", xmin = 8, xmax = 8.5,
             ymin = ymax * 0.84, ymax = ymax * 0.89,
             fill = strand_col[2]) +
    annotate("text", x = 8.7, y = ymax * 0.945,
             label = "Transcribed strand", hjust = 0,
             size = plot_title_cex * base_mm * 0.8) +
    annotate("text", x = 8.7, y = ymax * 0.865,
             label = "Untranscribed strand", hjust = 0,
             size = plot_title_cex * base_mm * 0.8)

  if (!show_axis_text_x) {
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

  return(p)
}
