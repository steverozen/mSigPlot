#' Plot a DBS144 strand-bias catalog using ggplot2
#'
#' Creates a 20-bar chart (10 pairs) from a 144-row DBS catalog, collapsing
#' by dinucleotide class and splitting by transcribed/untranscribed strand.
#' Uses 132 of 144 entries (12 self-complementary types are omitted).
#'
#' @param catalog Numeric vector, single-column data.frame, matrix, tibble,
#'   or data.table.
#' @param plot_title Character. Title displayed above the plot.
#' @param ylabels Logical, draw y-axis labels.
#' @param ylim Optional y-axis limits.
#' @param base_size Numeric. Base font size in points.
#' @param title_cex Numeric. Multiplier for the plot title size.
#' @param x_label_cex Numeric. Multiplier for x-axis labels.
#' @param axis_title_cex Numeric. Multiplier for the y-axis title size.
#' @param axis_text_cex Numeric. Multiplier for the y-axis tick label size.
#'
#' @return A ggplot object.
#'
#' @export
#'
#' @import ggplot2 dplyr
plot_DBS144 <- function(
  catalog,
  plot_title = NULL,
  ylabels = TRUE,
  ylim = NULL,
  base_size = 11,
  title_cex = 0.8,
  x_label_cex = 1.0,
  axis_title_cex = 1.0,
  axis_text_cex = 0.8
) {
  catalog <- normalize_catalog(catalog, 144, catalog_row_order()$DBS144, "DBS144")
  if (is.null(catalog)) return(NULL)
  if (is.null(plot_title)) plot_title <- colnames(catalog)[1] %||% ""

  base_mm <- base_size / (72.27 / 25.4)

  strand_col <- c("#394398", "#e83020")
  xlabel <- c("AC", "AT", "CC", "CG", "CT", "GC", "TA", "TC", "TG", "TT")

  # Reorder for plotting (132 entries in paired strand order)
  reorder <- reorder_DBS144_for_plotting()
  cat_reordered <- catalog[reorder, 1]

  # Detect catalog type
  catalog_type <- attributes(catalog)$catalog.type
  if (is.null(catalog_type)) {
    if (sum(abs(catalog[, 1])) >= 1.1) {
      catalog_type <- "counts"
    } else {
      catalog_type <- "counts.signature"
    }
  }

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
      limits = c(0, ymax),
      expand = c(0, 0),
      oob = scales::oob_keep
    ) +
    coord_cartesian(
      ylim = c(-ymax * 0.05, ymax * 1.1),
      clip = "off"
    ) +
    theme_classic(base_size = base_size) +
    theme(
      axis.title.y = element_text(size = axis_title_cex * base_size),
      axis.text.y = element_text(size = axis_text_cex * base_size),
      axis.text.x = element_text(size = x_label_cex * base_size),
      plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
    )

  if (ylabels) {
    p <- p + ylab(ylabel)
  } else {
    p <- p + ylab(NULL)
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
      size = title_cex * base_mm
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
             size = title_cex * base_mm * 0.8) +
    annotate("text", x = 8.7, y = ymax * 0.865,
             label = "Untranscribed strand", hjust = 0,
             size = title_cex * base_mm * 0.8)

  return(p)
}
