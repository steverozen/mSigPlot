#' Plot a condensed SBS12 strand bias chart using ggplot2
#'
#' Creates a 12-bar chart from a 192-row SBS catalog, collapsing the 96
#' trinucleotide contexts into 6 mutation classes, each split by
#' transcribed/untranscribed strand. Optionally performs a binomial strand
#' bias test and annotates significant results with asterisks.
#'
#' @param catalog Numeric vector, single-column data.frame, matrix, tibble,
#'   or data.table.
#' @param plot_title Character. Title displayed above the plot.
#' @param abundance Optional named numeric vector of 64 3-mer counts. If
#'   provided and the catalog is counts, a two-sided binomial test is
#'   performed for strand bias.
#' @param ylabels Logical, draw y-axis labels.
#' @param ylim Optional y-axis limits.
#' @param base_size Numeric. Base font size in points.
#' @param title_cex Numeric. Multiplier for the plot title size.
#' @param x_label_cex Numeric. Multiplier for x-axis labels.
#' @param axis_title_cex Numeric. Multiplier for the y-axis title size.
#' @param axis_text_cex Numeric. Multiplier for the y-axis tick label size.
#'
#' @return A ggplot object. If a binomial test was performed, the object has
#'   an attribute `"strand.bias.statistics"` containing a data frame of
#'   transcribed/untranscribed counts and q-values.
#'
#' @export
#'
#' @import ggplot2 dplyr
#' @importFrom stats binom.test p.adjust
plot_SBS12 <- function(
  catalog,
  plot_title = NULL,
  abundance = NULL,
  ylabels = TRUE,
  ylim = NULL,
  base_size = 11,
  title_cex = 0.8,
  x_label_cex = 1.0,
  axis_title_cex = 1.0,
  axis_text_cex = 0.8
) {
  catalog <- normalize_catalog(catalog, 192, catalog_row_order()$SBS192, "SBS192")
  if (is.null(catalog)) return(NULL)
  if (is.null(plot_title)) plot_title <- colnames(catalog)[1] %||% ""

  base_mm <- base_size / (72.27 / 25.4)

  strand_col <- c("#394398", "#e83020")
  maj_class_names <- c("C>A", "C>G", "C>T", "T>A", "T>C", "T>G")

  # Reorder catalog for paired strand display
  reorder <- reorder_SBS192_for_plotting()
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

  # Collapse 192 bars into 12 (6 classes x 2 strands)
  # In the reordered data: odd = transcribed, even = untranscribed
  # Each class has 32 bars (16 pairs)
  counts_strand <- numeric(12)
  for (i in 1:6) {
    offset <- 32 * (i - 1)
    counts_strand[2 * i - 1] <-
      sum(cat_reordered[seq(offset + 1, offset + 32, by = 2)])
    counts_strand[2 * i] <-
      sum(cat_reordered[seq(offset + 2, offset + 32, by = 2)])
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

  # Build data frame: 12 bars (6 pairs)
  df <- data.frame(
    class = rep(maj_class_names, each = 2),
    strand = rep(c("Transcribed", "Untranscribed"), 6),
    value = counts_strand,
    x = 1:12,
    stringsAsFactors = FALSE
  )
  df$class <- factor(df$class, levels = maj_class_names)

  bar_colors <- rep(strand_col, 6)

  # Binomial test for strand bias
  strand_bias <- NULL
  asterisks <- rep("", 6)

  if (catalog_type == "counts" && !is.null(abundance) && length(abundance) == 64) {
    # Calculate base counts from 3-mer abundance
    # Sum abundance by middle base
    nms <- names(abundance)
    mid <- substr(nms, 2, 2)
    base_counts <- tapply(abundance, mid, sum)

    # Proportion of pyrimidines on transcribed strand
    prop_C <- base_counts["G"] / (base_counts["C"] + base_counts["G"])
    prop_T <- base_counts["A"] / (base_counts["T"] + base_counts["A"])
    props <- c(rep(prop_C, 3), rep(prop_T, 3))
    names(props) <- maj_class_names

    # Binomial tests
    mat <- matrix(counts_strand, nrow = 2, ncol = 6)
    colnames(mat) <- maj_class_names
    rownames(mat) <- c("transcribed", "untranscribed")

    p_values <- numeric(6)
    names(p_values) <- maj_class_names
    for (type in maj_class_names) {
      htest <- stats::binom.test(x = mat[, type], p = props[type],
                                 alternative = "two.sided")
      p_values[type] <- htest$p.value
    }

    q_values <- stats::p.adjust(p_values, method = "BH")
    strand_bias <- as.data.frame(t(mat))
    strand_bias$q.values <- q_values

    # Assign asterisks
    for (k in 1:6) {
      q <- q_values[k]
      if (q < 0.001) {
        asterisks[k] <- "***"
      } else if (q < 0.01) {
        asterisks[k] <- "**"
      } else if (q < 0.05) {
        asterisks[k] <- "*"
      }
    }
  }

  # Build plot
  p <- ggplot(df, aes(x = x, y = value)) +
    geom_bar(stat = "identity", fill = bar_colors, width = 0.7) +
    scale_x_continuous(
      breaks = seq(1.5, 11.5, by = 2),
      labels = maj_class_names,
      limits = c(0, 13),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0, ymax),
      expand = c(0, 0)
    ) +
    coord_cartesian(
      ylim = c(-ymax * 0.05, ymax * 1.15),
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

  # Add asterisks for significant strand bias
  for (k in 1:6) {
    if (asterisks[k] != "") {
      pair_x <- c(2 * k - 1, 2 * k)
      pair_max <- max(df$value[pair_x])
      seg_y <- pair_max + ymax * 0.03
      star_y <- seg_y + ymax * 0.025

      p <- p +
        annotate("segment",
                 x = pair_x[1], xend = pair_x[2],
                 y = seg_y, yend = seg_y) +
        annotate("text",
                 x = mean(pair_x), y = star_y,
                 label = asterisks[k],
                 size = base_mm)
    }
  }

  # Sample name
  p <- p +
    annotate(
      "text",
      x = 0.5, y = ymax * 1.02,
      label = plot_title,
      hjust = 0,
      fontface = "bold",
      size = title_cex * base_mm
    )

  # Legend
  p <- p +
    annotate("rect", xmin = 7, xmax = 7.5,
             ymin = ymax * 0.92, ymax = ymax * 0.97,
             fill = strand_col[1]) +
    annotate("rect", xmin = 7, xmax = 7.5,
             ymin = ymax * 0.84, ymax = ymax * 0.89,
             fill = strand_col[2]) +
    annotate("text", x = 7.7, y = ymax * 0.945,
             label = "Transcribed strand", hjust = 0,
             size = title_cex * base_mm * 0.8) +
    annotate("text", x = 7.7, y = ymax * 0.865,
             label = "Untranscribed strand", hjust = 0,
             size = title_cex * base_mm * 0.8)

  # Attach strand bias statistics as attribute if computed
  if (!is.null(strand_bias)) {
    attr(p, "strand.bias.statistics") <- strand_bias
  }

  return(p)
}
