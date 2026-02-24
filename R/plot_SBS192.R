#' Plot a full SBS192 catalog using ggplot2
#'
#' Creates a 192-bar chart showing single base substitutions on transcribed
#' and untranscribed strands. Bars are paired (transcribed/untranscribed)
#' side-by-side, with 6 colored class backgrounds.
#'
#' @param catalog Numeric vector, single-column data.frame, matrix, tibble,
#'   or data.table.
#' @param plot_title Character. Title displayed above the plot.
#' @param grid Logical, draw grid lines.
#' @param upper Logical, draw colored class rectangles and labels above bars.
#' @param xlabels Logical, draw x-axis context labels.
#' @param ylabels Logical, draw y-axis labels.
#' @param ylim Optional y-axis limits.
#' @param base_size Numeric. Base font size in points.
#' @param title_cex Numeric. Multiplier for the plot title size.
#' @param count_label_cex Numeric. Multiplier for per-class count labels.
#' @param class_label_cex Numeric. Multiplier for major class labels.
#' @param x_label_cex Numeric. Multiplier for x-axis labels.
#' @param axis_title_cex Numeric. Multiplier for the y-axis title size.
#' @param axis_text_cex Numeric. Multiplier for the y-axis tick label size.
#' @param show_counts Logical or NULL. Auto-detect if NULL.
#'
#' @return A ggplot object.
#'
#' @export
#'
#' @import ggplot2 dplyr
plot_SBS192 <- function(
  catalog,
  plot_title = NULL,
  grid = TRUE,
  upper = TRUE,
  xlabels = TRUE,
  ylabels = TRUE,
  ylim = NULL,
  base_size = 11,
  title_cex = 0.8,
  count_label_cex = 0.9,
  class_label_cex = 0.9,
  x_label_cex = 0.5,
  axis_title_cex = 1.0,
  axis_text_cex = 0.8,
  show_counts = NULL
) {
  catalog <- normalize_catalog(catalog, 192, catalog_row_order()$SBS192, "SBS192")
  if (is.null(catalog)) return(NULL)
  if (is.null(plot_title)) plot_title <- colnames(catalog)[1] %||% ""

  base_mm <- base_size / (72.27 / 25.4)

  # Class colors and background colors
  class_col <- c("#03bcee", "#010101", "#e32926", "#999999", "#a1ce63", "#ebc6c4")
  bg_class_col <- c("#DCF8FF", "#E9E9E9", "#FFC7C7", "#F7F7F7", "#E5F9DF", "#F9E7E7")
  strand_col <- c("#394398", "#e83020")  # transcribed, untranscribed

  maj_class_names <- c("C>A", "C>G", "C>T", "T>A", "T>C", "T>G")

  # Reorder for plotting (pairs of transcribed/untranscribed)
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

  values <- cat_reordered
  if (catalog_type == "density") {
    values <- values * 1e6
    ylabel <- "mut/million"
    ymax <- max(values) * 1.3
  } else if (catalog_type == "counts") {
    ymax <- 4 * ceiling(max(max(values) * 1.3, 10) / 4)
    ylabel <- "counts"
  } else {
    ylabel <- ifelse(catalog_type == "counts.signature",
                     "counts proportion", "density proportion")
    ymax <- min(max(values) * 1.3, 1)
  }

  if (!is.null(ylim)) {
    ymax <- ylim[2]
  }

  # Build data frame: 192 bars
  df <- data.frame(
    x = 1:192,
    value = values,
    strand = rep(c("Transcribed", "Untranscribed"), 96),
    stringsAsFactors = FALSE
  )
  bar_colors <- rep(strand_col, 96)

  # Class boundaries: 6 classes, 32 bars each
  class_starts <- seq(1, 161, by = 32)
  class_ends <- seq(32, 192, by = 32)

  # Background rectangles
  bg_rects <- data.frame(
    xmin = class_starts - 0.5,
    xmax = class_ends + 0.5,
    ymin = 0,
    ymax = ymax,
    fill = bg_class_col,
    stringsAsFactors = FALSE
  )

  # Upper class color strips
  upper_rects <- data.frame(
    xmin = class_starts - 0.5,
    xmax = class_ends + 0.5,
    ymin = ymax * 1.01,
    ymax = ymax * 1.03,
    fill = class_col,
    stringsAsFactors = FALSE
  )

  # Resolve show_counts
  if (is.null(show_counts)) {
    show_counts <- (catalog_type == "counts")
  }

  # Count labels per class
  if (show_counts) {
    counts_per_class <- numeric(6)
    for (i in 1:6) {
      counts_per_class[i] <- sum(abs(df$value[class_starts[i]:class_ends[i]]))
    }
    count_labels <- data.frame(
      x = class_ends,
      y = ymax * 0.84,
      label = round(counts_per_class),
      stringsAsFactors = FALSE
    )
  }

  # Build plot
  p <- ggplot(df, aes(x = x, y = value)) +
    # Background colored panels
    geom_rect(
      data = bg_rects,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = bg_rects$fill, color = "grey90", linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    # Bars
    geom_bar(stat = "identity", fill = bar_colors, width = 0.8) +
    scale_x_continuous(
      limits = c(0, 193),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0, ymax),
      expand = c(0, 0)
    ) +
    coord_cartesian(
      ylim = c(
        if (xlabels) -ymax * 0.15 else 0,
        ymax * (if (upper) 1.15 else 1.05)
      ),
      clip = "off"
    ) +
    theme_classic(base_size = base_size) +
    theme(
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = axis_title_cex * base_size),
      axis.text.y = element_text(size = axis_text_cex * base_size),
      plot.margin = margin(
        t = if (upper) 25 * base_size / 11 else 10,
        r = 10,
        b = if (xlabels) 25 * base_size / 11 else 10,
        l = 10
      )
    )

  if (ylabels) {
    p <- p + ylab(ylabel)
  } else {
    p <- p + ylab(NULL) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }

  # Grid lines
  if (grid) {
    y_breaks <- seq(0, ymax, ymax / 4)
    p <- p +
      geom_hline(yintercept = y_breaks, color = "grey35", linewidth = 0.25)
  }

  # Upper class strips and labels
  if (upper) {
    p <- p +
      geom_rect(
        data = upper_rects,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        fill = upper_rects$fill,
        inherit.aes = FALSE
      ) +
      geom_text(
        data = data.frame(
          x = (class_starts + class_ends) / 2,
          y = ymax * 1.07,
          label = maj_class_names
        ),
        aes(x = x, y = y, label = label),
        size = class_label_cex * base_mm,
        inherit.aes = FALSE
      )
  }

  # X-axis context labels (3 rows: 3' base, ref base, 5' base)
  if (xlabels) {
    # Each pair of bars = one trinucleotide context
    pair_x <- (seq(1, 191, by = 2) + seq(2, 192, by = 2)) / 2

    # The reordered row names give us context info
    # Row names are 4-char: [5'][ref][3'][alt] for SBS96-like
    pair_names <- reorder[seq(1, 191, by = 2)]

    # Extract 3' base (position 1), ref (position 2?), and labels
    # In the ICAMS SBS192 encoding: position 1 = 5' base, 2 = ref, 3 = 3' base, 4 = alt
    # But for SBS192, the first 96 entries are transcribed strand
    # The labels show: line 1 = 3' base, line 2 = ref, line 3 = 5' base
    label_3prime <- c("A", "C", "G", "T")
    label_ref <- rep(c("C", "T"), each = 48)
    label_5prime <- rep(c("A", "C", "G", "T"), each = 4)

    # Row 1: last base in trinucleotide (3' context)
    p <- p +
      geom_text(
        data = data.frame(x = pair_x, y = -ymax * 0.01,
                          label = rep(label_3prime, 24)),
        aes(x = x, y = y, label = label),
        size = x_label_cex * base_mm, angle = 90, hjust = 1,
        inherit.aes = FALSE
      )

    # Row 2: reference base
    p <- p +
      geom_text(
        data = data.frame(x = pair_x, y = -ymax * 0.06,
                          label = label_ref),
        aes(x = x, y = y, label = label),
        size = x_label_cex * base_mm, angle = 90, hjust = 1,
        inherit.aes = FALSE
      )

    # Row 3: 5' base
    p <- p +
      geom_text(
        data = data.frame(x = pair_x, y = -ymax * 0.11,
                          label = rep(rep(label_5prime, 6), each = 1)),
        aes(x = x, y = y, label = label),
        size = x_label_cex * base_mm, angle = 90, hjust = 1,
        inherit.aes = FALSE
      )
  }

  # Count labels
  if (show_counts) {
    p <- p +
      geom_text(
        data = count_labels,
        aes(x = x, y = y, label = label),
        size = count_label_cex * base_mm,
        hjust = 1,
        inherit.aes = FALSE
      )
  }

  # Sample name
  p <- p +
    annotate(
      "text",
      x = 1.5,
      y = if (catalog_type == "counts") ymax * 7.4 / 8 else ymax * 7 / 8,
      label = plot_title,
      hjust = 0,
      fontface = "bold",
      size = title_cex * base_mm
    )

  # Legend for strand colors
  legend_df <- data.frame(
    x = c(160, 160),
    y = c(ymax * 1.05, ymax * 0.98),
    label = c("Transcribed strand", "Untranscribed strand"),
    fill = strand_col,
    stringsAsFactors = FALSE
  )
  p <- p +
    annotate("rect", xmin = 155, xmax = 158,
             ymin = ymax * 1.035, ymax = ymax * 1.065,
             fill = strand_col[1]) +
    annotate("rect", xmin = 155, xmax = 158,
             ymin = ymax * 0.965, ymax = ymax * 0.995,
             fill = strand_col[2]) +
    annotate("text", x = 159, y = ymax * 1.05,
             label = "Transcribed strand", hjust = 0,
             size = title_cex * base_mm * 0.8) +
    annotate("text", x = 159, y = ymax * 0.98,
             label = "Untranscribed strand", hjust = 0,
             size = title_cex * base_mm * 0.8)

  return(p)
}
