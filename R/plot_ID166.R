#' Plot an ID166 genic/intergenic indel catalog using ggplot2
#'
#' Creates a paired barplot showing 83 indel categories split into
#' genic (black) and intergenic (grey) regions. Rows 1-83 are genic,
#' rows 84-166 are intergenic.
#'
#' @param catalog Numeric vector, single-column data.frame, matrix, tibble,
#'   or data.table.
#' @param plot_title Character. Title displayed above the plot.
#' @param grid Logical, draw background color wash and grid lines.
#' @param upper Logical, draw category labels above bars.
#' @param xlabels Logical, draw x-axis labels.
#' @param ylabels Logical, draw y-axis labels.
#' @param ylim Optional y-axis limits.
#' @param base_size Numeric. Base font size in points.
#' @param plot_title_cex Numeric. Multiplier for the plot title size.
#' @param count_label_cex Numeric. Multiplier for per-class count labels.
#' @param block_label_cex Numeric. Multiplier for upper category block labels.
#' @param class_label_cex Numeric. Multiplier for major class labels.
#' @param axis_text_x_cex Numeric. Multiplier for x-axis labels.
#' @param bottom_label_cex Numeric. Multiplier for bottom category labels.
#' @param axis_title_x_cex Numeric. Multiplier for the x-axis title size. Currently has no effect in this function.
#' @param axis_title_y_cex Numeric. Multiplier for the y-axis title size.
#' @param axis_text_y_cex Numeric. Multiplier for the y-axis tick label size.
#' @param show_counts Logical or NULL. Auto-detect if NULL.
#'
#' @return A ggplot object.
#'
#' @examples
#' set.seed(1)
#' sig <- runif(166)
#' sig <- sig / sum(sig)
#' names(sig) <- catalog_row_order()$ID166
#' plot_ID166(sig, plot_title = "Example ID166 signature")
#'
#' @export
#'
#' @import ggplot2 dplyr
plot_ID166 <- function(
  catalog,
  plot_title = NULL,
  grid = TRUE,
  upper = TRUE,
  xlabels = TRUE,
  ylabels = TRUE,
  ylim = NULL,
  base_size = 11,
  plot_title_cex = 0.8,
  count_label_cex = 0.9,
  block_label_cex = 0.65,
  class_label_cex = 0.8,
  axis_text_x_cex = 0.5,
  bottom_label_cex = 0.65,
  axis_title_x_cex = 1.0,
  axis_title_y_cex = 1.0,
  axis_text_y_cex = 0.8,
  show_counts = NULL
) {
  catalog <- normalize_catalog(catalog, 166, catalog_row_order()$ID166, "ID166")
  if (is.null(catalog)) return(NULL)
  if (is.null(plot_title)) plot_title <- colnames(catalog)[1] %||% ""

  base_mm <- base_size / (72.27 / 25.4)

  # 16 indel class colors (same as ID83)
  indel_class_col <- c(
    "#fdbe6f", "#ff8001", "#b0dd8b", "#36a12e",
    "#fdcab5", "#fc8a6a", "#f14432", "#bc141a",
    "#d0e1f2", "#94c4df", "#4a98c9", "#1764ab",
    "#e2e2ef", "#b6b6d8", "#8683bd", "#61409b"
  )

  # 90% transparent versions for background
  bg_col <- sapply(indel_class_col, function(hex) {
    rgb_val <- grDevices::col2rgb(hex)
    grDevices::rgb(rgb_val[1], rgb_val[2], rgb_val[3],
                   maxColorValue = 255, alpha = 25)
  })

  region_col <- c("black", "grey")
  class_sizes <- c(6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 1, 2, 3, 5)

  # Detect catalog type
  catalog_type <- attributes(catalog)$catalog.type
  if (is.null(catalog_type)) {
    if (sum(abs(catalog[, 1])) >= 1.1) {
      catalog_type <- "counts"
    } else {
      catalog_type <- "counts.signature"
    }
  }

  # Genic = rows 1-83, Intergenic = rows 84-166
  genic <- catalog[1:83, 1]
  intergenic <- catalog[84:166, 1]

  # Build data frame: 166 bars as 83 side-by-side pairs
  # x positions: each pair gets positions (2*i-1, 2*i)
  df <- data.frame(
    x = 1:166,
    value = c(rbind(genic, intergenic)),
    region = rep(c("Genic", "Intergenic"), 83),
    stringsAsFactors = FALSE
  )
  bar_colors <- rep(region_col, 83)

  if (catalog_type == "counts") {
    ymax <- 4 * ceiling(max(max(df$value) * 1.45, 10) / 4)
    ylabel <- "counts"
  } else {
    ymax <- max(max(df$value) * 1.45, 0.01)
    ylabel <- "counts proportion"
  }

  if (!is.null(ylim)) {
    ymax <- ylim[2]
  }
  ymin <- min(0, min(df$value))

  # Class boundaries on the 166-bar scale
  class_ends_83 <- cumsum(class_sizes)
  class_starts_83 <- c(1, class_ends_83[-length(class_ends_83)] + 1)
  class_starts_166 <- 2 * class_starts_83 - 1
  class_ends_166 <- 2 * class_ends_83

  # Background wash rectangles
  bg_rects <- data.frame(
    xmin = class_starts_166 - 0.5,
    xmax = class_ends_166 + 0.5,
    ymin = 0,
    ymax = ymax * 0.9,
    fill = bg_col,
    stringsAsFactors = FALSE
  )

  # Upper rectangles
  blocks <- data.frame(
    xmin = class_starts_166 - 0.5,
    xmax = class_ends_166 + 0.5,
    ymin = ymax * 1.02,
    ymax = ymax * 1.11,
    fill = indel_class_col,
    stringsAsFactors = FALSE
  )

  category_lab <- c(rep(c("C", "T"), 2), rep(c("2", "3", "4", "5+"), 3))
  category_col <- c(rep(c("black", "white"), 2),
                     rep(c("black", "black", "black", "white"), 3))
  blocks$label <- category_lab
  blocks$label_col <- category_col

  maj_class_names <- c(
    "1bp deletion", "1bp insertion",
    ">1bp deletions at repeats\n(Deletion length)",
    ">1bp insertions at repeats\n(Insertion length)",
    "Deletions with microhomology\n(Deletion length)"
  )

  maj_class_pos <- c(
    (blocks$xmin[1] + blocks$xmax[2]) / 2,
    (blocks$xmin[3] + blocks$xmax[4]) / 2,
    (blocks$xmin[5] + blocks$xmax[8]) / 2,
    (blocks$xmin[9] + blocks$xmax[12]) / 2,
    (blocks$xmin[13] + blocks$xmax[16]) / 2
  )

  # Resolve show_counts
  if (is.null(show_counts)) {
    show_counts <- (catalog_type == "counts")
  }

  # Count labels per class
  if (show_counts) {
    counts_per_class <- numeric(16)
    for (i in 1:16) {
      s <- class_starts_166[i]
      e <- class_ends_166[i]
      counts_per_class[i] <- sum(abs(df$value[s:e]))
    }
    count_labels <- data.frame(
      x = (class_starts_166 + class_ends_166) / 2,
      y = ymax * 0.73,
      label = ifelse(counts_per_class > 0 & counts_per_class < 1,
                     signif(counts_per_class, 2), round(counts_per_class)),
      stringsAsFactors = FALSE
    )
  }

  # X-axis labels
  mut_type <- c(
    rep(c("1", "2", "3", "4", "5", "6+"), 2),
    rep(c("0", "1", "2", "3", "4", "5+"), 2),
    rep(c("1", "2", "3", "4", "5", "6+"), 4),
    rep(c("0", "1", "2", "3", "4", "5+"), 4),
    "1", "1", "2", "1", "2", "3", "1", "2", "3", "4", "5+"
  )

  bottom_pos <- c(
    (blocks$xmin[1] + blocks$xmax[2]) / 2,
    (blocks$xmin[3] + blocks$xmax[4]) / 2,
    (blocks$xmin[5] + blocks$xmax[8]) / 2,
    (blocks$xmin[9] + blocks$xmax[12]) / 2,
    (blocks$xmin[13] + blocks$xmax[16]) / 2
  )
  bottom_lab <- c(
    "Homopolymer length", "Homopolymer length",
    "Number of repeat units", "Number of repeat units",
    "Microhomology length"
  )

  # Bottom color blocks
  bottom_blocks <- data.frame(
    xmin = class_starts_166 - 0.5,
    xmax = class_ends_166 + 0.5,
    ymin = -ymax * 0.09,
    ymax = -ymax * 0.01,
    fill = indel_class_col,
    stringsAsFactors = FALSE
  )

  # Build plot
  p <- ggplot(df, aes(x = x, y = value)) +
    scale_x_continuous(limits = c(0, 167), expand = c(0, 0)) +
    scale_y_continuous(limits = c(min(0, ymin * 1.05), ymax * 0.9), expand = c(0, 0),
                       oob = scales::oob_keep,
                       labels = if (ylabel == "counts") {
                         scales::label_number(accuracy = 1)
                       } else {
                         ggplot2::waiver()
                       }) +
    coord_cartesian(
      ylim = c(
        min(if (xlabels) -ymax * 0.3 else 0, ymin * 1.05),
        ymax * (if (upper) 1.35 else 1.05)
      ),
      clip = "off"
    ) +
    theme_classic(base_size = base_size) +
    theme(
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = axis_title_y_cex * base_size),
      axis.text.y = element_text(size = axis_text_y_cex * base_size),
      plot.margin = margin(
        t = if (upper) 40 * base_size / 11 else 10,
        r = 10,
        b = if (xlabels) 50 * base_size / 11 else 10,
        l = 10
      )
    )

  # Background color wash
  if (grid) {
    p <- p +
      geom_rect(
        data = bg_rects,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        fill = bg_rects$fill, color = "grey90", linewidth = 0.5,
        inherit.aes = FALSE
      )

    grid_y <- seq(0, ymax * 0.9, ymax * 0.9 / 4)[1:4]
    p <- p +
      geom_hline(yintercept = grid_y, color = "grey60", linewidth = 0.25)
  }

  # Bars (drawn after background)
  p <- p +
    geom_bar(stat = "identity", fill = bar_colors, width = 0.8)

  if (ylabels) {
    p <- p + ylab(ylabel)
  } else {
    p <- p + ylab(NULL) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }

  # Upper blocks and labels
  if (upper) {
    p <- p +
      geom_rect(
        data = blocks,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        fill = blocks$fill,
        inherit.aes = FALSE
      ) +
      geom_text(
        data = blocks,
        aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = label),
        color = blocks$label_col,
        size = block_label_cex * base_mm,
        inherit.aes = FALSE
      ) +
      geom_text(
        data = data.frame(
          x = maj_class_pos,
          y = ymax * 1.27,
          label = maj_class_names
        ),
        aes(x = x, y = y, label = label),
        size = class_label_cex * base_mm,
        inherit.aes = FALSE
      )
  }

  # X-axis labels
  if (xlabels) {
    # Bottom color strip
    p <- p +
      geom_rect(
        data = bottom_blocks,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        fill = bottom_blocks$fill,
        inherit.aes = FALSE
      )

    # Channel labels (at center of each pair)
    pair_centers <- seq(1.5, 165.5, by = 2)
    p <- p +
      geom_text(
        data = data.frame(x = pair_centers, y = -ymax * 0.15,
                          label = mut_type),
        aes(x = x, y = y, label = label),
        size = axis_text_x_cex * base_mm,
        inherit.aes = FALSE
      ) +
      geom_text(
        data = data.frame(x = bottom_pos, y = -ymax * 0.27,
                          label = bottom_lab),
        aes(x = x, y = y, label = label),
        size = bottom_label_cex * base_mm,
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
        hjust = 0.5,
        inherit.aes = FALSE
      )
  }

  # Sample name
  p <- p +
    annotate(
      "text",
      x = 1.5,
      y = ymax * 7.4 / 8,
      label = plot_title,
      hjust = 0,
      fontface = "bold",
      size = plot_title_cex * base_mm
    )

  # Legend
  p <- p +
    annotate("rect", xmin = 135, xmax = 139,
             ymin = ymax * 1.04, ymax = ymax * 1.08,
             fill = region_col[1]) +
    annotate("rect", xmin = 135, xmax = 139,
             ymin = ymax * 0.96, ymax = ymax * 1.0,
             fill = region_col[2]) +
    annotate("text", x = 140, y = ymax * 1.06,
             label = "Genic region", hjust = 0,
             size = plot_title_cex * base_mm * 0.8) +
    annotate("text", x = 140, y = ymax * 0.98,
             label = "Intergenic region", hjust = 0,
             size = plot_title_cex * base_mm * 0.8)

  return(p)
}
