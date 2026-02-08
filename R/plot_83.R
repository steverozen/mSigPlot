#' Plot 83-type indel catalog using ggplot2 and return the ggplot object
#' @param catalog An 83-row, 1 - column indel catalog
#' @param text_size Numeric. Size of plot_title.
#' @param count_label_size Numeric. Size of the labels that show counts of mutations for each major mutation type.
#' @param plot_title Character. Title displayed above the plot.
#' @param grid Logical, draw grid lines
#' @param upper Logical, draw category labels above bars
#' @param xlabels Logical, draw x-axis labels
#' @param ylabels Logical, draw y-axis labels
#' @param ylim Optional y-axis limits
#' @param base_size Base font size for ggplot2's theme.
#' @return A ggplot object
#' @export
#'
#' @import ggplot2 dplyr
plot_83 <- function(
  catalog,
  text_size = 3,
  count_label_size = 0.8 * text_size,
  plot_title = colnames(catalog)[1],
  grid = TRUE,
  upper = TRUE,
  xlabels = TRUE,
  ylabels = TRUE,
  ylim = NULL,
  base_size = 11
) {
  stopifnot(nrow(catalog) == 83)

  # Define colors for 16 indel classes
  indel_class_col <- c(
    "#fdbe6f",
    "#ff8001",
    "#b0dd8b",
    "#36a12e",
    "#fdcab5",
    "#fc8a6a",
    "#f14432",
    "#bc141a",
    "#d0e1f2",
    "#94c4df",
    "#4a98c9",
    "#1764ab",
    "#e2e2ef",
    "#b6b6d8",
    "#8683bd",
    "#61409b"
  )

  # Number of channels per class
  class_sizes <- c(6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 1, 2, 3, 5)

  # Create color vector for all 83 channels
  cols <- rep(indel_class_col, class_sizes)

  # Build data frame
  df <- data.frame(
    x = 1:83,
    value = catalog[, 1],
    color = cols,
    stringsAsFactors = FALSE
  )

  # Determine catalog type and y-axis label
  catalog_type <- attributes(catalog)$catalog.type
  if (is.null(catalog_type)) {
    if (sum(df$value) < 1.1) {
      catalog_type = "counts.signature"
    } else {
      catalog_type <- "counts"
    }
  }

  if (catalog_type == "counts") {
    ymax <- 4 * ceiling(max(max(df$value) * 1.3, 10) / 4)
    ylabel <- "counts"
  } else {
    # if (catalog_type == "counts.signature") {
    ymax <- max(df$value) * 1.3
    ylabel <- "proportion"
  }

  if (!is.null(ylim)) {
    ymax <- ylim[2]
  }

  # Class boundaries (cumulative)
  class_ends <- cumsum(class_sizes)
  class_starts <- c(1, class_ends[-length(class_ends)] + 1)

  # Create blocks for upper category rectangles
  blocks <- data.frame(
    xmin = class_starts - 0.5,
    xmax = class_ends + 0.5,
    ymin = ymax * 1.02,
    ymax = ymax * 1.11,
    fill = indel_class_col,
    stringsAsFactors = FALSE
  )

  # Category labels
  category_lab <- c(rep(c("C", "T"), 2), rep(c("2", "3", "4", "5+"), 3))
  category_col <- c(
    rep(c("black", "white"), 2),
    rep(c("black", "black", "black", "white"), 3)
  )
  blocks$label <- category_lab
  blocks$label_col <- category_col

  # Major class labels
  maj_class_names <- c(
    "1bp deletion",
    "1bp insertion",
    ">1bp deletions at repeats\n(Deletion length)",
    ">1bp insertions at repeats\n(Insertion length)",
    "Deletions with microhomology\n(Deletion length)"
  )

  # Positions for major class labels
  maj_class_pos <- c(
    (blocks$xmin[1] + blocks$xmax[2]) / 2,
    (blocks$xmin[3] + blocks$xmax[4]) / 2,
    (blocks$xmin[5] + blocks$xmax[8]) / 2,
    (blocks$xmin[9] + blocks$xmax[12]) / 2,
    (blocks$xmin[13] + blocks$xmax[16]) / 2
  )

  maj_labels <- data.frame(
    x = maj_class_pos,
    y = ymax * 1.27,
    label = maj_class_names,
    stringsAsFactors = FALSE
  )

  # X-axis labels (mutation types)
  mut_type <- c(
    rep(c("1", "2", "3", "4", "5", "6+"), 2),
    rep(c("0", "1", "2", "3", "4", "5+"), 2),
    rep(c("1", "2", "3", "4", "5", "6+"), 4),
    rep(c("0", "1", "2", "3", "4", "5+"), 4),
    "1",
    "1",
    "2",
    "1",
    "2",
    "3",
    "1",
    "2",
    "3",
    "4",
    "5+"
  )

  # Bottom category labels
  bottom_pos <- c(
    (blocks$xmin[1] + blocks$xmax[2]) / 2,
    (blocks$xmin[3] + blocks$xmax[4]) / 2,
    (blocks$xmin[5] + blocks$xmax[8]) / 2,
    (blocks$xmin[9] + blocks$xmax[12]) / 2,
    (blocks$xmin[13] + blocks$xmax[16]) / 2
  )
  bottom_lab <- c(
    "Homopolymer length",
    "Homopolymer length",
    "Number of repeat units",
    "Number of repeat units",
    "Microhomology length"
  )

  bottom_labels <- data.frame(
    x = bottom_pos,
    y = -ymax * 0.27,
    label = bottom_lab,
    stringsAsFactors = FALSE
  )

  # Blocks for bottom x-axis color strip
  bottom_blocks <- data.frame(
    xmin = class_starts - 0.5,
    xmax = class_ends + 0.5,
    ymin = -ymax * 0.09,
    ymax = -ymax * 0.01,
    fill = indel_class_col,
    stringsAsFactors = FALSE
  )

  # Calculate counts per class for annotation
  if (catalog_type == "counts") {
    counts_per_class <- numeric(16)
    for (i in 1:16) {
      if (i == 1) {
        counts_per_class[i] <- sum(abs(df$value[1:class_ends[1]]))
      } else {
        counts_per_class[i] <- sum(abs(df$value[
          (class_ends[i - 1] + 1):class_ends[i]
        ]))
      }
    }
    count_labels <- data.frame(
      x = (class_starts + class_ends) / 2,
      y = ymax * 0.8,
      label = round(counts_per_class),
      stringsAsFactors = FALSE
    )
  }

  # Build plot
  p <- ggplot(df, aes(x = x, y = value)) +
    geom_bar(stat = "identity", fill = df$color, width = 0.8) +
    scale_x_continuous(
      limits = c(0.5, 83.5),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(-ymax * 0.3, ymax * 1.35),
      breaks = function(x) {
        b <- scales::extended_breaks()(c(0, ymax))
        b[b >= 0]
      },
      expand = c(0, 0)
    ) +
    coord_cartesian(ylim = c(-ymax * 0.3, ymax * 1.35), clip = "off") +
    theme_classic(base_size = base_size) +
    theme(
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.margin = margin(t = 40, r = 10, b = 50, l = 10)
    )

  # Add y-axis label
  if (ylabels) {
    p <- p + ylab(ylabel)
  } else {
    p <- p + ylab(NULL)
  }

  # Add x-axis label
  p <- p + xlab(NULL)

  # Add grid lines that match the y-axis ticks
  if (grid) {
    # Get the y-axis breaks that ggplot2 will use
    y_breaks <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y$breaks
    y_breaks <- y_breaks[!is.na(y_breaks) & y_breaks >= 0 & y_breaks <= ymax]

    p <- p +
      geom_hline(
        yintercept = y_breaks,
        color = "grey60",
        linewidth = 0.25
      ) +
      annotate(
        "rect",
        xmin = 0.5,
        xmax = 83.5,
        ymin = 0,
        ymax = ymax,
        fill = NA,
        color = "grey60",
        linewidth = 0.5
      )
  }

  # Add upper category blocks and labels
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
        size = 2.5 * base_size / 11,
        inherit.aes = FALSE
      ) +
      geom_text(
        data = maj_labels,
        aes(x = x, y = y, label = label),
        size = 3 * base_size / 11,
        inherit.aes = FALSE
      )
  }

  # Add x-axis labels
  if (xlabels) {
    p <- p +
      geom_rect(
        data = bottom_blocks,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        fill = bottom_blocks$fill,
        inherit.aes = FALSE
      ) +
      geom_text(
        aes(x = x, y = -ymax * 0.15, label = mut_type),
        size = 2 * base_size / 11,
        inherit.aes = FALSE,
        data = data.frame(x = 1:83, mut_type = mut_type)
      ) +
      geom_text(
        data = bottom_labels,
        aes(x = x, y = y, label = label),
        size = 2.5 * base_size / 11,
        inherit.aes = FALSE
      ) +
      theme(axis.text.x = element_blank())
  } else {
    p <- p + theme(axis.text.x = element_blank())
  }

  # Add count labels for counts catalog
  if (catalog_type == "counts") {
    p <- p +
      geom_text(
        data = count_labels,
        aes(x = x, y = y, label = label),
        size = count_label_size,
        hjust = 1,
        inherit.aes = FALSE
      )
  }

  # Add sample name
  p <- p +
    annotate(
      "text",
      x = 1.5,
      y = ymax * 7.4 / 8,
      label = plot_title,
      hjust = 0,
      # fontface = "bold",
      size = text_size
    )

  return(p)
}
