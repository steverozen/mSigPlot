#' @rdname bar_plots
#' @examples
#' # Plot a random SBS96 signature (proportions summing to 1)
#' set.seed(1)
#' sig <- runif(96)
#' sig <- sig / sum(sig)
#' names(sig) <- catalog_row_order()$SBS96
#' plot_SBS96(sig, plot_title = "Example SBS96 signature")
#'
#' @export
#'
#' @import ggplot2 dplyr
plot_SBS96 <- function(
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
  class_label_cex = 1.1,
  axis_text_x_cex = 0.7,
  axis_title_x_cex = 1.0,
  axis_title_y_cex = 1.0,
  axis_text_y_cex = 0.8,
  show_counts = NULL
) {
  # If row names are in stapled format like A[C>A]T, convert to ACTA
  rn <- if (is.data.frame(catalog) || is.matrix(catalog)) {
    rownames(catalog)
  } else if (is.numeric(catalog)) {
    names(catalog)
  } else {
    NULL
  }
  if (!is.null(rn) && all(grepl("^[ACGT]\\[[CT]>[ACGT]\\][ACGT]$", rn))) {
    converted <- unstaple_SBS96_rownames(rn)
    if (is.data.frame(catalog) || is.matrix(catalog)) {
      rownames(catalog) <- converted
    } else {
      names(catalog) <- converted
    }
  }

  catalog <- normalize_catalog(catalog, 96, catalog_row_order()$SBS96, "SBS96")
  if (is.null(catalog)) return(NULL)
  if (is.null(plot_title)) plot_title <- colnames(catalog)[1] %||% ""

  base_mm <- base_size / (72.27 / 25.4)

  # 6 SBS class colors
  class_col <- c(
    "#0000ff",   # C>A
    "#000000",   # C>G
    "#ff4040",   # C>T
    "#838383",   # T>A
    "#40ff40",   # T>C
    "#ff667f"    # T>G
  )
  maj_class_names <- c("C>A", "C>G", "C>T", "T>A", "T>C", "T>G")

  # 16 bars per class
  cols <- rep(class_col, each = 16)

  df <- data.frame(
    x = 1:96,
    value = catalog[, 1],
    stringsAsFactors = FALSE
  )

  # Detect catalog type
  catalog_type <- attributes(catalog)$catalog.type
  if (is.null(catalog_type)) {
    if (sum(df$value) >= 1.1) {
      catalog_type <- "counts"
    } else {
      catalog_type <- "counts.signature"
    }
  }

  if (catalog_type == "density") {
    ylabel <- "mut/million"
    df$value <- df$value * 1e6
    ymax <- max(df$value) * 1.3
  } else if (catalog_type == "counts") {
    ymax <- 4 * ceiling(max(max(df$value), 10) / 4)
    ylabel <- "counts"
  } else {
    # counts.signature or density.signature
    ymax <- max(df$value) * 1.3
    ylabel <- ifelse(catalog_type == "counts.signature",
                     "counts proportion", "density proportion")
  }

  if (!is.null(ylim)) {
    ymax <- ylim[2]
  }
  ymin <- min(0, min(df$value))

  # Class boundaries
  class_starts <- seq(1, 81, by = 16)
  class_ends <- seq(16, 96, by = 16)

  # Upper rectangles
  blocks <- data.frame(
    xmin = class_starts - 0.5,
    xmax = class_ends + 0.5,
    ymin = ymax * 1.02,
    ymax = ymax * 1.08,
    fill = class_col,
    label = maj_class_names,
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
      y = ymax * 0.85,
      label = round(counts_per_class),
      stringsAsFactors = FALSE
    )
  }

  # X-axis label data: 4 layers of base labels
  # Row names encode: [5'base][ref][3'base][alt]
  # For each group of 16: 4 groups of 4 (preceded by A,C,G,T)
  # Within each: followed by A,C,G,T
  preceded_by <- rep(c("A", "C", "G", "T"), each = 4, times = 6)
  followed_by <- rep(c("A", "C", "G", "T"), times = 24)

  # Build plot
  p <- ggplot(df, aes(x = x, y = value)) +
    geom_bar(stat = "identity", fill = cols, width = 0.8) +
    scale_x_continuous(
      limits = c(0, 97),
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
      ylim = c(
        min(if (xlabels) -ymax * 0.35 else 0, ymin * 1.05),
        ymax * (if (upper) 1.2 else 1.05)
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
        t = if (upper) 30 * base_size / 11 else 10,
        r = 10,
        b = if (xlabels) 40 * base_size / 11 else 10,
        l = 10
      )
    )

  # Y-axis label
  if (ylabels) {
    p <- p + ylab(ylabel)
  } else {
    p <- p + ylab(NULL) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }

  # X-axis line
  p <- p + annotate("segment", x = 0.5, xend = 96.5, y = 0, yend = 0,
                     color = "grey35", linewidth = 0.25)

  # Grid lines
  if (grid) {
    y_breaks <- seq(0, ymax, ymax / 4)
    p <- p +
      geom_hline(yintercept = y_breaks, color = "grey35", linewidth = 0.25)
  }

  # Upper colored blocks and class labels
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
        aes(x = (xmin + xmax) / 2, y = ymax * 1.14, label = label),
        size = class_label_cex * base_mm,
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

  # X-axis base labels (4 tiers)
  if (xlabels) {
    # Tier 1: preceded-by base (first line, every 4th position starting at 1)
    idx1 <- seq(1, 96, by = 4)
    label1 <- c("A", "C", "G", "T")
    p <- p +
      geom_text(
        data = data.frame(x = idx1, y = -ymax * 0.05,
                          label = rep(label1, 6)),
        aes(x = x + 1.5, y = y, label = label),
        size = axis_text_x_cex * base_mm,
        inherit.aes = FALSE
      )

    # Tiers 2-5: followed-by bases (4 rows, each shifted down)
    tier_labels <- c("A", "C", "G", "T")
    tier_y <- c(-ymax * 0.12, -ymax * 0.19, -ymax * 0.26, -ymax * 0.33)
    for (k in 1:4) {
      tier_x <- idx1 + (k - 1)
      p <- p +
        geom_text(
          data = data.frame(x = tier_x, y = tier_y[k],
                            label = rep(tier_labels[k], 24)),
          aes(x = x, y = y, label = label),
          size = axis_text_x_cex * base_mm,
          inherit.aes = FALSE
        )
    }

    # Labels on left side
    p <- p +
      annotate("text", x = 0, y = -ymax * 0.05,
               label = "preceded by 5'", hjust = 1,
               size = axis_text_x_cex * base_mm) +
      annotate("text", x = 0, y = -ymax * 0.12,
               label = "followed by 3'", hjust = 1,
               size = axis_text_x_cex * base_mm)
  }

  # Sample name title
  p <- p +
    annotate(
      "text",
      x = 1,
      y = if (catalog_type == "counts") ymax * 0.95 else ymax * 0.92,
      label = plot_title,
      hjust = 0,
      fontface = "bold",
      size = plot_title_cex * base_mm
    )

  return(p)
}
