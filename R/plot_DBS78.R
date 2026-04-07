#' @rdname bar_plots
#'
#' @examples
#' set.seed(1)
#' sig <- runif(78)
#' sig <- sig / sum(sig)
#' names(sig) <- catalog_row_order()$DBS78
#' plot_DBS78(sig, plot_title = "Example DBS78 signature")
#'
#' @export
#'
#' @import ggplot2 dplyr
plot_DBS78 <- function(
  catalog,
  plot_title = NULL,
  grid = TRUE,
  upper = TRUE,
  show_axis_text_x = TRUE,
  show_axis_text_y = TRUE,
  show_axis_title_x = TRUE,
  show_axis_title_y = TRUE,
  xlabels = NULL,
  ylabels = NULL,
  ylim = NULL,
  base_size = 11,
  plot_title_cex = 0.8,
  count_label_cex = 0.9,
  class_label_cex = 0.7,
  axis_text_x_cex = 0.5,
  axis_title_x_cex = 1.0,
  axis_title_y_cex = 1.0,
  axis_text_y_cex = 0.8,
  show_counts = NULL,
  num_peak_labels = 0,
  peak_label_cex = 0.7
) {
  catalog <- normalize_catalog(catalog, 78, catalog_row_order()$DBS78, "DBS78")
  if (is.null(catalog)) return(NULL)
  if (is.null(plot_title)) plot_title <- colnames(catalog)[1] %||% ""

  axis_vis <- resolve_axis_params(
    show_axis_text_x, show_axis_text_y,
    show_axis_title_x, show_axis_title_y,
    xlabels, ylabels
  )
  show_axis_text_x <- axis_vis$show_axis_text_x
  show_axis_text_y <- axis_vis$show_axis_text_y
  show_axis_title_y <- axis_vis$show_axis_title_y

  base_mm <- base_mm(base_size)

  # 10 DBS class colors (RColorBrewer "Paired")
  dinuc_class_col <- c(
    "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99",
    "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A"
  )

  # Class sizes: AC=9, AT=6, CC=9, CG=6, CT=9, GC=6, TA=6, TC=9, TG=9, TT=9
  class_sizes <- c(9, 6, 9, 6, 9, 6, 6, 9, 9, 9)
  maj_class_names <- c("AC>NN", "AT>NN", "CC>NN", "CG>NN", "CT>NN",
                        "GC>NN", "TA>NN", "TC>NN", "TG>NN", "TT>NN")

  cols <- rep(dinuc_class_col, class_sizes)

  df <- data.frame(
    x = 1:78,
    value = catalog[, 1],
    stringsAsFactors = FALSE
  )
  df$label <- rownames(catalog)

  # Detect catalog type
  catalog_type <- detect_catalog_type(df$value, attributes(catalog)$catalog.type)

  if (catalog_type == "density") {
    ylabel <- "mut/million"
    df$value <- df$value * 1e6
    ymax <- max(df$value) * 1.3
  } else if (catalog_type == "counts") {
    ymax <- 4 * ceiling(max(max(df$value) * 1.3, 10) / 4)
    ylabel <- "counts"
  } else {
    ymax <- max(max(df$value) * 1.3, 0.01)
    ylabel <- ifelse(catalog_type == "counts.signature",
                     "counts proportion", "density proportion")
  }

  if (!is.null(ylim)) {
    ymax <- ylim[2]
  }
  ymin <- min(0, min(df$value))

  # Class boundaries
  class_ends <- cumsum(class_sizes)
  class_starts <- c(1, class_ends[-length(class_ends)] + 1)

  # Upper rectangles
  blocks <- data.frame(
    xmin = class_starts - 0.5,
    xmax = class_ends + 0.5,
    ymin = ymax * 1.01,
    ymax = ymax * 1.08,
    fill = dinuc_class_col,
    label = maj_class_names,
    stringsAsFactors = FALSE
  )

  # Resolve show_counts
  show_counts <- resolve_show_counts(show_counts, catalog_type)

  # Count labels per class
  if (show_counts) {
    counts_per_class <- numeric(10)
    for (i in 1:10) {
      counts_per_class[i] <- sum(abs(df$value[class_starts[i]:class_ends[i]]))
    }
    count_labels <- data.frame(
      x = class_ends + 0.5,
      y = ymax * 0.84,
      label = round(counts_per_class),
      stringsAsFactors = FALSE
    )
  }

  # X-axis labels: 2-character alt dinucleotide from row names
  rnames <- rownames(catalog)
  if (!is.null(rnames) && length(rnames) == 78) {
    x_labels <- paste0(substr(rnames, 3, 3), substr(rnames, 4, 4))
  } else {
    x_labels <- rep("", 78)
  }

  # Build plot
  p <- ggplot(df, aes(x = x, y = value)) +
    geom_bar(stat = "identity", fill = cols, width = 0.8) +
    scale_x_continuous(
      limits = c(0, 79),
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
        min(if (show_axis_text_x) -ymax * 0.15 else 0, ymin * 1.05),
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
        t = if (upper) 25 * base_size / 11 else 10,
        r = 10,
        b = if (show_axis_text_x) 20 * base_size / 11 else 10,
        l = 10
      )
    )

  if (show_axis_title_y) {
    p <- p + ylab(ylabel)
  } else {
    p <- p + ylab(NULL)
  }
  if (!show_axis_text_y) {
    p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }

  # Grid
  if (grid) {
    y_breaks <- seq(0, ymax, ymax / 4)
    p <- p +
      geom_hline(yintercept = y_breaks, color = "grey60", linewidth = 0.25) +
      annotate("rect", xmin = 0.5, xmax = 78.5, ymin = 0, ymax = ymax,
               fill = NA, color = "grey60", linewidth = 0.5)
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
        aes(x = (xmin + xmax) / 2, y = ymax * 1.123, label = label),
        size = class_label_cex * base_mm,
        inherit.aes = FALSE
      )
  }

  # X-axis labels (two rows: alt base 1 and alt base 2)
  if (show_axis_text_x) {
    alt1 <- substr(x_labels, 1, 1)
    alt2 <- substr(x_labels, 2, 2)
    p <- p +
      geom_text(
        data = data.frame(x = 1:78, y = -ymax * 0.02, label = alt1),
        aes(x = x, y = y, label = label),
        size = axis_text_x_cex * base_mm, angle = 90, hjust = 1,
        inherit.aes = FALSE
      ) +
      geom_text(
        data = data.frame(x = 1:78, y = -ymax * 0.08, label = alt2),
        aes(x = x, y = y, label = label),
        size = axis_text_x_cex * base_mm, angle = 90, hjust = 1,
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
      size = plot_title_cex * base_mm
    )

  p <- add_peak_labels(p, df, "x", "value", "label",
                       num_peak_labels = num_peak_labels, peak_label_cex = peak_label_cex,
                       base_size = base_size)

  return(p)
}
