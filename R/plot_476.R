#' Plot 476-channel indel profile
#'
#' Creates a bar plot visualization of a 476-channel indel mutational signature
#' for a single sample. The plot shows mutation counts or proportions across
#' all 476 indel categories with color-coded category blocks and flanking base
#' annotations. Includes smart peak labeling using ggrepel.
#'
#' @param catalog Numeric vector of length 476 containing indel counts or
#'   proportions for a single sample.
#' @param block_text_size Numeric. Size of category block labels (e.g. "Del 1bp C").
#'   Scaled by `base_size / 11`.
#' @param plot_title Character. Title displayed above the plot.
#' @param num_labels Integer. Number of top peaks to label per category block.
#'   Set to 0 or NULL to disable labels.
#' @param ggrepel_size Numeric. Size of ggrepel peak labels. Scaled by
#'   `base_size / 11`.
#' @param label_threshold_denominator Numeric. Peaks with values less than
#'   max/label_threshold_denominator are not labeled.
#' @param vline_labels Character vector. IndelType labels at which to draw
#'   vertical reference lines. For example, `c("A[Del(C):R1]A", "G[Del(C):R1]A")`.
#' @param simplify_labels Logical. If TRUE, simplifies peak labels by removing
#'   the indel type prefix.
#' @param base_size Base font size for ggplot2's theme. All text sizes scale
#'   relative to this value.
#' @param title_text_size Numeric. Relative size of the plot title text, passed to `rel()`.
#' @param x_axis_tick_label_size Numeric. Relative size of x-axis tick labels, passed to `rel()`.
#' @param y_axis_tick_label_size Numeric. Relative size of y-axis tick labels, passed to `rel()`.
#' @param x_title_size Numeric. Relative size of x-axis title, passed to `rel()`.
#' @param y_title_size Numeric. Relative size of y-axis title, passed to `rel()`.
#' @param plot_complex Logical. If TRUE, include the 5 Complex indel channels.
#' @param text_size Deprecated. Use `block_text_size` instead.
#' @param label_size Deprecated. Use `ggrepel_size` instead.
#'
#' @return A ggplot2 object containing the 476-channel indel profile plot.
#'
#' @export
#'
#' @import ggplot2 reshape2 dplyr ggrepel
#'
plot_476 <- function(
  catalog,
  block_text_size = 3,
  plot_title = "test",
  num_labels = 3,
  ggrepel_size = 2,
  label_threshold_denominator = 7,
  vline_labels = c(),
  simplify_labels = TRUE,
  base_size = 11,
  title_text_size = 1.0,
  x_axis_tick_label_size = 0.8,
  y_axis_tick_label_size = 0.7,
  x_title_size = 0.7,
  y_title_size = 0.9,
  plot_complex = FALSE,
  text_size = NULL,
  label_size = NULL
) {
  # Handle deprecated text_size
  if (!is.null(text_size)) {
    if (!missing(block_text_size)) {
      stop("Cannot specify both 'text_size' and 'block_text_size'.")
    }
    warning("'text_size' is deprecated. Use 'block_text_size' instead.")
    block_text_size <- text_size
  }

  # Handle deprecated label_size
  if (!is.null(label_size)) {
    if (!missing(ggrepel_size)) {
      stop("Cannot specify both 'label_size' and 'ggrepel_size'.")
    }
    warning("'label_size' is deprecated. Use 'ggrepel_size' instead.")
    ggrepel_size <- label_size
  }

  # Ensure catalog is a numeric vector
  if (is.data.frame(catalog) || is.matrix(catalog)) {
    catalog <- as.numeric(catalog[, 1])
  }

  # Determine y-axis label based on sum
  ylabel <- if (sum(catalog, na.rm = TRUE) < 1.1) {
    "Proportion"
  } else {
    "Count"
  }
  Koh476_indeltype = type_476_indel_type()
  my_vector <- Koh476_indeltype$IndelType
  muts_basis <- data.frame(Sample = catalog, IndelType = my_vector)
  muts_basis_melt <- reshape2::melt(muts_basis, "IndelType")
  muts_basis_melt <- merge(
    Koh476_indeltype,
    muts_basis_melt,
    by = "IndelType",
    all.x = TRUE
  )
  muts_basis_melt[is.na(muts_basis_melt)] <- 0
  names(muts_basis_melt) <- c(
    "IndelType",
    "Indel",
    "Indel3",
    "Figlabel",
    "Sample",
    "freq"
  )
  muts_basis_melt$Sample <- as.character(muts_basis_melt$Sample)

  # Add x position for plotting
  muts_basis_melt$x_pos <- match(
    muts_basis_melt$IndelType,
    Koh476_indeltype$IndelType
  )

  if (!plot_complex) {
    muts_basis_melt <- muts_basis_melt[muts_basis_melt$Indel != "Complex", ]
  }

  indel_mypalette_fill <- c(
    "#000000",
    "#61409b",
    "#f14432",
    "#fdbe6f",
    "#ff8001",
    "#4a98c9",
    "#b0dd8b",
    "#36a12e"
  )

  indel_positions <- Koh476_indeltype$IndelType

  entry <- table(Koh476_indeltype$Indel)
  order_entry <- c(
    "Del(C)",
    "Del(T)",
    "Ins(C)",
    "Ins(T)",
    "Del(2,):R(1,9)",
    "Ins(2,):R(0,9)",
    "Del(2,):M(1,)",
    "Complex"
  )
  entry <- entry[order_entry]

  blocks <- data.frame(
    Type = unique(Koh476_indeltype$Indel),
    fill = indel_mypalette_fill,
    xmin = c(0, cumsum(entry)[-length(entry)]) + 0.5,
    xmax = cumsum(entry) + 0.5
  )
  blocks$ymin <- max(muts_basis_melt$freq) * 1.35
  blocks$ymax <- max(muts_basis_melt$freq) * 1.47
  blocks$labels <- c(
    "Del 1bp C",
    "Del 1bp T",
    "Ins 1bp C",
    "Ins 1bp T",
    "Del \u22652bp", # \u.... is >= character
    "Ins \u22652bp",
    "Mh",
    "X"
  )
  blocks$cl <- c(
    "black",
    "black",
    "black",
    "black",
    "white",
    "white",
    "white",
    "white"
  )

  indel_mypalette_fill_all <- c(
    "Del(2,):M(1,)" = "#61409b",
    "Del(2,):R(1,9)" = "#f14432",
    "Del(C)" = "#fdbe6f",
    "Del(T)" = "#ff8001",
    "Ins(2,):R(0,9)" = "#4a98c9",
    "Ins(C)" = "#b0dd8b",
    "Ins(T)" = "#36a12e",
    "Complex" = "black"
  )

  if (!plot_complex) {
    blocks <- blocks[blocks$Type != "Complex", ]
    indel_mypalette_fill_all <- indel_mypalette_fill_all[names(indel_mypalette_fill_all) != "Complex"]
  }

  n_channels <- max(muts_basis_melt$x_pos)

  # Create label data: top num_labels per block, excluding peaks < 1/10 max
  max_freq <- max(muts_basis_melt$freq)
  min_threshold <- max_freq / label_threshold_denominator

  if (!is.null(num_labels) && num_labels > 0) {
    label_data <- muts_basis_melt |>
      dplyr::filter(freq >= min_threshold) |>
      dplyr::group_by(Indel) |>
      dplyr::slice_max(order_by = freq, n = num_labels, with_ties = FALSE) |>
      dplyr::ungroup()
  } else {
    label_data <- muts_basis_melt[0, ] # Empty data frame, no labels
  }

  # Simplify labels if requested
  if (simplify_labels && nrow(label_data) > 0) {
    label_data$Figlabel <- ifelse(
      grepl("[ACGT]\\[(Del|Ins)\\([CT]", label_data$Figlabel),
      sub("^[ACGT]\\[(Del|Ins)\\([CT]\\):", "", label_data$Figlabel),
      label_data$Figlabel
    )
  }

  # Create flanking base annotation blocks
  flanking_patterns <- c(
    "A\\[Del",
    "G\\[Del",
    "T\\[Del",
    "A\\[Ins",
    "G\\[Ins",
    "T\\[Ins",
    "C\\[Del",
    "C\\[Ins",
    "Del2:U1:R1",
    "Ins\\(2,4\\):M",
    "Del2:M1"
  )
  flanking_labels <- c(
    " A...",
    " G...",
    " T...",
    " A...",
    " G...",
    " T...",
    " C...",
    " C...",
    "",
    "",
    ""
  )

  # Find runs of each pattern
  flanking_blocks <- data.frame(
    label = character(),
    xmin = numeric(),
    xmax = numeric(),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(flanking_patterns)) {
    matches <- grep(flanking_patterns[i], Koh476_indeltype$IndelType)
    if (length(matches) > 0) {
      # Find contiguous runs
      runs <- split(matches, cumsum(c(1, diff(matches) != 1)))
      for (run in runs) {
        flanking_blocks <- rbind(
          flanking_blocks,
          data.frame(
            label = flanking_labels[i],
            xmin = min(run) - 0.5,
            xmax = max(run) + 0.5,
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  # Position flanking blocks below the x-axis
  flanking_blocks$y <- -max(muts_basis_melt$freq) * 0.04
  # Find positions for vertical lines
  vline_positions <- unlist(lapply(vline_labels, function(label) {
    which(Koh476_indeltype$IndelType == label)
  }))

  p <- ggplot2::ggplot(
    data = muts_basis_melt,
    ggplot2::aes(x = x_pos, y = freq, fill = Indel)
  ) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    ggplot2::geom_vline(
      xintercept = vline_positions,
      linetype = "dashed",
      color = "gray50"
    ) +
    ggplot2::xlab("Indel Type") +
    ggplot2::ylab(ylabel) +
    ggplot2::scale_x_continuous(
      breaks = flanking_blocks$xmin,
      labels = flanking_blocks$label,
      limits = c(0.5, n_channels + 0.5)
    ) +
    ggplot2::scale_y_continuous(
      expand = expansion(mult = c(0.03, 0)) # No padding at bottom, 5% at top
    ) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::scale_fill_manual(values = indel_mypalette_fill_all) +
    ggplot2::coord_cartesian(ylim = c(0, max(blocks$ymax)), clip = "off") +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        size = rel(x_axis_tick_label_size),
        angle = 0,
        hjust = 0,
        vjust = 8, # Anchor text at bottom (above axis)
      ),
      axis.ticks.x = ggplot2::element_line(),
      axis.ticks.length.x = unit(-1, "line"), # Negative = upward ticks
      axis.text.y = ggplot2::element_text(size = rel(y_axis_tick_label_size), colour = "black"),
      legend.position = "none",
      axis.title.x = ggplot2::element_text(
        size = rel(x_title_size),
        margin = margin(t = 10)
      ),
      axis.title.y = ggplot2::element_text(size = rel(y_title_size)),
      plot.margin = margin(t = 10, r = 10, b = 80, l = 10),
      axis.line = ggplot2::element_line(linewidth = rel(0.5)),
      plot.title = ggplot2::element_text(size = rel(title_text_size))
    ) +
    ggplot2::scale_colour_manual(
      values = c("black" = "black", "white" = "white")
    ) +
    ggplot2::geom_rect(
      data = blocks,
      ggplot2::aes(
        xmin = xmin,
        ymin = ymin,
        xmax = xmax,
        ymax = ymax,
        fill = Type,
        colour = "white"
      ),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_text(
      data = blocks,
      ggplot2::aes(
        x = (xmax + xmin) / 2,
        y = (ymax + ymin) / 2,
        label = labels,
        colour = cl
      ),
      size = block_text_size * base_size / 11,
      fontface = "bold",
      inherit.aes = FALSE
    ) +
    ggrepel::geom_text_repel(
      data = label_data,
      ggplot2::aes(x = x_pos, y = freq, label = Figlabel),
      size = ggrepel_size * base_size / 11,
      nudge_y = max(muts_basis_melt$freq) * 0.1,
      direction = "both",
      segment.color = "gray50",
      segment.size = 0.5 * base_size / 11,
      arrow = grid::arrow(length = unit(0.01, "npc"), type = "closed"),
      max.overlaps = 50,
      min.segment.length = 0,

      box.padding = 1, # Padding around label box
      point.padding = 1, # Padding from data point
      force = 2, # Repulsion force between labels
      force_pull = 0.5, # Pull toward data point (lower = less pull)

      inherit.aes = FALSE
    )

  return(p)
}
