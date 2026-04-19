#' @rdname bar_plots
#'
#' @examples
#' set.seed(1)
#' sig <- runif(476)
#' sig <- sig / sum(sig)
#' names(sig) <- catalog_row_order()$ID476
#' plot_ID476_right(sig, plot_title = "Example ID476 right panel")
#'
#' @export
#'
#' @import ggplot2
#' @importFrom dplyr filter group_by slice_max ungroup
plot_ID476_right <- function(
  catalog,
  plot_title = NULL,
  base_size = 11,
  plot_title_cex = 1.0,
  title_outside_plot = FALSE,
  count_label_cex = 1.03,
  class_label_cex = 1.0,
  axis_text_x_cex = 0.5,
  axis_title_x_cex = 0.8,
  axis_title_y_cex = 0.8,
  axis_text_y_cex = 0.7,
  show_axis_text_x = TRUE,
  show_axis_text_y = TRUE,
  show_axis_title_x = TRUE,
  show_axis_title_y = TRUE,
  show_counts = NULL,
  num_peak_labels = 3,
  peak_label_cex = 0.7,
  label_threshold_denominator = 7,
  vline_labels = c(),
  simplify_labels = TRUE,
  plot_complex = FALSE,
  stop_at_9 = TRUE,
  grid = FALSE
) {
  catalog <- normalize_catalog(catalog, 476, catalog_row_order()$ID476, "ID476")
  if (is.null(catalog)) return(NULL)
  if (is.null(plot_title)) plot_title <- colnames(catalog)[1] %||% ""
  y_axis_type_attr <- attributes(catalog)$y_axis_type_attr
  catalog <- catalog[, 1]

  # Determine y-axis label based on catalog type
  catalog_type <- detect_y_axis_type(catalog, y_axis_type_attr = y_axis_type_attr)
  if (catalog_type == "muts_per_million") {
    ylabel <- "Muts/Million"
    catalog <- catalog * 1e6
  } else if (catalog_type == "counts") {
    ylabel <- "Counts"
  } else {
    ylabel <- ifelse(catalog_type == "proportion",
                     "Proportion", "Density Proportion")
  }
  Koh476_indeltype <- type_476_indel_type()
  muts_basis_melt <- prepare_indel_data(catalog, Koh476_indeltype)

  # Add x position for plotting
  muts_basis_melt$x_pos <- match(
    muts_basis_melt$IndelType,
    Koh476_indeltype$IndelType
  )

  # Filter to right portion (positions >= 343) and re-index
  muts_basis_melt <- muts_basis_melt[muts_basis_melt$x_pos >= 343, ]
  muts_basis_melt$x_pos <- muts_basis_melt$x_pos - 342

  if (!plot_complex) {
    muts_basis_melt <- muts_basis_melt[muts_basis_melt$Indel != "Complex", ]
  }

  indel_mypalette_fill <- c(
    "#f14432",
    "#4a98c9",
    "#61409b",
    "#000000"
  )

  right_indeltype <- Koh476_indeltype[343:476, ]

  entry <- table(right_indeltype$Indel)
  order_entry <- c(
    "Del(2,):R(1,9)",
    "Ins(2,):R(0,9)",
    "Del(2,):M(1,)",
    "Complex"
  )
  entry <- entry[order_entry]

  blocks <- data.frame(
    Type = order_entry,
    fill = indel_mypalette_fill,
    xmin = c(0, cumsum(entry)[-length(entry)]) + 0.5,
    xmax = cumsum(entry) + 0.5
  )
  blocks$ymin <- max(muts_basis_melt$freq) * 1.35
  blocks$ymax <- max(muts_basis_melt$freq) * 1.47
  blocks$labels <- c(
    "Del \u22652bp",
    "Ins \u22652bp",
    "Mh",
    "X"
  )
  blocks$cl <- c(
    "white",
    "white",
    "white",
    "white"
  )

  indel_mypalette_fill_all <- c(
    "Del(2,):R(1,9)" = "#f14432",
    "Ins(2,):R(0,9)" = "#4a98c9",
    "Del(2,):M(1,)" = "#61409b",
    "Complex" = "black"
  )

  if (!plot_complex) {
    blocks <- blocks[blocks$Type != "Complex", ]
    indel_mypalette_fill_all <- indel_mypalette_fill_all[
      names(indel_mypalette_fill_all) != "Complex"
    ]
  }

  # Create label data: top num_peak_labels per block, excluding peaks < 1/10 max
  max_freq <- max(muts_basis_melt$freq)
  min_threshold <- max_freq / label_threshold_denominator

  if (!is.null(num_peak_labels) && num_peak_labels > 0) {
    label_data <- muts_basis_melt |>
      dplyr::filter(freq >= min_threshold) |>
      dplyr::group_by(Indel) |>
      dplyr::slice_max(order_by = freq, n = num_peak_labels, with_ties = FALSE) |>
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
    "Del2:U1:R1",
    "Ins\\(2,4\\):M",
    "Del2:M1"
  )
  flanking_labels <- c(
    "",
    "",
    ""
  )

  # Find runs of each pattern against the right-side IndelTypes
  right_indeltypes <- Koh476_indeltype$IndelType[343:476]
  flanking_blocks <- data.frame(
    label = character(),
    xmin = numeric(),
    xmax = numeric(),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(flanking_patterns)) {
    matches <- grep(flanking_patterns[i], right_indeltypes)
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
  # Find positions for vertical lines (adjusted to 1-134 space)
  vline_positions <- unlist(lapply(vline_labels, function(label) {
    pos <- which(Koh476_indeltype$IndelType == label)
    pos <- pos - 342
    pos[pos >= 1]
  }))

  n_channels <- max(muts_basis_melt$x_pos)

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
      expand = expansion(mult = c(0.03, 0)), # No padding at bottom, 5% at top
      labels = if (ylabel == "Counts") {
        scales::label_number(accuracy = 1)
      } else {
        ggplot2::waiver()
      }
    ) +
    ggplot2::scale_fill_manual(values = indel_mypalette_fill_all) +
    ggplot2::coord_cartesian(ylim = c(0, max(blocks$ymax)), clip = "off") +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        size = axis_text_x_cex * base_size,
        angle = 0,
        hjust = 0,
        vjust = 0.5,
      ),
      axis.ticks.x = ggplot2::element_line(),
      axis.ticks.length.x = unit(0.3, "line"), # Downward ticks
      axis.text.y = ggplot2::element_text(
        size = axis_text_y_cex * base_size,
        colour = "black"
      ),
      legend.position = "none",
      axis.title.x = ggplot2::element_text(
        size = axis_title_x_cex * base_size,
        margin = margin(t = 10)
      ),
      axis.title.y = ggplot2::element_text(size = axis_title_y_cex * base_size),
      plot.margin = margin(t = 10, r = 10, b = 80, l = 10),
      axis.line = ggplot2::element_line(linewidth = 0.5)
    ) +
    ggplot2::scale_colour_manual(
      values = c("black" = "black", "white" = "white")
    )

  # Grid lines
  if (grid) {
    y_breaks <- seq(0, max_freq, max_freq / 4)
    p <- p +
      ggplot2::geom_hline(yintercept = y_breaks,
                          color = "grey35", linewidth = 0.25)
  }

  p <- p +
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
      size = class_label_cex * base_size / ggplot2::.pt,
      fontface = "bold",
      inherit.aes = FALSE
    ) +
    ggrepel::geom_text_repel(
      data = label_data,
      ggplot2::aes(x = x_pos, y = freq, label = Figlabel),
      size = peak_label_cex * base_size / ggplot2::.pt,
      nudge_y = max(muts_basis_melt$freq) * 0.1,
      direction = "both",
      segment.color = "gray50",
      segment.size = 0.13 * base_size / ggplot2::.pt,
      arrow = grid::arrow(length = unit(0.01, "npc"), type = "closed"),
      max.overlaps = 50,
      min.segment.length = 0,

      box.padding = 1, # Padding around label box
      point.padding = 0.1, # Padding from data point
      force = 2, # Repulsion force between labels
      force_pull = 0.5, # Pull toward data point (lower = less pull)

      inherit.aes = FALSE
    )

  # Resolve show_counts: NULL = auto (counts only), TRUE/FALSE = forced
  show_counts <- resolve_show_counts(show_counts, catalog_type)

  # Add count labels
  if (show_counts) {
    counts_by_block <- aggregate(
      abs(freq) ~ Indel,
      data = muts_basis_melt,
      FUN = sum
    )
    names(counts_by_block) <- c("Type", "count")
    counts_by_block$count <- ifelse(
      counts_by_block$count > 0 & counts_by_block$count < 1,
      signif(counts_by_block$count, 2),
      round(counts_by_block$count)
    )
    count_label_df <- merge(blocks, counts_by_block, by = "Type")
    count_label_df$x <- (count_label_df$xmin + count_label_df$xmax) / 2
    count_label_df$y <- max_freq * 1.2

    p <- p +
      ggplot2::geom_text(
        data = count_label_df,
        ggplot2::aes(x = x, y = y, label = count),
        size = count_label_cex * base_size / ggplot2::.pt,
        inherit.aes = FALSE
      )
  }

  if (!show_axis_text_x) {
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  if (!show_axis_text_y) {
    p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }
  if (!show_axis_title_x) {
    p <- p + theme(axis.title.x = element_blank())
  }
  if (!show_axis_title_y) {
    p <- p + theme(axis.title.y = element_blank())
  }

  # Use blocks$ymin (bottom of the colored-block strip at 1.35*max_freq) so
  # the title sits in the gap between the tallest bar and the blocks.
  p <- add_plot_title(p, plot_title, title_outside_plot,
                      plot_title_cex, base_size,
                      ymax = min(blocks$ymin), x = 1)

  return(p)
}
