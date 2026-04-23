# Derive ID89 axis Figlabel strings from the canonical IndelType names.
# Single-base channels (Del(C), Del(T), Ins(C), Ins(T)): drop the Del(..)/Ins(..)
# wrapper and the ":R" prefix, then pad trailing open-ended repeat ranges
# "(N,)" to "(N,9)". Multi-base channels (longer Del/Ins/MH): rewrite leading
# "Del("/"Ins(" to "L(", space-pad the leading length "(N,)" to "(N, )", 9-pad a
# trailing "R(N,)" to "R(N,9)", and space-pad a trailing "M(N,)" to "M(N, )".
id89_figlabels <- function(indel_types) {
  x <- indel_types
  is_single <- grepl("(Del|Ins)\\([CT]\\)", x)
  is_multi <- !is_single & x != "Complex"

  x[is_single] <- gsub("(Del|Ins)\\(([CT])\\)", "\\2", x[is_single])
  x[is_single] <- gsub(":R", "", x[is_single])
  x[is_single] <- gsub("\\(([0-9]+),\\)", "(\\1+)", x[is_single])
  x[is_single] <- gsub("[][]", "|", x[is_single])
  x[is_single] <- sub("^\\|", "", x[is_single])
  x[is_single] <- gsub("\\)\\|", ")", x[is_single])

  x[is_multi] <- sub("^(Del|Ins)\\(", "L(", x[is_multi])
  x[is_multi] <- sub("^L\\(([0-9]+),\\)", "L(\\1, )", x[is_multi])
  x[is_multi] <- sub("R\\(([0-9]+),\\)$", "R(\\1+)", x[is_multi])
  x[is_multi] <- sub("M\\(([0-9]+),\\)$", "M(\\1, )", x[is_multi])
  x[is_multi] <- gsub(":", "", x[is_multi])

  x
}

#' @rdname bar_plots
#'
#' @examples
#' set.seed(1)
#' sig <- runif(89)
#' sig <- sig / sum(sig)
#' names(sig) <- catalog_row_order()$ID89
#' plot_ID89(sig, plot_title = "Example ID89")
#'
#' # Label the top 5 peaks on the axis.
#' plot_ID89(sig, plot_title = "Example ID89 with peak labels",
#'           num_peak_labels = 5)
#'
#' @export
#'
#' @import ggplot2
plot_ID89 <- function(
  catalog,
  plot_title = NULL,
  upper = TRUE,
  show_axis_text_x = TRUE,
  show_axis_text_y = TRUE,
  show_axis_title_x = TRUE,
  show_axis_title_y = TRUE,
  ylim = NULL,
  base_size = 11,
  plot_title_cex = 1.0,
  title_outside_plot = FALSE,
  title_x = 0.4,
  count_label_cex = 0.9,
  block_label_cex = 0.65,
  class_label_cex = 0.8,
  axis_text_x_cex = 0.5,
  axis_title_x_cex = 0.8,
  axis_title_y_cex = 0.8,
  axis_text_y_cex = 0.7,
  show_counts = NULL,
  plot_complex = FALSE,
  num_peak_labels = 0,
  peak_label_cex = 0.7,
  stop_at_9 = FALSE,
  grid = FALSE
) {
  catalog <- normalize_catalog(catalog, 89, catalog_row_order()$ID89, "ID89")
  if (is.null(catalog)) {
    return(NULL)
  }
  base_mm <- base_mm(base_size)
  if (is.null(plot_title)) {
    plot_title <- colnames(catalog)[1] %||% ""
  }
  y_axis_type_attr <- attributes(catalog)$y_axis_type_attr
  catalog <- catalog[, 1]

  # Detect catalog type and set y-axis label
  catalog_type <- detect_y_axis_type(
    catalog,
    y_axis_type_attr = y_axis_type_attr,
    ylim = ylim
  )
  if (catalog_type == "muts_per_million") {
    ylabel <- "Muts/Million"
    catalog <- catalog * 1e6
  } else if (catalog_type == "counts") {
    ylabel <- "Counts"
  } else {
    ylabel <- ifelse(
      catalog_type == "proportion",
      "Proportion",
      "Density Proportion"
    )
  }
  # === 1. Define Indel Type Labels and Categories ===
  id89_rows <- catalog_row_order()$ID89
  stopifnot(length(id89_rows) == 89)
  indel_type_4_figurelabel <- structure(
    list(
      IndelType = id89_rows,
      Indel = c(
        rep("Del(C)", 10),
        rep("Del(T)", 27),
        rep("Ins(C)", 5),
        rep("Ins(T)", 27),
        rep("Del(2,):R(0,9)", 6),
        rep("Ins(2,)", 6),
        rep("Del(2,):M(1,)", 7),
        "Complex"
      ),
      Figlabel = id89_figlabels(id89_rows)
    ),
    class = "data.frame",
    row.names = c(NA, -89L)
  )

  # Add SubIndel column for finer block subdivision of Del(T) and Ins(T)
  indel_type_4_figurelabel$SubIndel <- indel_type_4_figurelabel$Indel
  del_t <- indel_type_4_figurelabel$Indel == "Del(T)"
  indel_type_4_figurelabel$SubIndel[del_t] <- ifelse(
    grepl("R\\(1,4\\)", indel_type_4_figurelabel$IndelType[del_t]),
    "Del(T):R(1,4)",
    ifelse(
      grepl("R\\(5,7\\)", indel_type_4_figurelabel$IndelType[del_t]),
      "Del(T):R(5,7)",
      "Del(T):R(8,)"
    )
  )
  ins_t <- indel_type_4_figurelabel$Indel == "Ins(T)"
  indel_type_4_figurelabel$SubIndel[ins_t] <- ifelse(
    grepl("R\\(0,4\\)", indel_type_4_figurelabel$IndelType[ins_t]),
    "Ins(T):R(0,4)",
    ifelse(
      grepl("R\\(5,7\\)", indel_type_4_figurelabel$IndelType[ins_t]),
      "Ins(T):R(5,7)",
      "Ins(T):R(8,)"
    )
  )

  # === 2. Prepare Data for Plotting ===
  muts_basis_melt <- prepare_indel_data(catalog, indel_type_4_figurelabel)

  if (!is.null(ylim)) {
    ymax <- ylim[2]
  } else {
    ymax <- 1.1 * max(muts_basis_melt$freq)
  }

  # === 3. Define Palettes and Block Positions ===
  indel_mypalette_block_fill <- c(
    "#fdbe6f", # Del(C)
    "#ffb34d", # Del(T):R(1,4)
    "#ff8001", # Del(T):R(5,7)
    "#cc6600", # Del(T):R(8,)
    "#b0dd8b", # Ins(C)
    "#6dcf65", # Ins(T):R(0,4)
    "#36a12e", # Ins(T):R(5,7)
    "#1a7a14", # Ins(T):R(8,)
    "#f14432", # Del(2,):R(0,9)
    "#4a98c9", # Ins(2,)
    "#61409b", # Del(2,):M(1,)
    "#000000" # Complex
  )

  indel_positions <- indel_type_4_figurelabel$IndelType
  indel_positions_labels <- indel_type_4_figurelabel$Figlabel

  entry <- table(indel_type_4_figurelabel$SubIndel)
  order_entry <- c(
    "Del(C)",
    "Del(T):R(1,4)",
    "Del(T):R(5,7)",
    "Del(T):R(8,)",
    "Ins(C)",
    "Ins(T):R(0,4)",
    "Ins(T):R(5,7)",
    "Ins(T):R(8,)",
    "Del(2,):R(0,9)",
    "Ins(2,)",
    "Del(2,):M(1,)",
    "Complex"
  )
  entry <- entry[order_entry]
  blocks <- data.frame(
    Type = names(entry),
    fill = indel_mypalette_block_fill,
    xmin = c(0, cumsum(entry)[-length(entry)]) + 0.5,
    xmax = cumsum(entry) + 0.5
  )

  top_bar_mult <- if (num_peak_labels > 0) c(1.25, 1.37) else c(1.08, 1.2)
  blocks$ymin <- ymax * top_bar_mult[1]
  blocks$ymax <- ymax * top_bar_mult[2]
  del_t_8_label <- if (stop_at_9) "Del 1 T (8-9)" else "Del 1 T (8+)"
  ins_t_8_label <- if (stop_at_9) "Ins 1 T (8-9)" else "Ins 1 T (8+)"
  blocks$labels <- c(
    "Del 1 C",
    "Del 1 T (2-4)",
    "Del 1 T (5-7)",
    del_t_8_label,
    "Ins 1 C",
    "Ins 1 T (0-4)",
    "Ins 1 T (5-7)",
    ins_t_8_label,
    "Del \u22652",
    "Ins \u22652",
    "Del Mh",
    "X"
  )
  blocks$cl <- c(
    "black",
    "black",
    "black",
    "white",
    "black",
    "black",
    "white",
    "white",
    "white",
    "white",
    "white",
    "white"
  )

  indel_mypalette_fill_all <- c(
    "Del(2,):M(1,)" = "#61409b",
    "Del(2,):R(0,9)" = "#f14432",
    "Del(C)" = "#fdbe6f",
    "Del(T)" = "#ff8001",
    "Ins(2,)" = "#4a98c9",
    "Ins(C)" = "#b0dd8b",
    "Ins(T)" = "#36a12e",
    "Complex" = "black",
    "Del(T):R(1,4)" = "#ffb34d",
    "Del(T):R(5,7)" = "#ff8001",
    "Del(T):R(8,)" = "#cc6600",
    "Ins(T):R(0,4)" = "#6dcf65",
    "Ins(T):R(5,7)" = "#36a12e",
    "Ins(T):R(8,)" = "#1a7a14"
  )

  if (!plot_complex) {
    muts_basis_melt <- muts_basis_melt[muts_basis_melt$Indel != "Complex", ]
    complex_idx <- which(indel_type_4_figurelabel$Indel == "Complex")
    indel_positions <- indel_positions[-complex_idx]
    indel_positions_labels <- indel_positions_labels[-complex_idx]
    blocks <- blocks[blocks$Type != "Complex", ]
    indel_mypalette_fill_all <- indel_mypalette_fill_all[
      !names(indel_mypalette_fill_all) %in% "Complex"
    ]
  }

  # === 5. Plotting ===
  p <- ggplot2::ggplot(
    data = muts_basis_melt,
    ggplot2::aes(x = IndelType, y = freq, fill = Indel)
  ) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", width = .7) +
    ggplot2::xlab("Indel Type") +
    ggplot2::ylab(ylabel) +
    ggplot2::scale_x_discrete(
      limits = indel_positions,
      labels = indel_positions_labels
    ) +
    ggplot2::scale_fill_manual(values = indel_mypalette_fill_all) +
    ggplot2::scale_y_continuous(
      limits = c(
        min(0, min(muts_basis_melt$freq) * 1.05),
        if (upper) {
          ymax * top_bar_mult[2]
        } else {
          ymax * 1.05
        }
      ),
      labels = if (identical(ylabel, "Counts")) {
        scales::label_number(accuracy = 1)
      } else {
        ggplot2::waiver()
      },
      expand = c(0, 0)
    ) +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      axis.text.x = if (show_axis_text_x) {
        ggplot2::element_text(
          angle = 90,
          vjust = 0.5,
          size = axis_text_x_cex * base_size,
          colour = "black",
          hjust = 1
        )
      } else {
        ggplot2::element_blank()
      },
      axis.ticks.x = if (show_axis_text_x) {
        ggplot2::element_line()
      } else {
        ggplot2::element_blank()
      },
      axis.text.y = ggplot2::element_text(
        size = axis_text_y_cex * base_size,
        colour = "black"
      ),
      legend.position = "none",
      axis.title.x = ggplot2::element_text(
        size = axis_title_x_cex * base_size,
        margin = margin(t = ifelse(show_axis_text_x, -12, 1), b = 0)
      ),
      axis.title.y = ggplot2::element_text(size = axis_title_y_cex * base_size)
    ) +
    ggplot2::scale_colour_manual(values = c("black", "white"))

  if (!show_axis_title_x) {
    p <- p + theme(axis.title.x = element_blank())
  }
  if (!show_axis_title_y) {
    p <- p + theme(axis.title.y = element_blank())
  }
  if (!show_axis_text_y) {
    p <- p +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }

  # Grid lines
  if (grid) {
    y_breaks <- seq(0, ymax, ymax / 4)
    p <- p +
      ggplot2::geom_hline(
        yintercept = y_breaks,
        color = "grey35",
        linewidth = 0.25
      )
  }

  # Add top bar conditionally
  if (upper) {
    # Always add the second bar (e.g., "Del 1bp C")
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
        size = class_label_cex * base_mm,
        fontface = "bold",
        inherit.aes = FALSE
      )
  }

  # Resolve show_counts: NULL = auto (counts only), TRUE/FALSE = forced
  show_counts <- resolve_show_counts(show_counts, catalog_type)

  # Add count labels
  if (show_counts) {
    counts_by_block <- aggregate(
      abs(freq) ~ SubIndel,
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
    count_label_df$y <- ymax * 0.9

    p <- p +
      ggplot2::geom_text(
        data = count_label_df,
        ggplot2::aes(x = x, y = y, label = count),
        size = count_label_cex * base_mm,
        inherit.aes = FALSE
      )
  }

  p <- add_peak_labels(
    p,
    muts_basis_melt,
    "IndelType",
    "freq",
    "Figlabel",
    num_peak_labels = num_peak_labels,
    peak_label_cex = peak_label_cex,
    base_size = base_size
  )

  # Scale title y by top_bar_mult[1] (bottom of the colored-block strip) so
  # the title sits in the gap between the tallest bar and the blocks, matching
  # plot_ID83's visual layout.
  p <- add_plot_title(
    p,
    plot_title,
    title_outside_plot,
    plot_title_cex,
    base_size,
    ymax = ymax * top_bar_mult[1],
    x = 0.5 + title_x * length(indel_positions)
  )

  return(p)
}
