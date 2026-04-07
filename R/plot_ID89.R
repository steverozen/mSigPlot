#' @rdname bar_plots
#'
#' @examples
#' set.seed(1)
#' sig <- runif(89)
#' sig <- sig / sum(sig)
#' names(sig) <- catalog_row_order()$ID89
#' plot_ID89(sig, plot_title = "Example ID89")
#'
#' @export
#'
#' @import ggplot2 reshape2 dplyr ggrepel
plot_ID89 <- function(
  catalog,
  plot_title = NULL,
  upper = TRUE,
  show_axis_text_x = TRUE,
  show_axis_text_y = TRUE,
  show_axis_title_x = TRUE,
  show_axis_title_y = TRUE,
  xlabels = NULL,
  ylim = NULL,
  base_size = 11,
  plot_title_cex = 1.0,
  count_label_cex = 1.03,
  block_label_cex = 3,
  class_label_cex = block_label_cex,
  axis_text_x_cex = 0.7,
  axis_title_x_cex = 0.9,
  axis_title_y_cex = 0.9,
  axis_text_y_cex = 0.7,
  show_counts = NULL,
  ylab = TRUE,
  show_extra_top_bar = FALSE,
  plot_complex = FALSE,
  num_peak_labels = 0,
  peak_label_cex = 0.7,
  stop_at_9 = TRUE
) {
  catalog <- normalize_catalog(catalog, 89, catalog_row_order()$ID89, "ID89")
  if (is.null(catalog)) {
    return(NULL)
  }
  if (is.null(plot_title)) {
    plot_title <- colnames(catalog)[1] %||% ""
  }
  catalog <- catalog[, 1]

  axis_vis <- resolve_axis_params(
    show_axis_text_x, show_axis_text_y,
    show_axis_title_x, show_axis_title_y,
    xlabels = xlabels
  )
  show_axis_text_x <- axis_vis$show_axis_text_x
  show_axis_text_y <- axis_vis$show_axis_text_y
  show_axis_title_x <- axis_vis$show_axis_title_x
  show_axis_title_y <- axis_vis$show_axis_title_y

  # Resolve ylab parameter
  if (isTRUE(ylab)) {
    catalog_type <- detect_catalog_type(catalog, ylim = ylim)
    ylabel <- if (catalog_type == "counts") "Counts" else "Proportion"
  } else if (is.character(ylab)) {
    ylabel <- ylab
    catalog_type <- detect_catalog_type(catalog, ylim = ylim)
  } else {
    # NULL or FALSE: suppress y-axis title
    if (show_axis_title_y) {
      warning(
        "ylab = NULL/FALSE conflicts with show_axis_title_y = TRUE; ",
        "suppressing y-axis title",
        call. = FALSE
      )
    }
    show_axis_title_y <- FALSE
    catalog_type <- detect_catalog_type(catalog, ylim = ylim)
    ylabel <- if (catalog_type == "counts") "Counts" else "Proportion"
  }
  # === 1. Define Indel Type Labels and Categories ===
  indel_type_4_figurelabel <- structure(
    list(
      IndelType = c(
        # Single base deletions - C
        "[Del(C):R1]A",
        "[Del(C):R1]T",
        "[Del(C):R2]A",
        "[Del(C):R2]T",
        "[Del(C):R3]A",
        "[Del(C):R3]T",
        "[Del(C):R(4,5)]A",
        "[Del(C):R(4,5)]T",
        "[Del(C):R(1,5)]G",
        "Del(C):R(6,9)",
        # Single base deletions - T
        "A[Del(T):R(1,4)]A",
        "A[Del(T):R(1,4)]C",
        "A[Del(T):R(1,4)]G",
        "C[Del(T):R(1,4)]A",
        "C[Del(T):R(1,4)]C",
        "C[Del(T):R(1,4)]G",
        "G[Del(T):R(1,4)]A",
        "G[Del(T):R(1,4)]C",
        "G[Del(T):R(1,4)]G",
        "A[Del(T):R(5,7)]A",
        "A[Del(T):R(5,7)]C",
        "A[Del(T):R(5,7)]G",
        "C[Del(T):R(5,7)]A",
        "C[Del(T):R(5,7)]C",
        "C[Del(T):R(5,7)]G",
        "G[Del(T):R(5,7)]A",
        "G[Del(T):R(5,7)]C",
        "G[Del(T):R(5,7)]G",
        "A[Del(T):R(8,)]A",
        "A[Del(T):R(8,)]C",
        "A[Del(T):R(8,)]G",
        "C[Del(T):R(8,)]A",
        "C[Del(T):R(8,)]C",
        "C[Del(T):R(8,)]G",
        "G[Del(T):R(8,)]A",
        "G[Del(T):R(8,)]C",
        "G[Del(T):R(8,)]G",
        # Single base insertions - C
        "A[Ins(C):R0]A",
        "A[Ins(C):R0]T",
        "Ins(C):R(0,3)",
        "Ins(C):R(4,6)",
        "Ins(C):R(7,)",
        # Single base insertions - T
        "A[Ins(T):R(0,4)]A",
        "A[Ins(T):R(0,4)]C",
        "A[Ins(T):R(0,4)]G",
        "C[Ins(T):R(0,4)]A",
        "C[Ins(T):R(0,4)]C",
        "C[Ins(T):R(0,4)]G",
        "G[Ins(T):R(0,4)]A",
        "G[Ins(T):R(0,4)]C",
        "G[Ins(T):R(0,4)]G",
        "A[Ins(T):R(5,7)]A",
        "A[Ins(T):R(5,7)]C",
        "A[Ins(T):R(5,7)]G",
        "C[Ins(T):R(5,7)]A",
        "C[Ins(T):R(5,7)]C",
        "C[Ins(T):R(5,7)]G",
        "G[Ins(T):R(5,7)]A",
        "G[Ins(T):R(5,7)]C",
        "G[Ins(T):R(5,7)]G",
        "A[Ins(T):R(8,)]A",
        "A[Ins(T):R(8,)]C",
        "A[Ins(T):R(8,)]G",
        "C[Ins(T):R(8,)]A",
        "C[Ins(T):R(8,)]C",
        "C[Ins(T):R(8,)]G",
        "G[Ins(T):R(8,)]A",
        "G[Ins(T):R(8,)]C",
        "G[Ins(T):R(8,)]G",
        # Longer deletions (no MH)
        "Del(2,4):R1",
        "Del(5,):R1",
        "Del(2,8):U(1,2):R(2,4)",
        "Del(2,):U(1,2):R(5,)",
        "Del(3,):U(3,):R2",
        "Del(3,):U(3,):R(3,)",
        # Longer insertions
        "Ins(2,4):R0",
        "Ins(5,):R0",
        "Ins(2,4):R1",
        "Ins(5,):R1",
        "Ins(2,):R(2,4)",
        "Ins(2,):R(5,)",
        # Deletions with MH
        "Del(2,5):M1",
        "Del(3,5):M2",
        "Del(4,5):M(3,4)",
        "Del(6,):M1",
        "Del(6,):M2",
        "Del(6,):M3",
        "Del(6,):M(4,)",
        # Complex
        "Complex"
      ),
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
      Indel3 = c(
        rep("Deletion", 37),
        rep("Insertion", 32),
        rep("Deletion", 6),
        rep("Insertion", 6),
        rep("Deletion", 7),
        "Complex"
      ),
      Figlabel = c(
        # Deletion labels
        "[C1]A",
        "[C1]T",
        "[C2]A",
        "[C2]T",
        "[C3]A",
        "[C3]T",
        "[C(4,5)]A",
        "[C(4,5)]T",
        "[C(1,5)]G",
        "C(6,9)",
        "A[T(1,4)]A",
        "A[T(1,4)]C",
        "A[T(1,4)]G",
        "C[T(1,4)]A",
        "C[T(1,4)]C",
        "C[T(1,4)]G",
        "G[T(1,4)]A",
        "G[T(1,4)]C",
        "G[T(1,4)]G",
        "A[T(5,7)]A",
        "A[T(5,7)]C",
        "A[T(5,7)]G",
        "C[T(5,7)]A",
        "C[T(5,7)]C",
        "C[T(5,7)]G",
        "G[T(5,7)]A",
        "G[T(5,7)]C",
        "G[T(5,7)]G",
        "A[T(8,)]A",
        "A[T(8,)]C",
        "A[T(8,)]G",
        "C[T(8,)]A",
        "C[T(8,)]C",
        "C[T(8,)]G",
        "G[T(8,)]A",
        "G[T(8,)]C",
        "G[T(8,)]G",
        # Insertion labels
        "A[C0]A",
        "A[C0]T",
        "C(0,3)",
        "C(4,6)",
        "C(7,)",
        "A[T(0,4)]A",
        "A[T(0,4)]C",
        "A[T(0,4)]G",
        "C[T(0,4)]A",
        "C[T(0,4)]C",
        "C[T(0,4)]G",
        "G[T(0,4)]A",
        "G[T(0,4)]C",
        "G[T(0,4)]G",
        "A[T(5,7)]A",
        "A[T(5,7)]C",
        "A[T(5,7)]G",
        "C[T(5,7)]A",
        "C[T(5,7)]C",
        "C[T(5,7)]G",
        "G[T(5,7)]A",
        "G[T(5,7)]C",
        "G[T(5,7)]G",
        "A[T(8,)]A",
        "A[T(8,)]C",
        "A[T(8,)]G",
        "C[T(8,)]A",
        "C[T(8,)]C",
        "C[T(8,)]G",
        "G[T(8,)]A",
        "G[T(8,)]C",
        "G[T(8,)]G",
        # Longer del/ins/MH/complex
        "L(2,4):R1",
        "L(5, ):R1",
        "L(2,8):U(1,2):R(2,4)",
        "L(2, ):U(1,2):R(5,)",
        "L(3, ):U(3,):R2",
        "L(3, ):U(3,):R(3,)",
        "L(2,4):R0",
        "L(5, ):R0",
        "L(2,4):R1",
        "L(5, ):R1",
        "L(2, ):R(2,4)",
        "L(2, ):R(5,)",
        "L(2,5):M1",
        "L(3,5):M2",
        "L(4,5):M(3,4)",
        "L(6, ):M1",
        "L(6, ):M2",
        "L(6, ):M3",
        "L(6, ):M(4, )",
        "Complex"
      )
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
  my_vector <- indel_type_4_figurelabel$IndelType
  muts_basis <- data.frame(Sample = catalog, IndelType = my_vector)
  muts_basis_melt <- reshape2::melt(muts_basis, "IndelType")
  muts_basis_melt <- merge(
    indel_type_4_figurelabel,
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
    "SubIndel",
    "Sample",
    "freq"
  )
  muts_basis_melt$Sample <- as.character(muts_basis_melt$Sample)

  if (!is.null(ylim) && ylim < 1.1 * max(muts_basis_melt$freq)) {
    ylim <- 1.1 * max(muts_basis_melt$freq)
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
  blocks$ymin <- ifelse(
    !is.null(ylim),
    ylim,
    max(muts_basis_melt$freq)
  ) *
    top_bar_mult[1]
  blocks$ymax <- ifelse(
    !is.null(ylim),
    ylim,
    max(muts_basis_melt$freq)
  ) *
    top_bar_mult[2]
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
    "black",
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
    ggplot2::ggtitle(plot_title) +
    ggplot2::scale_fill_manual(values = indel_mypalette_fill_all) +
    ggplot2::scale_y_continuous(
      limits = c(
        min(0, min(muts_basis_melt$freq) * 1.05),
        if (upper) {
          ifelse(!is.null(ylim), ylim * top_bar_mult[2], unique(blocks$ymax))
        } else {
          ifelse(
            !is.null(ylim),
            ylim * 1.05,
            max(muts_basis_melt$freq) * 1.05
          )
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
          size = rel(axis_text_x_cex),
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
        size = rel(axis_text_y_cex),
        colour = "black"
      ),
      legend.position = "none",
      axis.title.x = ggplot2::element_text(
        size = rel(axis_title_x_cex),
        margin = margin(t = ifelse(show_axis_text_x, -12, 1), b = 0)
      ),
      axis.title.y = ggplot2::element_text(size = rel(axis_title_y_cex)),
      plot.title = ggplot2::element_text(size = rel(plot_title_cex))
    ) +
    ggplot2::scale_colour_manual(values = c("black", "white"))

  if (!show_axis_title_x) {
    p <- p + theme(axis.title.x = element_blank())
  }
  if (!show_axis_title_y) {
    p <- p + theme(axis.title.y = element_blank())
  }
  if (!show_axis_text_y) {
    p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
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
        size = class_label_cex * base_size / 11,
        fontface = "bold",
        inherit.aes = FALSE
      )

  }

  # Resolve show_counts: NULL = auto (counts only), TRUE/FALSE = forced
  show_counts <- resolve_show_counts(show_counts, if (ylabel == "Counts") "counts" else "counts.signature")

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
    max_freq <- ifelse(!is.null(ylim), ylim, max(muts_basis_melt$freq))
    count_label_df$y <- max_freq * 0.9

    p <- p +
      ggplot2::geom_text(
        data = count_label_df,
        ggplot2::aes(x = x, y = y, label = count),
        size = count_label_cex * base_size / ggplot2::.pt,
        inherit.aes = FALSE
      )
  }

  p <- add_peak_labels(p, muts_basis_melt, "IndelType", "freq", "Figlabel",
                       num_peak_labels = num_peak_labels,
                       peak_label_cex = peak_label_cex,
                       base_size = base_size)

  return(p)
}
