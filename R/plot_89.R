#' Plot 89-channel indel profile
#'
#' Creates a bar plot visualization of an 89-channel indel mutational signature
#' for a single sample. The plot shows mutation counts or proportions across
#' all 89 indel categories with color-coded category blocks.
#'
#' @param catalog Numeric vector of length 89 containing indel counts or
#'   proportions for a single sample.
#' @param text_cex Numeric. Size of text labels in the plot. Used as default
#'   for `top_bar_text_cex`.
#' @param top_bar_text_cex Numeric. Size of labels in the colored top bars
#'   (both the category bar and the extra summary bar).
#' @param title_text_cex Numeric. Size of the plot title text, relative to `base_size`.
#' @param plot_title Character. Title displayed above the plot.
#' @param setyaxis Numeric or NULL. If provided, sets a fixed y-axis maximum.
#'   If NULL, y-axis scales automatically to the data.
#' @param ylabel Character. Label for the y-axis.
#' @param base_size Base font size for ggplot2's `theme_classic()`.
#' @param x_axis_tick_label_cex Numeric. Size of x-axis tick labels, relative to `base_size`.
#' @param y_axis_tick_label_cex Numeric. Size of y-axis tick labels, relative to `base_size`.
#' @param x_title_cex Numeric. Size of x-axis title, relative to `base_size`.
#' @param y_title_cex Numeric. Size of y-axis title, relative to `base_size`.
#' @param show_x_axis_text Logical. If `TRUE`, display x-axis tick labels.
#' @param show_top_bar Logical. If `TRUE`, display the category bar above the
#'   plot (e.g., "Del 1bp C", "Ins 1bp T").
#' @param show_extra_top_bar Logical. If `TRUE`, display the extra summary bar
#'   above the category bar (e.g., "Del", "Ins"). Only shown if `show_top_bar`
#'   is also `TRUE`. This is really for backward compatibility.
#' @param plot_complex Logical. If `TRUE`, include the Complex indel channel.
#' @param show_counts Logical or NULL. If `TRUE`, always display per-class
#'   mutation count labels. If `FALSE`, never display them. If `NULL`
#'   (the default), display them only when the catalog contains counts
#'   (sum > 1.1).
#' @param count_label_cex Numeric. Size of per-class count labels, as a
#'   fraction of `base_size`.
#' @param text_size Deprecated. Use `text_cex` instead.
#' @param top_bar_text_size Deprecated. Use `top_bar_text_cex` instead.
#' @param title_text_size Deprecated. Use `title_text_cex` instead.
#' @param x_axis_tick_label_size Deprecated. Use `x_axis_tick_label_cex` instead.
#' @param y_axis_tick_label_size Deprecated. Use `y_axis_tick_label_cex` instead.
#' @param x_title_size Deprecated. Use `x_title_cex` instead.
#' @param y_title_size Deprecated. Use `y_title_cex` instead.
#' @param count_label_size Deprecated. Use `count_label_cex` instead.
#'
#' @return A ggplot2 object containing the 89-channel indel profile plot.
#'
#' @export
#'
#' @import ggplot2 reshape2 dplyr ggrepel
#'
plot_89 <- function(
  catalog,
  text_cex = 3,
  top_bar_text_cex = text_cex,
  title_text_cex = 1.0,
  plot_title = colnames(catalog)[1],
  setyaxis = NULL,
  ylabel = NULL,
  base_size = 11,
  x_axis_tick_label_cex = 0.6,
  y_axis_tick_label_cex = 0.8,
  x_title_cex = 0.9,
  y_title_cex = 0.9,
  show_x_axis_text = TRUE,
  show_top_bar = TRUE,
  show_extra_top_bar = FALSE,
  plot_complex = FALSE,
  show_counts = NULL,
  count_label_cex = 1.03,
  text_size = NULL,
  top_bar_text_size = NULL,
  title_text_size = NULL,
  x_axis_tick_label_size = NULL,
  y_axis_tick_label_size = NULL,
  x_title_size = NULL,
  y_title_size = NULL,
  count_label_size = NULL
) {
  # === start dealing with deprecated
  if (!is.null(text_size)) {
    if (!missing(text_cex)) {
      stop("Cannot specify both 'text_size' and 'text_cex'.")
    }
    warning("'text_size' is deprecated. Use 'text_cex' instead.")
    text_cex <- text_size
  }
  if (!is.null(top_bar_text_size)) {
    if (!missing(top_bar_text_cex)) {
      stop("Cannot specify both 'top_bar_text_size' and 'top_bar_text_cex'.")
    }
    warning("'top_bar_text_size' is deprecated. Use 'top_bar_text_cex' instead.")
    top_bar_text_cex <- top_bar_text_size
  }
  if (!is.null(title_text_size)) {
    if (!missing(title_text_cex)) {
      stop("Cannot specify both 'title_text_size' and 'title_text_cex'.")
    }
    warning("'title_text_size' is deprecated. Use 'title_text_cex' instead.")
    title_text_cex <- title_text_size
  }
  if (!is.null(x_axis_tick_label_size)) {
    if (!missing(x_axis_tick_label_cex)) {
      stop("Cannot specify both 'x_axis_tick_label_size' and 'x_axis_tick_label_cex'.")
    }
    warning("'x_axis_tick_label_size' is deprecated. Use 'x_axis_tick_label_cex' instead.")
    x_axis_tick_label_cex <- x_axis_tick_label_size
  }
  if (!is.null(y_axis_tick_label_size)) {
    if (!missing(y_axis_tick_label_cex)) {
      stop("Cannot specify both 'y_axis_tick_label_size' and 'y_axis_tick_label_cex'.")
    }
    warning("'y_axis_tick_label_size' is deprecated. Use 'y_axis_tick_label_cex' instead.")
    y_axis_tick_label_cex <- y_axis_tick_label_size
  }
  if (!is.null(x_title_size)) {
    if (!missing(x_title_cex)) {
      stop("Cannot specify both 'x_title_size' and 'x_title_cex'.")
    }
    warning("'x_title_size' is deprecated. Use 'x_title_cex' instead.")
    x_title_cex <- x_title_size
  }
  if (!is.null(y_title_size)) {
    if (!missing(y_title_cex)) {
      stop("Cannot specify both 'y_title_size' and 'y_title_cex'.")
    }
    warning("'y_title_size' is deprecated. Use 'y_title_cex' instead.")
    y_title_cex <- y_title_size
  }
  if (!is.null(count_label_size)) {
    if (!missing(count_label_cex)) {
      stop("Cannot specify both 'count_label_size' and 'count_label_cex'.")
    }
    warning("'count_label_size' is deprecated. Use 'count_label_cex' instead.")
    count_label_cex <- count_label_size
  }
  # === end dealing with deprecated

  if (is.null(ylabel)) {
    if ((!is.null(setyaxis) && setyaxis > 1.5) ||
        !(sum(catalog) < 1.1 && max(catalog) != 1)) {
      ylabel = "Counts"
    } else {
      ylabel = "Proportion"
    }
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
    ifelse(grepl("R\\(5,7\\)", indel_type_4_figurelabel$IndelType[del_t]),
      "Del(T):R(5,7)", "Del(T):R(8,)")
  )
  ins_t <- indel_type_4_figurelabel$Indel == "Ins(T)"
  indel_type_4_figurelabel$SubIndel[ins_t] <- ifelse(
    grepl("R\\(0,4\\)", indel_type_4_figurelabel$IndelType[ins_t]),
    "Ins(T):R(0,4)",
    ifelse(grepl("R\\(5,7\\)", indel_type_4_figurelabel$IndelType[ins_t]),
      "Ins(T):R(5,7)", "Ins(T):R(8,)")
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

  # === 3. Define Palettes and Block Positions ===
  indel_mypalette_block_fill <- c(
    "#fdbe6f",  # Del(C)
    "#ffb34d",  # Del(T):R(1,4)
    "#ff8001",  # Del(T):R(5,7)
    "#cc6600",  # Del(T):R(8,)
    "#b0dd8b",  # Ins(C)
    "#6dcf65",  # Ins(T):R(0,4)
    "#36a12e",  # Ins(T):R(5,7)
    "#1a7a14",  # Ins(T):R(8,)
    "#f14432",  # Del(2,):R(0,9)
    "#4a98c9",  # Ins(2,)
    "#61409b",  # Del(2,):M(1,)
    "#000000"   # Complex
  )

  indel_positions <- indel_type_4_figurelabel$IndelType
  indel_positions_labels <- indel_type_4_figurelabel$Figlabel

  entry <- table(indel_type_4_figurelabel$SubIndel)
  order_entry <- c(
    "Del(C)",
    "Del(T):R(1,4)", "Del(T):R(5,7)", "Del(T):R(8,)",
    "Ins(C)",
    "Ins(T):R(0,4)", "Ins(T):R(5,7)", "Ins(T):R(8,)",
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

  blocks$ymin <- ifelse(
    !is.null(setyaxis),
    setyaxis,
    max(muts_basis_melt$freq)
  ) * 1.08
  blocks$ymax <- ifelse(
    !is.null(setyaxis),
    setyaxis,
    max(muts_basis_melt$freq)
  ) * 1.2
  blocks$labels <- c(
    "Del 1 C",
    "Del 1 T (2-4)", "Del 1 T (5-7)", "Del 1 T (8+)",
    "Ins 1 C",
    "Ins 1 T (0-4)", "Ins 1 T (5-7)", "Ins 1 T (8+)",
    "Del \u22652",
    "Ins \u22652",
    "Del Mh",
    "X"
  )
  blocks$cl <- c(
    "black",
    "black", "black", "white",
    "black",
    "black", "black", "white",
    "white",
    "white",
    "white",
    "white"
  )

  # Grey blocks for insertion/deletion/complex
  indel_mypalette_fill3 <- c("#000000", "#888888", "#DDDDDD")

  # === 4. Prepare Block3 for Overhead Labels ===
  blocks3 <- blocks %>%
    dplyr::mutate(
      Type = ifelse(
        Type == "Del(C)" | grepl("^Del\\(T\\)", Type), "Del1", Type
      )
    ) %>%
    dplyr::mutate(
      Type = ifelse(
        Type == "Ins(C)" | grepl("^Ins\\(T\\)", Type), "Ins1", Type
      )
    ) %>%
    dplyr::group_by(Type) %>%
    dplyr::summarise(xmin = min(xmin), xmax = max(xmax)) %>%
    dplyr::mutate(labels = substr(Type, 1, 3)) %>%
    dplyr::mutate(labels = ifelse(labels == "Com", "X", labels)) %>%
    dplyr::arrange(xmin)
  blocks3$ymin <- ifelse(
    !is.null(setyaxis),
    setyaxis,
    max(muts_basis_melt$freq)
  ) * 1.2
  blocks3$ymax <- ifelse(
    !is.null(setyaxis),
    setyaxis,
    max(muts_basis_melt$freq)
  ) * 1.32
  blocks3$cl <- "white"
  blocks3$Type <- c("Del1", "Ins1", "Del2", "Ins2", "DelMH", "X")
  blocks3$cl[1:2] <- "black"

  indel_mypalette_fill_all <- c(
    "Del1" = "#fe9f38",
    "Ins1" = "#73bf5d",
    "Del2" = "#f14432",
    "Ins2" = "#4a98c9",
    "DelMH" = "#61409b",
    "X" = "black",
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
    blocks3 <- blocks3[blocks3$Type != "X", ]
    indel_mypalette_fill_all <- indel_mypalette_fill_all[
      !names(indel_mypalette_fill_all) %in% c("Complex", "X")
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
        0,
        if (show_top_bar && show_extra_top_bar) {
          ifelse(!is.null(setyaxis), setyaxis * 1.32, unique(blocks3$ymax))
        } else if (show_top_bar) {
          ifelse(!is.null(setyaxis), setyaxis * 1.2, unique(blocks$ymax))
        } else {
          ifelse(
            !is.null(setyaxis),
            setyaxis * 1.05,
            max(muts_basis_melt$freq) * 1.05
          )
        }
      ),
      labels = scales::number_format(
        accuracy = if (ylabel == "Counts") 1 else 0.01
      ),
      expand = c(0, 0)
    ) +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      axis.text.x = if (show_x_axis_text) {
        ggplot2::element_text(
          angle = 90,
          vjust = 0.5,
          size = rel(x_axis_tick_label_cex),
          colour = "black",
          hjust = 1
        )
      } else {
        ggplot2::element_blank()
      },
      axis.ticks.x = if (show_x_axis_text) {
        ggplot2::element_line()
      } else {
        ggplot2::element_blank()
      },
      axis.text.y = ggplot2::element_text(
        size = rel(y_axis_tick_label_cex),
        colour = "black"
      ),
      legend.position = "none",
      axis.title.x = ggplot2::element_text(
        size = rel(x_title_cex),
        margin = margin(t = ifelse(show_x_axis_text, -12, 1), b = 0)
      ),
      axis.title.y = ggplot2::element_text(size = rel(y_title_cex)),
      plot.title = ggplot2::element_text(size = rel(title_text_cex))
    ) +
    ggplot2::scale_colour_manual(values = c("black", "white"))

  # Add top bar conditionally
  if (show_top_bar) {
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
        size = top_bar_text_cex * base_size / 11,
        fontface = "bold",
        inherit.aes = FALSE
      )

    # Add extra top bar only if requested (e.g., "Del", "Ins")
    if (show_extra_top_bar) {
      p <- p +
        ggplot2::geom_rect(
          data = blocks3,
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
          data = blocks3,
          ggplot2::aes(
            x = (xmax + xmin) / 2,
            y = (ymax + ymin) / 2,
            label = labels,
            colour = cl
          ),
          size = top_bar_text_cex * base_size / 11,
          fontface = "bold",
          inherit.aes = FALSE
        )
    }
  }

  # Resolve show_counts: NULL = auto (counts only), TRUE/FALSE = forced
  if (is.null(show_counts)) {
    show_counts <- (ylabel == "Counts")
  }

  # Add count labels
  if (show_counts) {
    counts_by_block <- aggregate(
      abs(freq) ~ SubIndel,
      data = muts_basis_melt,
      FUN = sum
    )
    names(counts_by_block) <- c("Type", "count")
    counts_by_block$count <- ifelse(counts_by_block$count > 0 & counts_by_block$count < 1,
      signif(counts_by_block$count, 2), round(counts_by_block$count))
    count_label_df <- merge(blocks, counts_by_block, by = "Type")
    count_label_df$x <- (count_label_df$xmin + count_label_df$xmax) / 2
    max_freq <- ifelse(!is.null(setyaxis), setyaxis, max(muts_basis_melt$freq))
    count_label_df$y <- max_freq * 0.9

    p <- p +
      ggplot2::geom_text(
        data = count_label_df,
        ggplot2::aes(x = x, y = y, label = count),
        size = count_label_cex * base_size / ggplot2::.pt,
        inherit.aes = FALSE
      )
  }

  return(p)
}
