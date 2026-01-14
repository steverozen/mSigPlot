#' Plot 89-channel indel profile
#'
#' Creates a bar plot visualization of an 89-channel indel mutational signature
#' for a single sample. The plot shows mutation counts or proportions across
#' all 89 indel categories with color-coded category blocks.
#'
#' @param catalog Numeric vector of length 89 containing indel counts or
#'   proportions for a single sample.
#' @param text_size Numeric. Size of text labels in the plot. Default is 3.
#' @param plot_title Character. Title displayed above the plot. Default is "ID89".
#' @param setyaxis Numeric or NULL. If provided, sets a fixed y-axis maximum.
#'   If NULL, y-axis scales automatically to the data. Default is NULL.
#' @param ylabel Character. Label for the y-axis. Default is "Counts".
#' @param base_size Base font size for ggplot2's `theme_classic()`.
#' @param show_x_axis_text Logical. If `TRUE`, display x-axis tick labels.
#' @param show_top_bar Logical. If `TRUE`, display the category bar above the
#'   plot (e.g., "Del 1bp C", "Ins 1bp T").
#' @param show_extra_top_bar Logical. If `TRUE`, display the extra summary bar
#'   above the category bar (e.g., "Del", "Ins"). Only shown if `show_top_bar`
#'   is also `TRUE`. This is really for backward compatibility.
#'
#' @return A ggplot2 object containing the 89-channel indel profile plot.
#'
#' @export
#'
#' @import ggplot2 reshape2 dplyr ggrepel
#'
plot_89 <- function(
  catalog,
  text_size = 3,
  plot_title = colnames(catalog)[1],
  setyaxis = NULL,
  ylabel = NULL,
  base_size = 11,
  show_x_axis_text = TRUE,
  show_top_bar = TRUE,
  show_extra_top_bar = FALSE
) {
  if (is.null(ylabel)) {
    if (sum(catalog) < 1.1 && max(catalog) != 1) {
      ylabel = "Proportion"
    } else {
      ylabel = "Counts"
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
    "Sample",
    "freq"
  )
  muts_basis_melt$Sample <- as.character(muts_basis_melt$Sample)

  # === 3. Define Palettes and Block Positions ===
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

  indel_positions <- indel_type_4_figurelabel$IndelType
  indel_positions_labels <- indel_type_4_figurelabel$Figlabel

  entry <- table(indel_type_4_figurelabel$Indel)
  order_entry <- c(
    "Del(C)",
    "Del(T)",
    "Ins(C)",
    "Ins(T)",
    "Del(2,):R(0,9)",
    "Ins(2,)",
    "Del(2,):M(1,)",
    "Complex"
  )
  entry <- entry[order_entry]
  blocks <- data.frame(
    Type = unique(indel_type_4_figurelabel$Indel),
    fill = indel_mypalette_fill,
    xmin = c(0, cumsum(entry)[-length(entry)]) + 0.5,
    xmax = cumsum(entry) + 0.5
  )
  # blocks$ymin <- max(muts_basis_melt$freq) * 1.08
  #  blocks$ymax <- max(muts_basis_melt$freq) * 1.2

  blocks$ymin <- ifelse(
    !is.null(setyaxis),
    setyaxis,
    max(muts_basis_melt$freq)
  ) *
    1.08
  blocks$ymax <- ifelse(
    !is.null(setyaxis),
    setyaxis,
    max(muts_basis_melt$freq)
  ) *
    1.2
  blocks$labels <- c(
    "Del 1bp C",
    "Del 1bp T",
    "Ins 1bp C",
    "Ins 1bp T",
    "Del \u22652bp",
    "Ins \u22652bp",
    "Del Mh",
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

  # Grey blocks for insertion/deletion/complex
  indel_mypalette_fill3 <- c("#000000", "#888888", "#DDDDDD")

  # === 4. Prepare Block3 for Overhead Labels ===
  blocks3 <- blocks %>%
    dplyr::mutate(
      Type = ifelse(Type %in% c("Del(C)", "Del(T)"), "Del1", Type)
    ) %>%
    dplyr::mutate(
      Type = ifelse(Type %in% c("Ins(C)", "Ins(T)"), "Ins1", Type)
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
  ) *
    1.2
  blocks3$ymax <- ifelse(
    !is.null(setyaxis),
    setyaxis,
    max(muts_basis_melt$freq)
  ) *
    1.32
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
    "Complex" = "black"
  )

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
          size = rel(0.6),
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
      axis.text.y = ggplot2::element_text(size = rel(0.8), colour = "black"),
      legend.position = "none",
      axis.title.x = ggplot2::element_text(
        size = rel(0.9),
        margin = margin(t = ifelse(show_x_axis_text, -12, 1), b = 0)
      ),
      axis.title.y = ggplot2::element_text(size = rel(0.9))
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
        size = text_size * base_size / 11,
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
          size = text_size * base_size / 11,
          fontface = "bold",
          inherit.aes = FALSE
        )
    }
  }

  return(p)
}
