#' Plot an SBS1536 catalog as a 2x3 heatmap grid using ggplot2
#'
#' Creates a 2x3 grid of 16x16 heatmaps (one per mutation type: C>A, C>G,
#' C>T, T>A, T>C, T>G) using `geom_tile()` with a white-to-forestgreen
#' color gradient. Axis labels show colored bases (A, C, G, T).
#'
#' @param catalog Numeric vector, single-column data.frame, matrix, tibble,
#'   or data.table.
#' @param plot_title Character. Title displayed at the top center.
#' @param base_size Numeric. Base font size in points.
#' @param title_cex Numeric. Multiplier for the plot title size.
#' @param axis_label_cex Numeric. Multiplier for axis base labels.
#' @param panel_label_cex Numeric. Multiplier for panel mutation type labels.
#'
#' @return A ggplot object (assembled via patchwork or gridExtra).
#'
#' @export
#'
#' @import ggplot2 dplyr
#' @importFrom gridExtra grid.arrange
plot_SBS1536 <- function(
  catalog,
  plot_title = NULL,
  base_size = 11,
  title_cex = 1.2,
  axis_label_cex = 0.8,
  panel_label_cex = 1.0
) {
  catalog <- normalize_catalog(catalog, 1536, catalog_row_order()$SBS1536, "SBS1536")
  if (is.null(catalog)) return(NULL)
  if (is.null(plot_title)) plot_title <- colnames(catalog)[1] %||% ""

  base_mm <- base_size / (72.27 / 25.4)

  # Base colors for axis labels
  bases <- c("A", "C", "G", "T")
  base_cols <- c("forestgreen", "dodgerblue2", "black", "red")

  # Detect catalog type
  catalog_type <- attributes(catalog)$catalog.type
  if (is.null(catalog_type)) {
    if (sum(abs(catalog[, 1])) >= 1.1) {
      catalog_type <- "counts"
    } else {
      catalog_type <- "counts.signature"
    }
  }

  # Sort catalog in plotting order
  rates <- data.frame(value = catalog[, 1], stringsAsFactors = FALSE)
  mut_type <- rownames(catalog)
  rates$mut_type <- mut_type
  rates$ref2alt <- paste0(substr(mut_type, 3, 3), substr(mut_type, 6, 6))
  rates$minus1bs <- substr(mut_type, 2, 2)
  rates$minus2bs <- substr(mut_type, 1, 1)
  rates$plus1bs <- substr(mut_type, 4, 4)
  rates$plus2bs <- substr(mut_type, 5, 5)
  rates <- rates %>%
    arrange(ref2alt, desc(minus1bs), desc(minus2bs), plus1bs, plus2bs)

  # Main mutation types
  main_mut_type <- paste0(substr(rates$mut_type, 3, 3), ">",
                          substr(rates$mut_type, 6, 6))
  main_types <- unique(main_mut_type)

  # Calculate totals per type
  type_totals <- tapply(rates$value, main_mut_type, sum)

  # Create one heatmap panel per mutation type
  panel_list <- list()

  for (idx in seq_along(main_types)) {
    mt <- main_types[idx]
    sub <- rates[main_mut_type == mt, ]

    # Build 16x16 matrix: x = 3' context (plus2, plus1), y = 5' context (minus1, minus2)
    # Each row of sub corresponds to one cell
    # The ordering after arrange gives us: rows go by minus1(desc), minus2(desc)
    # cols go by plus1, plus2
    tile_df <- data.frame(
      x = rep(1:16, each = 16),
      y = rep(16:1, times = 16),
      value = sub$value,
      stringsAsFactors = FALSE
    )

    # Panel title
    if (catalog_type == "counts") {
      panel_title <- paste0(mt, " (N=", round(type_totals[mt]), ")")
    } else if (catalog_type %in% c("counts.signature", "density.signature")) {
      panel_title <- paste0(mt, " (", round(100 * type_totals[mt], 1), "%)")
    } else {
      panel_title <- mt
    }

    p <- ggplot(tile_df, aes(x = x, y = y, fill = value)) +
      geom_tile(color = NA) +
      scale_fill_gradient(low = "white", high = "forestgreen",
                          guide = "none") +
      # Grid lines at 4-base intervals
      geom_vline(xintercept = c(0.5, 4.5, 8.5, 12.5, 16.5),
                 color = "black", linewidth = 0.3) +
      geom_hline(yintercept = c(0.5, 4.5, 8.5, 12.5, 16.5),
                 color = "black", linewidth = 0.3) +
      coord_fixed() +
      labs(title = panel_title) +
      theme_void(base_size = base_size) +
      theme(
        plot.title = element_text(hjust = 0.5,
                                  size = panel_label_cex * base_size),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
      )

    # Add base labels on x-axis (top): two rows
    # Row 1 (1bp 3'): each group of 4 gets A, C, G, T
    x_label_1 <- rep(bases, each = 4)
    x_label_2 <- rep(bases, times = 4)
    p <- p +
      annotate("text", x = 1:16, y = 17.3,
               label = x_label_1,
               color = rep(base_cols, each = 4),
               fontface = "bold",
               size = axis_label_cex * base_mm) +
      annotate("text", x = 1:16, y = 18.3,
               label = x_label_2,
               color = rep(base_cols, times = 4),
               fontface = "bold",
               size = axis_label_cex * base_mm)

    # Add base labels on y-axis (left): two columns
    y_label_1 <- rep(bases, each = 4)
    y_label_2 <- rep(bases, times = 4)
    p <- p +
      annotate("text", x = -0.3, y = 16:1,
               label = y_label_1,
               color = rep(base_cols, each = 4),
               fontface = "bold",
               size = axis_label_cex * base_mm) +
      annotate("text", x = -1.3, y = 16:1,
               label = y_label_2,
               color = rep(base_cols, times = 4),
               fontface = "bold",
               size = axis_label_cex * base_mm)

    panel_list[[idx]] <- p
  }

  # Arrange 2x3 grid with title
  title_grob <- grid::textGrob(
    plot_title,
    gp = grid::gpar(fontsize = title_cex * base_size, fontface = "bold")
  )

  # Add axis description labels
  subtitle_grob <- grid::textGrob(
    "X: 3' context (1bp inner, 2bp outer)    Y: 5' context (1bp inner, 2bp outer)",
    gp = grid::gpar(fontsize = base_size * 0.8)
  )

  result <- gridExtra::grid.arrange(
    title_grob,
    gridExtra::arrangeGrob(
      grobs = panel_list,
      nrow = 2,
      ncol = 3
    ),
    subtitle_grob,
    nrow = 3,
    heights = c(0.08, 0.87, 0.05)
  )

  invisible(result)
}
