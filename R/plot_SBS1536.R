#' @rdname heatmap_plots
#'
#' @examples
#' set.seed(1)
#' sig <- runif(1536)
#' sig <- sig / sum(sig)
#' names(sig) <- catalog_row_order()$SBS1536
#' plot_SBS1536(sig, plot_title = "Example SBS1536")
#'
#' @export
#'
#' @import ggplot2
#' @importFrom dplyr arrange desc %>%
#' @importFrom patchwork wrap_plots plot_annotation
plot_SBS1536 <- function(
  catalog,
  plot_title = NULL,
  base_size = 11,
  plot_title_cex = 1.2,
  axis_text_cex = 0.8,
  strip_text_cex = 1.0,
  show_axis_text_x = TRUE,
  show_axis_text_y = TRUE,
  show_axis_title_x = TRUE,
  show_axis_title_y = TRUE
) {
  catalog <- normalize_catalog(catalog, 1536, catalog_row_order()$SBS1536, "SBS1536")
  if (is.null(catalog)) return(NULL)
  if (is.null(plot_title)) plot_title <- colnames(catalog)[1] %||% ""

  base_mm <- base_mm(base_size)

  # Base colors for axis labels
  bases <- c("A", "C", "G", "T")
  base_cols <- c("forestgreen", "dodgerblue2", "black", "red")

  # Detect catalog type
  catalog_type <- detect_y_axis_type(catalog[, 1], attributes(catalog)$y_axis_type_attr)

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
                                  size = strip_text_cex * base_size),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
      )

    # Add base labels on x-axis (top): two rows
    if (show_axis_text_x) {
      x_label_1 <- rep(bases, each = 4)
      x_label_2 <- rep(bases, times = 4)
      p <- p +
        annotate("text", x = 1:16, y = 17.3,
                 label = x_label_1,
                 color = rep(base_cols, each = 4),
                 fontface = "bold",
                 size = axis_text_cex * base_mm) +
        annotate("text", x = 1:16, y = 18.3,
                 label = x_label_2,
                 color = rep(base_cols, times = 4),
                 fontface = "bold",
                 size = axis_text_cex * base_mm)
    }

    # Add base labels on y-axis (left): two columns
    if (show_axis_text_y) {
      y_label_1 <- rep(bases, each = 4)
      y_label_2 <- rep(bases, times = 4)
      p <- p +
        annotate("text", x = -0.3, y = 16:1,
                 label = y_label_1,
                 color = rep(base_cols, each = 4),
                 fontface = "bold",
                 size = axis_text_cex * base_mm) +
        annotate("text", x = -1.3, y = 16:1,
                 label = y_label_2,
                 color = rep(base_cols, times = 4),
                 fontface = "bold",
                 size = axis_text_cex * base_mm)
    }

    panel_list[[idx]] <- p
  }

  # Arrange 2x3 grid with title and caption
  patchwork::wrap_plots(panel_list, nrow = 2, ncol = 3) +
    patchwork::plot_annotation(
      title = plot_title,
      caption = "X: 3' context (1bp inner, 2bp outer)    Y: 5' context (1bp inner, 2bp outer)",
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(
          size = plot_title_cex * base_size, face = "bold", hjust = 0.5
        ),
        plot.caption = ggplot2::element_text(
          size = base_size * 0.8, hjust = 0.5
        )
      )
    )
}
