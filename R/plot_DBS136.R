#' plot_DBS136, plot_DBS136_pdf, plot_SBS1536, plot_SBS1536_pdf
#'
#' Plot functions for SBS and DBS mutational signature catalogs
#' as heatmaps. All functions return ggplot2 objects.
#'
#' Functions in this family:
#' - `plot_SBS1536`: SBS pentanucleotide context (1536 channels)
#' - `plot_DBS136`: DBS heatmap (136 channels, 10 4x4 panels)
#'
#' Each has a corresponding `_pdf()` variant for multi-sample PDF export.
#'
#' @param catalog Numeric vector, single-column data.frame, matrix, tibble,
#'   or data.table. If there are row names (or for a vector, names), they
#'   will be checked against [catalog_row_order()].
#' @param plot_title Character. Title displayed above the plot.
#' @param filename Character. Path to the output PDF file (\_pdf functions only).
#' @param base_size Numeric. Base font size in points.
#' @param plot_title_cex Numeric. Multiplier for the plot title size.
#' @param axis_text_cex Numeric. Multiplier for axis label size.
#' @param strip_text_cex Numeric. Multiplier for panel/facet label size.
#' @param show_axis_text_x Logical. If FALSE, hide x-axis base labels.
#' @param show_axis_text_y Logical. If FALSE, hide y-axis base labels.
#' @param show_axis_title_x Logical. If FALSE, hide the x-axis description.
#' @param show_axis_title_y Logical. If FALSE, hide the y-axis description.
#' @param ... Additional arguments passed to the underlying plot function
#'   (\_pdf variants only).
#'
#' @return Plot functions return a ggplot2 object, or NULL with a warning
#'   if the catalog is invalid (wrong size or row names). PDF functions
#'   return NULL invisibly (called for side effect of creating a PDF file),
#'   or stop with an error if the catalog is invalid.
#'
#' @name heatmap_plots
NULL

#' @rdname heatmap_plots
#'
#' @examples
#' set.seed(1)
#' sig <- runif(136)
#' sig <- sig / sum(sig)
#' names(sig) <- catalog_row_order()$DBS136
#' plot_DBS136(sig, plot_title = "Example DBS136")
#'
#' @export
#'
#' @import ggplot2 dplyr
#' @importFrom gridExtra grid.arrange arrangeGrob
plot_DBS136 <- function(
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
  catalog <- normalize_catalog(catalog, 136, catalog_row_order()$DBS136, "DBS136")
  if (is.null(catalog)) return(NULL)
  if (is.null(plot_title)) plot_title <- colnames(catalog)[1] %||% ""

  base_mm <- base_mm(base_size)

  bases <- c("A", "C", "G", "T")
  base_cols <- c("forestgreen", "dodgerblue2", "black", "red")

  ref_order <- c("AC", "AT", "GC", "CC", "CG", "CT", "TA", "TC", "TG", "TT")
  mut_type_labels <- paste0(ref_order, ">NN")

  # Detect catalog type
  catalog_type <- detect_catalog_type(catalog[, 1], attributes(catalog)$catalog.type)

  # Map catalog to 160-position layout using order_DBS136_for_plotting
  plotting_order <- order_DBS136_for_plotting()
  values_160 <- numeric(160)
  for (i in 1:160) {
    if (plotting_order[i] %in% rownames(catalog)) {
      values_160[i] <- catalog[plotting_order[i], 1]
    } else {
      values_160[i] <- NA
    }
  }

  # Calculate per-class statistics
  df_stats <- data.frame(value = values_160[!is.na(values_160)],
                          stringsAsFactors = FALSE)
  df_stats$ref <- substr(plotting_order[!is.na(values_160)], 2, 3)

  if (catalog_type == "counts") {
    max_per_class <- tapply(df_stats$value, df_stats$ref, max)
    sum_per_class <- tapply(df_stats$value, df_stats$ref, sum)
  } else if (catalog_type == "density") {
    max_per_class <- tapply(df_stats$value * 1e6, df_stats$ref, max)
    max_per_class <- round(max_per_class, 3)
  }

  # Create one heatmap panel per class
  panel_list <- list()

  for (idx in 1:10) {
    chunk <- values_160[((idx - 1) * 16 + 1):(idx * 16)]

    tile_df <- data.frame(
      x = rep(1:4, each = 4),
      y = rep(4:1, times = 4),
      value = chunk,
      stringsAsFactors = FALSE
    )

    # Panel title with counts
    ptitle <- mut_type_labels[idx]
    if (catalog_type == "counts" && ref_order[idx] %in% names(sum_per_class)) {
      ptitle <- paste0(ptitle, " (", round(sum_per_class[ref_order[idx]]), ")")
    }

    # Determine fill color range
    non_na <- chunk[!is.na(chunk)]
    if (length(non_na) == 0 || max(non_na) == 0) {
      fill_high <- "white"
    } else {
      fill_high <- "forestgreen"
    }

    p <- ggplot(tile_df, aes(x = x, y = y, fill = value)) +
      geom_tile(color = NA) +
      scale_fill_gradient(low = "white", high = fill_high,
                          na.value = "grey80", guide = "none") +
      # Border
      annotate("rect", xmin = 0.5, xmax = 4.5, ymin = 0.5, ymax = 4.5,
               fill = NA, color = "black", linewidth = 0.5) +
      coord_fixed() +
      labs(title = ptitle) +
      theme_void(base_size = base_size) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold",
                                  size = strip_text_cex * base_size * 0.9),
        plot.margin = margin(t = 3, r = 3, b = 3, l = 3)
      )

    # Axis labels
    if (show_axis_text_x) {
      p <- p +
        annotate("text", x = 1:4, y = 5, label = bases,
                 color = base_cols, fontface = "bold",
                 size = axis_text_cex * base_mm)
    }
    if (show_axis_text_y) {
      p <- p +
        annotate("text", x = 0, y = 4:1, label = bases,
                 color = base_cols, fontface = "bold",
                 size = axis_text_cex * base_mm)
    }

    panel_list[[idx]] <- p
  }

  # Create maxima panel
  if (catalog_type == "counts") {
    maxima_text <- paste(ref_order, "=",
                         sapply(ref_order, function(r) {
                           if (r %in% names(max_per_class)) max_per_class[r]
                           else 0
                         }))
    maxima_unit <- "(counts)"
  } else if (catalog_type == "density") {
    maxima_text <- paste(ref_order, "=",
                         sapply(ref_order, function(r) {
                           if (r %in% names(max_per_class)) max_per_class[r]
                           else 0
                         }))
    maxima_unit <- "(mut/million)"
  } else {
    maxima_text <- rep("", 10)
    maxima_unit <- ""
  }

  maxima_panel <- ggplot() +
    annotate("text", x = 0.5, y = 0.95, label = "Maxima per class",
             size = base_mm * 1.2, fontface = "bold") +
    annotate("text", x = 0.5, y = 0.88, label = maxima_unit,
             size = base_mm * 0.9) +
    annotate("text", x = rep(c(0.15, 0.55), each = 5),
             y = rep(seq(0.78, 0.38, length.out = 5), 2),
             label = maxima_text,
             hjust = 0, size = base_mm * 0.9) +
    xlim(0, 1) + ylim(0, 1) +
    theme_void()

  # Layout: 3 rows x 4 cols, with maxima panel in position 11
  # Row 1: panels 7,8,9,10 (TA, TC, TG, TT)
  # Row 2: panels 4,5,6,maxima (CC, CG, CT, maxima)
  # Row 3: panels 1,2,3,maxima (AC, AT, GC, maxima)
  # Following standard DBS136 heatmap layout
  all_panels <- c(panel_list, list(maxima_panel))

  title_grob <- grid::textGrob(
    plot_title,
    gp = grid::gpar(fontsize = plot_title_cex * base_size, fontface = "bold")
  )

  layout_matrix <- rbind(
    c(7, 8, 9, 10),
    c(4, 5, 6, 11),
    c(1, 2, 3, 11)
  )

  result <- gridExtra::grid.arrange(
    title_grob,
    gridExtra::arrangeGrob(
      grobs = all_panels,
      layout_matrix = layout_matrix
    ),
    nrow = 2,
    heights = c(0.06, 0.94)
  )

  invisible(result)
}
