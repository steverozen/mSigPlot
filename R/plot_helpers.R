#' Internal helper functions shared across plot functions
#'
#' @keywords internal
#' @name plot_helpers
NULL

# Package-level cache environment
.pkg_cache <- new.env(parent = emptyenv())


#' Convert base_size (points) to mm for geom_text sizing
#'
#' @param base_size Numeric base font size in points.
#' @return Numeric base size in mm.
#' @keywords internal
base_mm <- function(base_size) {
  base_size / (72.27 / 25.4)
}


#' Detect catalog type from data and attributes
#'
#' Determines whether the catalog represents counts, counts.signature,
#' density, or density.signature. Checks the `catalog.type` attribute first;
#' if absent, infers from data values and optional ylim.
#'
#' @param values Numeric vector of catalog values.
#' @param catalog_type_attr The `catalog.type` attribute from the catalog, or NULL.
#' @param ylim Optional y-axis limits vector.
#' @return Character string: one of "counts", "counts.signature", "density",
#'   or "density.signature".
#' @keywords internal
detect_catalog_type <- function(values, catalog_type_attr = NULL, ylim = NULL) {
  if (!is.null(catalog_type_attr)) return(catalog_type_attr)
  if (!is.null(ylim) && max(ylim) > 1.5) return("counts")
  if (sum(abs(values), na.rm = TRUE) >= 1.1) "counts" else "counts.signature"
}


#' Resolve show_counts parameter
#'
#' If `show_counts` is NULL (auto), returns TRUE when catalog_type is "counts",
#' FALSE otherwise. If explicitly set, returns the value unchanged.
#'
#' @param show_counts NULL, TRUE, or FALSE.
#' @param catalog_type Character catalog type string.
#' @return Logical.
#' @keywords internal
resolve_show_counts <- function(show_counts, catalog_type) {
  if (is.null(show_counts)) return(catalog_type == "counts")
  show_counts
}


#' Add peak labels with arrows to a ggplot
#'
#' Adds `ggrepel::geom_text_repel()` labels with closed arrowhead segments
#' pointing from labels to the top bars in a bar plot. Used to identify
#' dominant channels.
#'
#' @param plot A ggplot object to add labels to.
#' @param df Data frame containing the plot data.
#' @param x_col Character name of the x-position column in `df`.
#' @param y_col Character name of the y-value column in `df`.
#' @param label_col Character name of the label column in `df`.
#' @param num_peak_labels Integer number of top bars to label (0 = none).
#' @param peak_label_cex Numeric multiplier for label text size.
#' @param base_size Numeric base font size.
#' @param arrow_length Numeric arrow length in npc units.
#' @param label_threshold_denominator Numeric; only bars with value >
#'   max / denominator are candidates for labeling.
#'
#' @return The ggplot object with labels added (or unchanged if num_peak_labels == 0).
#' @keywords internal
#'
#' @importFrom ggrepel geom_text_repel
add_peak_labels <- function(
    plot, df, x_col, y_col, label_col,
    num_peak_labels = 0,
    peak_label_cex = 0.7,
    base_size = 11,
    arrow_length = 0.01,
    label_threshold_denominator = 7
) {
  if (num_peak_labels == 0) return(plot)

  threshold <- max(abs(df[[y_col]]), na.rm = TRUE) / label_threshold_denominator
  candidates <- df[abs(df[[y_col]]) > threshold, ]
  candidates <- candidates[order(-abs(candidates[[y_col]])), ]
  label_data <- utils::head(candidates, num_peak_labels)

  if (nrow(label_data) == 0) return(plot)

  plot + ggrepel::geom_text_repel(
    data = label_data,
    ggplot2::aes(
      x = .data[[x_col]],
      y = .data[[y_col]],
      label = .data[[label_col]]
    ),
    size = peak_label_cex * base_size / ggplot2::.pt,
    nudge_y = max(abs(df[[y_col]]), na.rm = TRUE) * 0.1,
    direction = "both",
    segment.color = "gray50",
    segment.size = 0.13 * base_size / ggplot2::.pt,
    arrow = grid::arrow(
      length = grid::unit(arrow_length, "npc"),
      type = "closed"
    ),
    max.overlaps = 50,
    min.segment.length = 0,
    box.padding = 1,
    point.padding = 0.1,
    force = 2,
    force_pull = 0.5,
    inherit.aes = FALSE
  )
}


#' Generic multi-sample PDF export for bar plots
#'
#' Creates a multi-page PDF with plots arranged in a grid. Each page
#' contains up to `plots_per_page` plots in a single column.
#'
#' @param catalog Multi-column matrix or data.frame of catalogs.
#' @param filename Character path to output PDF file.
#' @param plot_fn The single-sample plotting function to call for each column.
#' @param plots_per_page Integer number of plots per page.
#' @param width Numeric PDF page width in inches.
#' @param height Numeric PDF page height in inches.
#' @param ... Additional arguments passed to `plot_fn`.
#'
#' @return NULL invisibly.
#' @keywords internal
#'
#' @importFrom gridExtra grid.arrange
#' @importFrom Cairo CairoPDF
#' @importFrom grDevices dev.off
plot_catalog_pdf <- function(
    catalog, filename, plot_fn,
    plots_per_page = 5,
    width = 12, height = 14,
    ...
) {
  n_samples <- ncol(catalog)

  Cairo::CairoPDF(file = filename, width = width, height = height)
  on.exit(grDevices::dev.off())

  if (plots_per_page == 1) {
    # Heatmap mode: one plot per page, function draws directly
    for (i in seq_len(n_samples)) {
      plot_fn(
        catalog = catalog[, i, drop = FALSE],
        plot_title = colnames(catalog)[i],
        ...
      )
    }
  } else {
    # Bar plot mode: collect ggplot objects, arrange per page
    for (i in seq(1, n_samples, by = plots_per_page)) {
      end_idx <- min(i + plots_per_page - 1, n_samples)
      page_plots <- list()

      for (j in i:end_idx) {
        p <- plot_fn(
          catalog = catalog[, j, drop = FALSE],
          plot_title = colnames(catalog)[j],
          ...
        )
        page_plots[[length(page_plots) + 1]] <- p
      }

      # Pad with blank plots to fill the page
      while (length(page_plots) < plots_per_page) {
        page_plots[[length(page_plots) + 1]] <- ggplot2::ggplot() +
          ggplot2::theme_void()
      }

      gridExtra::grid.arrange(
        grobs = page_plots,
        ncol = 1,
        nrow = plots_per_page
      )
    }
  }

  invisible(NULL)
}


#' Resolve axis visibility parameters with deprecation handling
#'
#' Maps the deprecated `xlabels`/`ylabels` parameters to the new
#' `show_axis_text_x`, `show_axis_text_y`, `show_axis_title_x`,
#' `show_axis_title_y` parameters. Errors if both old and new are specified.
#'
#' @param show_axis_text_x Logical. Show x-axis tick labels.
#' @param show_axis_text_y Logical. Show y-axis tick labels.
#' @param show_axis_title_x Logical. Show x-axis title.
#' @param show_axis_title_y Logical. Show y-axis title.
#' @param xlabels Deprecated. Use `show_axis_text_x`.
#' @param ylabels Deprecated. Use `show_axis_text_y` and `show_axis_title_y`.
#'
#' @return A named list with resolved values for `show_axis_text_x`,
#'   `show_axis_text_y`, `show_axis_title_x`, `show_axis_title_y`.
#' @keywords internal
resolve_axis_params <- function(
    show_axis_text_x = TRUE,
    show_axis_text_y = TRUE,
    show_axis_title_x = TRUE,
    show_axis_title_y = TRUE,
    xlabels = NULL,
    ylabels = NULL
) {
  if (!is.null(xlabels)) {
    if (!identical(show_axis_text_x, TRUE)) {
      stop(
        "Cannot specify both 'xlabels' and 'show_axis_text_x'. ",
        "Use 'show_axis_text_x' (xlabels is deprecated).",
        call. = FALSE
      )
    }
    warning("'xlabels' is deprecated. Use 'show_axis_text_x' instead.",
            call. = FALSE)
    show_axis_text_x <- xlabels
  }

  if (!is.null(ylabels)) {
    if (!identical(show_axis_text_y, TRUE) ||
        !identical(show_axis_title_y, TRUE)) {
      stop(
        "Cannot specify both 'ylabels' and ",
        "'show_axis_text_y'/'show_axis_title_y'. ",
        "Use the new parameters (ylabels is deprecated).",
        call. = FALSE
      )
    }
    warning(
      "'ylabels' is deprecated. ",
      "Use 'show_axis_text_y' and 'show_axis_title_y' instead.",
      call. = FALSE
    )
    show_axis_text_y <- ylabels
    show_axis_title_y <- ylabels
  }

  list(
    show_axis_text_x = show_axis_text_x,
    show_axis_text_y = show_axis_text_y,
    show_axis_title_x = show_axis_title_x,
    show_axis_title_y = show_axis_title_y
  )
}
