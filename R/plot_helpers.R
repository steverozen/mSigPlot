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


#' Detect y-axis type from data and attributes
#'
#' Determines the y-axis type for a catalog. Checks the `y_axis_type_attr`
#' attribute first; if absent, infers from data values and optional ylim.
#'
#' @param values Numeric vector of catalog values.
#' @param y_axis_type_attr The `y_axis_type_attr` attribute from the catalog, or NULL.
#' @param ylim Optional y-axis limits vector.
#' @return Character string: one of `"counts"`, `"proportion"`,
#'   `"muts_per_million"`, or `"density_proportion"`.
#' @keywords internal
detect_y_axis_type <- function(values, y_axis_type_attr = NULL, ylim = NULL) {
  if (!is.null(y_axis_type_attr)) return(y_axis_type_attr)
  if (!is.null(ylim) && max(ylim) > 1.5) return("counts")
  if (sum(abs(values), na.rm = TRUE) >= 1.1) "counts" else "proportion"
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


#' Add a plot title, inside the panel (annotate) or above it (ggtitle)
#'
#' If `title_outside_plot` is FALSE (the default), places `plot_title` inside
#' the plot area via `annotate("text", ...)` at `y = ymax * y_frac` — the
#' `plot_ID83` style. If TRUE, places it above the plot via
#' `ggtitle() + theme(plot.title = ...)`. Title size is always
#' `plot_title_cex * base_size` (points), normalized to mm for `annotate`.
#'
#' @param p A ggplot object.
#' @param plot_title Character title (NULL or empty = no title added).
#' @param title_outside_plot Logical. FALSE = inside, TRUE = above.
#' @param plot_title_cex Numeric size multiplier.
#' @param base_size Numeric base font size in points.
#' @param ymax Numeric. Top of the plotting region used to position the
#'   inside title. Ignored when `title_outside_plot = TRUE`.
#' @param x Numeric x-coordinate for the inside title.
#' @param y_frac Numeric. Inside title sits at `ymax * y_frac`.
#' @param hjust Horizontal justification of the inside title.
#' @return The ggplot object with the title added.
#' @keywords internal
add_plot_title <- function(p, plot_title, title_outside_plot,
                           plot_title_cex, base_size, ymax,
                           x = 1, y_frac = 7.4 / 8, hjust = 0) {
  if (is.null(plot_title) || !nzchar(plot_title)) return(p)
  if (title_outside_plot) {
    p +
      ggplot2::ggtitle(plot_title) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          size = plot_title_cex * base_size))
  } else {
    p + ggplot2::annotate(
      "text",
      x = x, y = ymax * y_frac,
      label = plot_title, hjust = hjust,
      fontface = "bold",
      size = plot_title_cex * base_mm(base_size))
  }
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


#' Prepare indel data for plotting
#'
#' Merges a catalog vector with an indel type table, producing a long-format
#' data frame ready for ggplot. Used by `plot_ID476`, `plot_ID476_right`,
#' and `plot_ID89`.
#'
#' @param catalog Named numeric vector of indel channel values.
#' @param type_table Data frame with at least an `IndelType` column and
#'   classification columns (e.g. `Indel`, `Figlabel`).
#' @return Data frame with columns from `type_table` plus `Sample` and `freq`.
#' @keywords internal
prepare_indel_data <- function(catalog, type_table) {
  melt_df <- data.frame(
    IndelType = type_table$IndelType,
    variable  = "Sample",
    value     = catalog,
    stringsAsFactors = FALSE
  )
  merged <- merge(type_table, melt_df, by = "IndelType", all.x = TRUE)
  merged[is.na(merged)] <- 0
  # Rename last two columns to Sample and freq
  nc <- ncol(merged)
  names(merged)[(nc - 1):nc] <- c("Sample", "freq")
  merged$Sample <- as.character(merged$Sample)
  merged
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
    # One-per-page mode: heatmaps draw directly, patchwork/ggplot need print()
    for (i in seq_len(n_samples)) {
      p <- plot_fn(
        catalog = catalog[, i, drop = FALSE],
        plot_title = colnames(catalog)[i],
        ...
      )
      if (inherits(p, "patchwork") || inherits(p, "ggplot")) {
        print(p)
      }
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
        # Patchwork objects (e.g. SBS288) can't go into grid.arrange;
        # print each on its own page
        if (inherits(p, "patchwork")) {
          print(p)
        } else {
          page_plots[[length(page_plots) + 1]] <- p
        }
      }

      # Only arrange if we collected any ggplot objects
      if (length(page_plots) > 0) {
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
  }

  invisible(NULL)
}