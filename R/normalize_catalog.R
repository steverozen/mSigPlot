#' Normalize catalog input to a 1-column data.frame
#'
#' Internal helper that coerces a catalog from any supported input type
#' (numeric vector, matrix, data.table, tibble, data.frame) to a 1-column
#' data.frame with validated row names.
#'
#' @param catalog Input catalog in any supported format.
#' @param expected_nrow Integer. Expected number of rows/elements.
#' @param canonical_order Character vector of canonical row names, or NULL
#'   to skip row name validation.
#' @param type_label Character label for error/warning messages (e.g. "SBS96").
#'
#' @return A 1-column data.frame, or NULL if row names do not match canonical
#'   order.
#'
#' @keywords internal
normalize_catalog <- function(
  catalog,
  expected_nrow,
  canonical_order = NULL,
  type_label = ""
) {
  original_attrs <- attributes(catalog)
  y_axis_type_attr <- original_attrs$y_axis_type_attr

  if (is.numeric(catalog) && !is.matrix(catalog)) {
    # Named or unnamed numeric vector
    nms <- names(catalog)
    catalog <- data.frame(sample = unname(catalog))
    if (!is.null(nms)) rownames(catalog) <- nms
  } else if (is.matrix(catalog)) {
    catalog <- as.data.frame(catalog[, 1, drop = FALSE])
  } else if (inherits(catalog, "data.table") || inherits(catalog, "tbl_df")) {
    catalog <- as.data.frame(catalog)[, 1, drop = FALSE]
  } else if (is.data.frame(catalog)) {
    catalog <- catalog[, 1, drop = FALSE]
  } else {
    stop(
      "catalog must be a numeric vector, matrix, data.frame, tibble, or data.table"
    )
  }

  # Preserve y_axis_type_attr attribute from original input
  if (!is.null(y_axis_type_attr)) {
    attr(catalog, "y_axis_type_attr") <- y_axis_type_attr
  }

  if (nrow(catalog) != expected_nrow) {
    warning(
      "Expected ",
      expected_nrow,
      " rows but got ",
      nrow(catalog),
      " rows; returning NULL"
    )
    return(NULL)
  }

  # Validate and reorder row names if canonical_order is provided
  if (!is.null(canonical_order)) {
    rn <- rownames(catalog)
    if (is.null(rn) || all(rn == as.character(seq_len(expected_nrow)))) {
      # Trivial row names: assume data is in canonical order
      rownames(catalog) <- canonical_order
    } else {
      if (!setequal(rn, canonical_order)) {
        warning(
          "Row names of catalog do not match canonical ",
          type_label,
          " row names; returning NULL"
        )
        return(NULL)
      }
      catalog <- catalog[canonical_order, , drop = FALSE]
    }
  }

  catalog
}
