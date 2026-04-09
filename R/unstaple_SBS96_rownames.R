#' Convert stapled SBS96 row names to compact format
#'
#' Converts names like `"A[C>A]T"` to `"ACTA"` (compact
#' `<5' base><ref><3' base><alt>` format).
#'
#' @param stapled_names Character vector of stapled mutation type identifiers
#'   (e.g. `"A[C>A]T"`).
#'
#' @return Character vector of compact 4-letter identifiers.
#' @keywords internal
unstaple_SBS96_rownames <- function(stapled_names) {
  paste0(
    substr(stapled_names, 1, 1),
    substr(stapled_names, 3, 3),
    substr(stapled_names, 7, 7),
    substr(stapled_names, 5, 5)
  )
}
