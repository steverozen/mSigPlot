#' Derive ID89 figure-axis labels from canonical IndelType strings
#'
#' Rewrites the canonical ID89 channel names from `catalog_row_order()$ID89`
#' into the compact tick-label style used on the x-axis of `plot_ID89()`.
#'
#' Single-base channels (matching `Del(C)`, `Del(T)`, `Ins(C)`, `Ins(T)`):
#' the `Del(..)`/`Ins(..)` wrapper and the `:R` prefix are dropped, and any
#' open-ended repeat range `(N,)` is rewritten as `(N+)`.
#'
#' Multi-base channels (longer Del/Ins/MH rows): leading `Del(`/`Ins(` is
#' rewritten to `L(`, a leading open-ended length `(N,)` is space-padded to
#' `(N, )`, a trailing `R(N,)` becomes `R(N+)`, a trailing `M(N,)` is
#' space-padded to `M(N, )`, and all colons are removed.
#'
#' The `"Complex"` channel is returned unchanged.
#'
#' @param indel_types Character vector of ID89 IndelType strings, typically
#'   `catalog_row_order()$ID89`.
#'
#' @return A character vector of the same length as `indel_types`, giving the
#'   figure-label form of each channel name.
#'
#' @examples
#' id89_figlabels(c(
#'   "[Del(C):R1]A",
#'   "Del(C):R(6,9)",
#'   "Del(2,4):R1",
#'   "Del(2,):U(1,2):R(5,)",
#'   "Del(6,):M(4,)",
#'   "Complex"
#' ))
#'
#' @export
id89_figlabels <- function(indel_types) {
  x <- indel_types
  is_single <- grepl("(Del|Ins)\\([CT]\\)", x)
  is_multi <- !is_single & x != "Complex"

  x[is_single] <- gsub("(Del|Ins)\\(([CT])\\)", "\\2", x[is_single])
  x[is_single] <- gsub(":R", "", x[is_single])
  x[is_single] <- gsub("\\(([0-9]+),\\)", "(\\1+)", x[is_single])
  x[x == "C(6,9)"] <- "C(6+)"

  x[is_multi] <- sub("^(Del|Ins)\\(", "L(", x[is_multi])
  x[is_multi] <- sub("^L\\(([0-9]+),\\)", "L(\\1+)", x[is_multi])
  x[is_multi] <- sub("R\\(([0-9]+),\\)$", "R(\\1+)", x[is_multi])
  x[is_multi] <- sub("M\\(([0-9]+),\\)$", "M(\\1+)", x[is_multi])
  x[is_multi] <- gsub(":", "", x[is_multi])
  x[x == "L(3+)U(3,)R(3+)"] <- "L(3+)U(3+)R(3+)"
  x[x == "L(3+)U(3,)R2"] <- "L(3+)U(3+)R2"

  x
}
