#' Convert SBS96-channel mutations-type identifiers like this \code{"A[C>A]T" -> "ACTA"}
#'
#' @param c1 A vector of character strings with the mutation indicated by
#' e.g. \code{[C>A]} in the middle.
#'
#' @keywords internal
unstaple_SBS96_rownames <- function(c1) {
  retval <-
    paste0(
      substr(c1, 1, 1),
      substr(c1, 3, 3),
      substr(c1, 7, 7),
      substr(c1, 5, 5)
    )
  return(retval)
}
