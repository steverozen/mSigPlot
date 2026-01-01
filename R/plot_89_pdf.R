library(magrittr)
library(indelsig.tools.lib)
library(gridExtra)
library(data.table)
library(ICAMS)
library(ggplot2)
library(dplyr)


#' Export 89-channel indel profiles to PDF
#'
#' Creates a multi-page PDF file containing 89-channel indel profile plots
#' for multiple samples. Plots are arranged with 5 samples per page.
#'
#' @param ID89_catalog A matrix or data frame with 89 rows (indel types) and
#'   one column per sample. Column names are used as plot titles.
#' @param filename Character. Path to the output PDF file.
#'
#' @return NULL. Called for side effect of creating a PDF file.
#'
#' @export
#'
#' @import Cairo
#' @importFrom grDevices dev.off cairo_pdf
plot_89_pdf <- function(ID89_catalog, filename) {
  plot_list <- lapply(1:ncol(ID89_catalog), function(i) {
    plot_89(
      ID89.catalog = ID89_catalog[, i],
      text_size = 3,
      plot_title = colnames(ID89_catalog)[i]
    )
  })
  plots_per_page <- 5

  # Total number of pages
  total_pages <- ceiling(length(plot_list) / plots_per_page)
  library(gridExtra)
  # Open a PDF device
  cairo_pdf(filename, width = 8.2677, height = 14.61613)

  # Loop through pages and save each group of plots to the same PDF
  for (page in 1:total_pages) {
    # Get the plots for the current page
    start_index <- (page - 1) * plots_per_page + 1
    end_index <- min(page * plots_per_page, length(plot_list))
    plots_on_page <- plot_list[start_index:end_index]

    # Arrange the plots in a grid (2 rows x 4 columns)
    do.call(gridExtra::grid.arrange, c(plots_on_page, nrow = 5, ncol = 1))
  }

  # Close the PDF device
  dev.off()
}

xxKoh89_indeltype <- structure(
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
