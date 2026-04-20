test_that("plot_SBS96 grid = TRUE adds horizontal gridlines", {
  set.seed(1)
  sig <- runif(96)
  names(sig) <- catalog_row_order()$SBS96

  has_hline <- function(p) {
    any(vapply(p$layers,
               function(l) inherits(l$geom, "GeomHline"),
               logical(1)))
  }

  expect_false(has_hline(plot_SBS96(sig)))
  expect_true(has_hline(plot_SBS96(sig, grid = TRUE)))
})

test_that("plot_SBS96 title_outside_plot controls title placement", {
  set.seed(1)
  sig <- runif(96)
  names(sig) <- catalog_row_order()$SBS96

  p_inside  <- plot_SBS96(sig, plot_title = "TestTitle")
  p_outside <- plot_SBS96(sig, plot_title = "TestTitle",
                          title_outside_plot = TRUE)

  # title_outside_plot = TRUE uses ggtitle(): sets labels$title.
  # plot_SBS96 appends a newline to the title in this branch to keep it
  # from crowding the class-label strip.
  expect_equal(p_outside$labels$title, "TestTitle\n")
  # title_outside_plot = FALSE uses annotate("text", ...): labels$title unset
  expect_null(p_inside$labels$title)
})
