test_that("plot_SBS96 accepts show_axis_text_x = FALSE", {
  set.seed(1)
  sig <- runif(96)
  names(sig) <- catalog_row_order()$SBS96
  p <- plot_SBS96(sig, show_axis_text_x = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_DBS78 accepts show_axis_text_y = FALSE", {
  set.seed(1)
  sig <- runif(78)
  names(sig) <- catalog_row_order()$DBS78
  p <- plot_DBS78(sig, show_axis_text_y = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_ID476 accepts show_axis_text_x = FALSE", {
  set.seed(1)
  sig <- runif(476)
  names(sig) <- catalog_row_order()$ID476
  p <- plot_ID476(sig, show_axis_text_x = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_DBS144 accepts show_axis_text_x = FALSE", {
  set.seed(1)
  sig <- runif(144)
  names(sig) <- catalog_row_order()$DBS144
  p <- plot_DBS144(sig, show_axis_text_x = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_ID89 accepts show_axis_text_x = FALSE", {
  fixture_path <- testthat::test_path("fixtures", "type89_liu_et_al_sigs.tsv")
  sigs <- read.table(fixture_path, header = TRUE, sep = "\t",
                     row.names = 1, check.names = FALSE)
  p <- plot_ID89(sigs[, 1, drop = FALSE], show_axis_text_x = FALSE)
  expect_s3_class(p, "ggplot")
})
