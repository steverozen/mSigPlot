test_that("resolve_axis_params returns defaults when no deprecated params", {
  result <- resolve_axis_params()
  expect_true(result$show_axis_text_x)
  expect_true(result$show_axis_text_y)
  expect_true(result$show_axis_title_x)
  expect_true(result$show_axis_title_y)
})

test_that("resolve_axis_params maps xlabels to show_axis_text_x", {
  expect_warning(result <- resolve_axis_params(xlabels = FALSE),
                 "xlabels.*deprecated")
  expect_false(result$show_axis_text_x)
  expect_true(result$show_axis_text_y)
})

test_that("resolve_axis_params maps ylabels to text_y and title_y", {
  expect_warning(result <- resolve_axis_params(ylabels = FALSE),
                 "ylabels.*deprecated")
  expect_false(result$show_axis_text_y)
  expect_false(result$show_axis_title_y)
  expect_true(result$show_axis_text_x)
  expect_true(result$show_axis_title_x)
})

test_that("resolve_axis_params errors on xlabels + show_axis_text_x conflict", {
  expect_error(
    resolve_axis_params(show_axis_text_x = FALSE, xlabels = FALSE),
    "Cannot specify both"
  )
})

test_that("resolve_axis_params errors on ylabels + show_axis_text_y conflict", {
  expect_error(
    resolve_axis_params(show_axis_text_y = FALSE, ylabels = FALSE),
    "Cannot specify both"
  )
})

test_that("plot_SBS96 accepts show_axis_text_x = FALSE", {
  set.seed(1)
  sig <- runif(96)
  names(sig) <- catalog_row_order()$SBS96
  p <- plot_SBS96(sig, show_axis_text_x = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_SBS96 warns on deprecated xlabels", {
  set.seed(1)
  sig <- runif(96)
  names(sig) <- catalog_row_order()$SBS96
  expect_warning(
    plot_SBS96(sig, xlabels = FALSE),
    "xlabels.*deprecated"
  )
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
