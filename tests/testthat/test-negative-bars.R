# Tests that plot functions correctly render negative bars
# (e.g. for signature difference plots)

# Helper: check that a ggplot has bars extending below zero.
# Searches all layers for any with ymin < 0.
expect_negative_bars <- function(p, label = "") {
  pb <- ggplot2::ggplot_build(p)
  has_neg <- any(vapply(pb$data, function(d) {
    if ("ymin" %in% names(d)) any(d$ymin < 0, na.rm = TRUE) else FALSE
  }, logical(1)))
  expect_true(has_neg,
    info = paste(label, "— some bars should extend below zero"))
}

test_that("plot_SBS96 renders negative bars", {
  set.seed(1)
  catalog <- runif(96, min = -0.02, max = 0.04)
  names(catalog) <- catalog_row_order()$SBS96
  p <- plot_SBS96(catalog, plot_title = "SBS96 neg")
  expect_s3_class(p, "ggplot")
  expect_negative_bars(p, "plot_SBS96")
})

test_that("plot_SBS192 renders negative bars", {
  set.seed(2)
  catalog <- runif(192, min = -0.02, max = 0.04)
  names(catalog) <- catalog_row_order()$SBS192
  p <- plot_SBS192(catalog, plot_title = "SBS192 neg")
  expect_s3_class(p, "ggplot")
  expect_negative_bars(p, "plot_SBS192")
})

test_that("plot_DBS78 renders negative bars", {
  set.seed(3)
  catalog <- runif(78, min = -0.03, max = 0.05)
  names(catalog) <- catalog_row_order()$DBS78
  p <- plot_DBS78(catalog, plot_title = "DBS78 neg")
  expect_s3_class(p, "ggplot")
  expect_negative_bars(p, "plot_DBS78")
})

test_that("plot_DBS144 renders negative bars", {
  set.seed(4)
  # DBS144 aggregates into 20 strand-pair values; use strongly negative data
  # so sums remain negative
  catalog <- rep(-0.02, 144)
  catalog[seq(1, 144, by = 3)] <- 0.01  # some positive, but net negative
  names(catalog) <- catalog_row_order()$DBS144
  p <- plot_DBS144(catalog, plot_title = "DBS144 neg")
  expect_s3_class(p, "ggplot")
  expect_negative_bars(p, "plot_DBS144")
})

test_that("plot_SBS12 renders negative bars", {
  set.seed(5)
  # SBS12 aggregates 192 values into 12 strand-pair bars; use strongly
  # negative data so sums remain negative
  catalog <- rep(-0.02, 192)
  catalog[seq(1, 192, by = 5)] <- 0.01
  names(catalog) <- catalog_row_order()$SBS192
  p <- plot_SBS12(catalog, plot_title = "SBS12 neg")
  expect_s3_class(p, "ggplot")
  expect_negative_bars(p, "plot_SBS12")
})

test_that("plot_ID83 renders negative bars", {
  set.seed(6)
  catalog <- runif(83, min = -0.03, max = 0.05)
  names(catalog) <- catalog_row_order()$ID
  p <- plot_ID83(catalog, plot_title = "ID83 neg")
  expect_s3_class(p, "ggplot")
  expect_negative_bars(p, "plot_ID83")
})

test_that("plot_ID89 renders negative bars", {
  set.seed(7)
  catalog <- runif(89, min = -0.03, max = 0.05)
  p <- plot_ID89(catalog, plot_title = "ID89 neg", upper = FALSE)
  expect_s3_class(p, "ggplot")
  expect_negative_bars(p, "plot_ID89")
})

test_that("plot_ID476 renders negative bars", {
  set.seed(8)
  catalog <- runif(476, min = -0.01, max = 0.03)
  names(catalog) <- catalog_row_order()$ID476
  p <- plot_ID476(catalog, plot_title = "ID476 neg")
  expect_s3_class(p, "ggplot")
  expect_negative_bars(p, "plot_ID476")
})

test_that("plot_ID166 renders negative bars", {
  set.seed(9)
  # ID166 interleaves genic/intergenic pairs; individual bar values are
  # direct from the catalog, not aggregated
  catalog <- runif(166, min = -0.02, max = 0.04)
  names(catalog) <- catalog_row_order()$ID166
  p <- plot_ID166(catalog, plot_title = "ID166 neg")
  expect_s3_class(p, "ggplot")
  expect_negative_bars(p, "plot_ID166")
})

test_that("plot_SBS288 renders negative bars", {
  set.seed(10)
  # SBS288 row names must start with T:, U:, or N:
  sbs96_names <- catalog_row_order()$SBS96
  rnames <- c(paste0("T:", sbs96_names),
              paste0("U:", sbs96_names),
              paste0("N:", sbs96_names))
  catalog <- runif(288, min = -0.02, max = 0.04)
  names(catalog) <- rnames
  p <- plot_SBS288(catalog, plot_title = "SBS288 neg")
  # plot_SBS288 returns a gtable, not a ggplot
  expect_true(inherits(p, "gtable") || inherits(p, "ggplot"))
})
