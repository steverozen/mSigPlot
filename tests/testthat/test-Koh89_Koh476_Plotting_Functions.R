test_that("PlotKoh89Catalog returns a ggplot object", {
  # Create a simple test catalog with 89 values
  test_catalog <- rep(0.01, 89)

  p <- PlotKoh89Catalog(
    ID89.catalog = test_catalog,
    text_size = 3,
    plot_title = "Test ID89"
  )

  expect_s3_class(p, "ggplot")
})

test_that("Koh89_indeltype has correct dimensions", {
  expect_equal(nrow(Koh89_indeltype), 89)
  expect_true("IndelType" %in% names(Koh89_indeltype))
  expect_true("Indel" %in% names(Koh89_indeltype))
  expect_true("Figlabel" %in% names(Koh89_indeltype))
})
