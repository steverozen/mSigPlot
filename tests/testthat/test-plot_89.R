test_that("plot_89 returns a ggplot object", {
  # Create a simple test catalog with 89 values
  test_catalog <- rep(0.01, 89)

  p <- plot_89(
    ID89.catalog = test_catalog,
    text_size = 3,
    plot_title = "Test ID89"
  )

  expect_s3_class(p, "ggplot")

  p <- plot_89(
    ID89.catalog = test_catalog,
    text_size = 3,
    plot_title = "Test ID89",
    setyaxis = .03
  )
})
