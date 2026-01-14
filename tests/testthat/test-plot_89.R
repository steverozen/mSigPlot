test_that("plot_89 returns a ggplot object", {
  # Create a simple test catalog with 89 values
  test_catalog <- rep(0.01, 89)

  p <- plot_89(
    catalog = test_catalog,
    text_size = 3,
    plot_title = "Test 89 type plot"
  )

  expect_s3_class(p, "ggplot")

  p <- plot_89(
    catalog = test_catalog,
    text_size = 3,
    plot_title = "Test 89 type plot",
    setyaxis = .03
  )
})
test_that("plot_89_pdf creates PDF file", {
  fixture_path <- testthat::test_path("fixtures", "type89_liu_et_al_sigs.tsv")
  sig_data <- read.table(
    fixture_path,
    header = TRUE,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )

  # Use first 3 signatures for faster test
  catalog_subset <- sig_data[, 1:3, drop = FALSE]

  # Create temp file for output

  temp_pdf <- tempfile(fileext = ".pdf")

  plot_89_pdf(
    catalog_subset,
    filename = temp_pdf
  )

  expect_true(file.exists(temp_pdf))
  expect_gt(file.size(temp_pdf), 0)

  plot_89_pdf(
    catalog_subset,
    filename = temp_pdf,
    top_bar_text_size = 2.5,
    text_size = 3.5,
    title_text_size = 0.9
  )

  expect_true(file.exists(temp_pdf))
  expect_gt(file.size(temp_pdf), 0)

  # Cleanup
  unlink(temp_pdf)
})
