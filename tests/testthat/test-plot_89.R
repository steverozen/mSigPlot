test_that("plot_89 returns a ggplot object", {
  # Create a simple test catalog with 89 values
  test_catalog <- rep(0.01, 89)

  p <- plot_89(
    catalog = test_catalog,
    text_cex = 3,
    plot_title = "Test 89 type plot"
  )

  expect_s3_class(p, "ggplot")

  p <- plot_89(
    catalog = test_catalog,
    text_cex = 3,
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
    top_bar_text_cex = 2.5,
    text_cex = 3.5,
    title_text_cex = 0.9
  )

  expect_true(file.exists(temp_pdf))
  expect_gt(file.size(temp_pdf), 0)

  # Cleanup
  unlink(temp_pdf)
})

test_that("plot_89 count labels render with count catalog", {
  fixture_path <- testthat::test_path("fixtures", "type89_liu_et_al_sigs.tsv")
  sig_data <- read.table(
    fixture_path,
    header = TRUE,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )

  # Multiply by 100 to create a count-type catalog (sum >> 1.1)
  catalog_count <- sig_data[, 1] * 100

  p <- plot_89(
    catalog = catalog_count,
    plot_title = "Counts test"
  )

  expect_s3_class(p, "ggplot")

  # Verify count labels layer is present
  pb <- ggplot2::ggplot_build(p)
  has_count_layer <- any(vapply(pb$data, function(d) {
    "label" %in% names(d) && any(grepl("^[0-9]+$", as.character(d$label)))
  }, logical(1)))
  expect_true(has_count_layer)
})

test_that("plot_89 show_counts = FALSE suppresses count labels", {
  fixture_path <- testthat::test_path("fixtures", "type89_liu_et_al_sigs.tsv")
  sig_data <- read.table(
    fixture_path,
    header = TRUE,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )

  catalog_count <- sig_data[, 1] * 100

  p <- plot_89(
    catalog = catalog_count,
    plot_title = "No counts",
    show_counts = FALSE
  )

  expect_s3_class(p, "ggplot")

  # Count labels should NOT be present
  pb <- ggplot2::ggplot_build(p)
  has_count_layer <- any(vapply(pb$data, function(d) {
    "label" %in% names(d) && any(grepl("^[0-9]+$", as.character(d$label)))
  }, logical(1)))
  expect_false(has_count_layer)
})
