test_that("plot_ID476 returns a ggplot object", {
  # Load fixture data
  fixture_path <- testthat::test_path("fixtures", "type476_liu_et_al_sigs.tsv")
  sig_data <- read.table(
    fixture_path,
    header = TRUE,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )

  # Test with first signature column
  catalog <- as.numeric(sig_data[, 1])

  p <- plot_ID476(
    catalog = catalog,
    class_label_cex = 3,
    plot_title = colnames(sig_data)[1]
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_ID476 handles different label options", {
  fixture_path <- testthat::test_path("fixtures", "type476_liu_et_al_sigs.tsv")
  sig_data <- read.table(
    fixture_path,
    header = TRUE,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )

  catalog <- as.numeric(sig_data[, 1])

  # Test with no labels
  p_no_labels <- plot_ID476(
    catalog = catalog,
    plot_title = "No labels",
    num_peak_labels = 0
  )
  expect_s3_class(p_no_labels, "ggplot")

  # Test with simplified labels
  p_simplified <- plot_ID476(
    catalog = catalog,
    plot_title = "Simplified labels",
    num_peak_labels = 5,
    simplify_labels = TRUE
  )
  expect_s3_class(p_simplified, "ggplot")

  # Test with full labels
  p_full <- plot_ID476(
    catalog = catalog,
    plot_title = "Full labels",
    num_peak_labels = 3,
    simplify_labels = FALSE
  )
  expect_s3_class(p_full, "ggplot")
})

test_that("plot_ID476 handles vertical line annotations", {
  fixture_path <- testthat::test_path("fixtures", "type476_liu_et_al_sigs.tsv")
  sig_data <- read.table(
    fixture_path,
    header = TRUE,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )

  catalog <- as.numeric(sig_data[, 1])

  p <- plot_ID476(
    catalog = catalog,
    plot_title = "With vlines",
    vline_labels = c("A[Del(C):R1]A", "G[Del(C):R1]A")
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_ID476 handles base_size", {
  fixture_path <- testthat::test_path("fixtures", "type476_liu_et_al_sigs.tsv")
  sig_data <- read.table(
    fixture_path,
    header = TRUE,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )

  catalog <- as.numeric(sig_data[, 1])

  p <- plot_ID476(
    catalog = catalog,
    plot_title = "With base_size",
    base_size = 30
  )

  expect_s3_class(p, "ggplot")
})


test_that("plot_ID476_pdf creates PDF file", {
  fixture_path <- testthat::test_path("fixtures", "type476_liu_et_al_sigs.tsv")
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

  plot_ID476_pdf(
    catalog = catalog_subset,
    filename = temp_pdf,
    num_peak_labels = 2
  )

  expect_true(file.exists(temp_pdf))
  expect_gt(file.size(temp_pdf), 0)

  plot_ID476_pdf(
    catalog = catalog_subset,
    filename = temp_pdf,
    num_peak_labels = 2,
    plot_title_cex = 0.8
  )

  expect_true(file.exists(temp_pdf))
  expect_gt(file.size(temp_pdf), 0)

  # Cleanup
  unlink(temp_pdf)
})

test_that("plot_ID476 count labels render with count catalog", {
  fixture_path <- testthat::test_path("fixtures", "type476_liu_et_al_sigs.tsv")
  sig_data <- read.table(
    fixture_path,
    header = TRUE,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )

  # Multiply by 100 to create a count-type catalog (sum >> 1.1)
  catalog_count <- as.numeric(sig_data[, 1]) * 100

  p <- plot_ID476(
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

test_that("plot_ID476 show_counts = FALSE suppresses count labels", {
  fixture_path <- testthat::test_path("fixtures", "type476_liu_et_al_sigs.tsv")
  sig_data <- read.table(
    fixture_path,
    header = TRUE,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )

  catalog_count <- as.numeric(sig_data[, 1]) * 100

  p <- plot_ID476(
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

test_that("plot_ID476_right count labels render with count catalog", {
  fixture_path <- testthat::test_path("fixtures", "type476_liu_et_al_sigs.tsv")
  sig_data <- read.table(
    fixture_path,
    header = TRUE,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )

  catalog_count <- as.numeric(sig_data[, 1]) * 100

  p <- plot_ID476_right(
    catalog = catalog_count,
    plot_title = "Right counts test"
  )

  expect_s3_class(p, "ggplot")

  # Verify count labels layer is present
  pb <- ggplot2::ggplot_build(p)
  has_count_layer <- any(vapply(pb$data, function(d) {
    "label" %in% names(d) && any(grepl("^[0-9]+$", as.character(d$label)))
  }, logical(1)))
  expect_true(has_count_layer)
})
