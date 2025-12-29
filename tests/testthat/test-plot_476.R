test_that("plot_476 returns a ggplot object", {
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

  p <- plot_476(
    catalog = catalog,
    text_size = 3,
    plot_title = colnames(sig_data)[1]
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_476 handles different label options", {
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
  p_no_labels <- plot_476(
    catalog = catalog,
    plot_title = "No labels",
    num_labels = 0
  )
  expect_s3_class(p_no_labels, "ggplot")

  # Test with simplified labels
  p_simplified <- plot_476(
    catalog = catalog,
    plot_title = "Simplified labels",
    num_labels = 5,
    simplify_labels = TRUE
  )
  expect_s3_class(p_simplified, "ggplot")

  # Test with full labels
  p_full <- plot_476(
    catalog = catalog,
    plot_title = "Full labels",
    num_labels = 3,
    simplify_labels = FALSE
  )
  expect_s3_class(p_full, "ggplot")
})

test_that("plot_476 handles vertical line annotations", {
  fixture_path <- testthat::test_path("fixtures", "type476_liu_et_al_sigs.tsv")
  sig_data <- read.table(
    fixture_path,
    header = TRUE,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )

  catalog <- as.numeric(sig_data[, 1])

  p <- plot_476(
    catalog = catalog,
    plot_title = "With vlines",
    vline_labels = c("A[Del(C):R1]A", "G[Del(C):R1]A")
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_476 handles base_size", {
  fixture_path <- testthat::test_path("fixtures", "type476_liu_et_al_sigs.tsv")
  sig_data <- read.table(
    fixture_path,
    header = TRUE,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )

  catalog <- as.numeric(sig_data[, 1])

  p <- plot_476(
    catalog = catalog,
    plot_title = "With base_size",
    base_size = 20
  )

  expect_s3_class(p, "ggplot")
})


test_that("plot_476_pdf creates PDF file", {
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

  plot_476_pdf(
    Koh476_catalog = catalog_subset,
    filename = temp_pdf,
    num_labels = 2
  )

  expect_true(file.exists(temp_pdf))
  expect_gt(file.size(temp_pdf), 0)

  # Cleanup
  unlink(temp_pdf)
})
