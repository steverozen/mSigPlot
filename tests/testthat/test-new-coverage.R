# Tests for previously untested functionality

# --- #9: plot_SBS12 strand bias with abundance parameter ---

test_that("plot_SBS12 strand bias test runs with abundance", {
  set.seed(1)
  # Create a 192-channel count catalog
  catalog <- round(runif(192, 10, 100))
  names(catalog) <- catalog_row_order()$SBS192

  # Create a 64-element abundance vector (trinucleotide frequencies)
  bases <- c("A", "C", "G", "T")
  trimers <- expand.grid(bases, bases, bases)
  abundance <- runif(64, 1000, 5000)
  names(abundance) <- apply(trimers, 1, paste0, collapse = "")

  p <- plot_SBS12(catalog, plot_title = "Strand bias test", abundance = abundance)
  expect_s3_class(p, "ggplot")

  # Verify the strand.bias.statistics attribute is attached
  bias_stats <- attr(p, "strand.bias.statistics")
  expect_true(!is.null(bias_stats))
  expect_true("q.values" %in% names(bias_stats))
  expect_equal(nrow(bias_stats), 6)
})

# --- #10: Heatmap PDF tests ---

test_that("plot_DBS136_pdf creates PDF file", {
  set.seed(1)
  catalog <- data.frame(
    sample1 = runif(136),
    sample2 = runif(136),
    row.names = catalog_row_order()$DBS136
  )
  temp_pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_pdf), add = TRUE)

  plot_DBS136_pdf(catalog, filename = temp_pdf)

  expect_true(file.exists(temp_pdf))
  expect_gt(file.size(temp_pdf), 0)
})

test_that("plot_SBS1536_pdf creates PDF file", {
  set.seed(1)
  catalog <- data.frame(
    sample1 = runif(1536),
    sample2 = runif(1536),
    row.names = catalog_row_order()$SBS1536
  )
  temp_pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_pdf), add = TRUE)

  plot_SBS1536_pdf(catalog, filename = temp_pdf)

  expect_true(file.exists(temp_pdf))
  expect_gt(file.size(temp_pdf), 0)
})

# --- #11: num_peak_labels > 0 tests ---

test_that("plot_SBS96 with num_peak_labels renders peak labels", {
  set.seed(1)
  catalog <- round(runif(96, 10, 500))
  names(catalog) <- catalog_row_order()$SBS96

  p <- plot_SBS96(catalog, plot_title = "Peak labels", num_peak_labels = 3)
  expect_s3_class(p, "ggplot")
})

test_that("plot_DBS78 with num_peak_labels renders peak labels", {
  set.seed(1)
  catalog <- round(runif(78, 10, 500))
  names(catalog) <- catalog_row_order()$DBS78

  p <- plot_DBS78(catalog, plot_title = "Peak labels", num_peak_labels = 3)
  expect_s3_class(p, "ggplot")
})

test_that("plot_ID83 with num_peak_labels renders peak labels", {
  set.seed(1)
  catalog <- round(runif(83, 10, 500))
  names(catalog) <- catalog_row_order()$ID

  p <- plot_ID83(catalog, plot_title = "Peak labels", num_peak_labels = 3)
  expect_s3_class(p, "ggplot")
})

test_that("plot_ID89 with num_peak_labels renders peak labels", {
  set.seed(1)
  catalog <- round(runif(89, 10, 500))
  names(catalog) <- catalog_row_order()$ID89

  p <- plot_ID89(catalog, plot_title = "Peak labels", num_peak_labels = 3)
  expect_s3_class(p, "ggplot")
})

test_that("plot_ID166 with num_peak_labels renders peak labels", {
  set.seed(1)
  catalog <- round(runif(166, 10, 500))
  names(catalog) <- catalog_row_order()$ID166

  p <- plot_ID166(catalog, plot_title = "Peak labels", num_peak_labels = 3)
  expect_s3_class(p, "ggplot")
})

test_that("plot_SBS192 with num_peak_labels renders peak labels", {
  set.seed(1)
  catalog <- round(runif(192, 10, 500))
  names(catalog) <- catalog_row_order()$SBS192

  p <- plot_SBS192(catalog, plot_title = "Peak labels", num_peak_labels = 3)
  expect_s3_class(p, "ggplot")
})
