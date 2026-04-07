test_that("plot_ID83 returns a ggplot object", {
  fixture_path <- testthat::test_path(
    "fixtures",
    "COSMIC_v3.5_ID_GRCh37_signatures.tsv"
  )
  sig_data <- read.table(
    fixture_path,
    header = TRUE,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )

  catalog <- sig_data[, 1, drop = FALSE]

  p <- plot_ID83(
    catalog = catalog,
    plot_title = colnames(sig_data)[1]
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_ID83 handles base_size", {
  fixture_path <- testthat::test_path(
    "fixtures",
    "COSMIC_v3.5_ID_GRCh37_signatures.tsv"
  )
  sig_data <- read.table(
    fixture_path,
    header = TRUE,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )

  catalog <- sig_data[, 1, drop = FALSE]

  p <- plot_ID83(
    catalog = catalog,
    plot_title = "With base_size",
    base_size = 30
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_ID83 handles cex parameters", {
  fixture_path <- testthat::test_path(
    "fixtures",
    "COSMIC_v3.5_ID_GRCh37_signatures.tsv"
  )
  sig_data <- read.table(
    fixture_path,
    header = TRUE,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )

  catalog <- sig_data[, 1, drop = FALSE]

  p <- plot_ID83(
    catalog = catalog,
    plot_title = "Custom cex",
    base_size = 30,
    plot_title_cex = 1.0,
    count_label_cex = 0.8,
    block_label_cex = 0.7,
    class_label_cex = 0.9,
    axis_text_x_cex = 0.6,
    bottom_label_cex = 0.7,
    axis_title_y_cex = 1.2,
    axis_text_y_cex = 1.0
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_ID83 handles count vs proportion y-axis label", {
  fixture_path <- testthat::test_path(
    "fixtures",
    "COSMIC_v3.5_ID_GRCh37_signatures.tsv"
  )
  sig_data <- read.table(
    fixture_path,
    header = TRUE,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )

  # Proportions (sum <= 1.1)
  catalog_prop <- sig_data[, 1, drop = FALSE]
  p_prop <- plot_ID83(catalog = catalog_prop, plot_title = "Proportions")
  expect_s3_class(p_prop, "ggplot")

  # Counts (multiply to get sum > 1.1)
  catalog_count <- catalog_prop * 1000
  p_count <- plot_ID83(catalog = catalog_count, plot_title = "Counts")
  expect_s3_class(p_count, "ggplot")
})

test_that("plot_ID83 count labels render with count catalog", {
  fixture_path <- testthat::test_path(
    "fixtures",
    "COSMIC_v3.5_ID_GRCh37_signatures.tsv"
  )
  sig_data <- read.table(
    fixture_path,
    header = TRUE,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )

  # Multiply by 100 to create a count-type catalog (sum >> 1.1)
  catalog_count <- sig_data[, "ID6", drop = FALSE] * 100

  p <- plot_ID83(
    catalog = catalog_count,
    plot_title = "ID6 x100 counts",
    count_label_cex = 0.8
  )

  expect_s3_class(p, "ggplot")

  # Verify the count labels layer is present by checking the built plot data
  pb <- ggplot2::ggplot_build(p)
  # Count labels are a geom_text layer; find one whose data has the count values
  has_count_layer <- any(vapply(pb$data, function(d) {
    "label" %in% names(d) && any(grepl("^[0-9]+$", as.character(d$label)))
  }, logical(1)))
  expect_true(has_count_layer)
})

test_that("plot_ID83 show_counts = FALSE suppresses count labels", {
  fixture_path <- testthat::test_path(
    "fixtures",
    "COSMIC_v3.5_ID_GRCh37_signatures.tsv"
  )
  sig_data <- read.table(
    fixture_path,
    header = TRUE,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )

  catalog_count <- sig_data[, "ID6", drop = FALSE] * 100

  p <- plot_ID83(
    catalog = catalog_count,
    plot_title = "No counts",
    show_counts = FALSE
  )

  expect_s3_class(p, "ggplot")

  # Count labels (multi-digit numbers) should NOT be present
  # (single-digit "2", "3", "4" are block category labels, not count labels)
  pb <- ggplot2::ggplot_build(p)
  has_count_layer <- any(vapply(pb$data, function(d) {
    "label" %in% names(d) && any(grepl("^[0-9]{2,}$", as.character(d$label)))
  }, logical(1)))
  expect_false(has_count_layer)
})
