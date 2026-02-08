test_that("plot_83 returns a ggplot object", {
  # Load fixture data

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

  # Test with first signature column
  catalog <- sig_data[, 1, drop = FALSE]

  p <- plot_83(
    catalog = catalog,
    text_size = 3,
    plot_title = colnames(sig_data)[1]
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_83 handles base_size", {
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

  p <- plot_83(
    catalog = catalog,
    plot_title = "With base_size",
    base_size = 30
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_83 handles count_text_size", {
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

  p <- plot_83(
    catalog = catalog,
    plot_title = "With base_size",
    base_size = 30,
    text_size = 7,
    count_label_size = 7
  )

  expect_s3_class(p, "ggplot")
})


test_that("plot_83 handles count vs proportion ylabel", {
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
  p_prop <- plot_83(catalog = catalog_prop, plot_title = "Proportions")
  expect_s3_class(p_prop, "ggplot")

  # Counts (multiply to get sum > 1.1)
  catalog_count <- catalog_prop * 1000
  p_count <- plot_83(catalog = catalog_count, plot_title = "Counts")
  expect_s3_class(p_count, "ggplot")
})
