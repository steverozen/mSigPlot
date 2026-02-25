test_that("plot_SBS96 accepts stapled row names like A[C>A]T", {
  sig_data <- read.table(
    test_path("fixtures", "21BRCA.SBS96.tsv"),
    header = TRUE, sep = "\t", row.names = 1, check.names = FALSE
  )
  catalog <- sig_data[, 1, drop = FALSE]

  # Confirm row names are in stapled format
  expect_true(all(grepl("^[ACGT]\\[[CT]>[ACGT]\\][ACGT]$", rownames(catalog))))

  # Should produce a valid ggplot without warnings about row names
  expect_silent(p <- plot_SBS96(catalog))
  expect_s3_class(p, "ggplot")
})

test_that("plot_SBS96 handles stapled row names on a numeric vector", {
  sig_data <- read.table(
    test_path("fixtures", "21BRCA.SBS96.tsv"),
    header = TRUE, sep = "\t", row.names = 1, check.names = FALSE
  )
  vec <- sig_data[, 1]
  names(vec) <- rownames(sig_data)

  expect_silent(p <- plot_SBS96(vec))
  expect_s3_class(p, "ggplot")
})
