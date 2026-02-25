test_that("plot_SBS288_test returns a patchwork object", {
  fixture_path <- testthat::test_path("fixtures", "21BRCA.SBS96.tsv")
  sbs96 <- read.table(
    fixture_path,
    header = TRUE,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )

  # Build a 288-row catalog by prepending T:, U:, N: to stapled row names
  rn96 <- rownames(sbs96)
  rn288 <- c(paste0("T:", rn96), paste0("U:", rn96), paste0("N:", rn96))

  # Use first sample column, replicated 3x with slight variation
  vals <- sbs96[, 1]
  catalog_288 <- data.frame(
    sample1 = c(vals, vals * 0.8, vals * 0.5),
    row.names = rn288
  )

  p <- plot_SBS288_test(catalog_288, plot_title = "Test SBS288")

  expect_s3_class(p, "patchwork")
})

test_that("plot_SBS288_test prints without error", {
  fixture_path <- testthat::test_path("fixtures", "21BRCA.SBS96.tsv")
  sbs96 <- read.table(
    fixture_path,
    header = TRUE,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )

  rn96 <- rownames(sbs96)
  rn288 <- c(paste0("T:", rn96), paste0("U:", rn96), paste0("N:", rn96))
  vals <- sbs96[, 1]
  catalog_288 <- data.frame(
    sample1 = c(vals, vals * 0.8, vals * 0.5),
    row.names = rn288
  )

  p <- plot_SBS288_test(catalog_288)

  # Printing/rendering should not error
  expect_no_error(print(p))
})

test_that("plot_SBS288_test works with real SBS288 signature file", {
  fixture_path <- testthat::test_path("fixtures", "SBS288_De-Novo_Signatures.txt")
  sbs288 <- read.table(
    fixture_path,
    header = TRUE,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )

  p <- plot_SBS288_test(sbs288[, 1, drop = FALSE], plot_title = "SBS288A")

  expect_s3_class(p, "patchwork")
  expect_no_error(print(p))
})
