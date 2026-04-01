test_that("normalize_catalog warns and returns NULL for wrong row count", {
  expect_warning(
    result <- normalize_catalog(
      runif(50), expected_nrow = 96,
      canonical_order = catalog_row_order()$SBS96, type_label = "SBS96"
    ),
    "Expected 96 rows but got 50 rows"
  )
  expect_null(result)
})

test_that("normalize_catalog warns and returns NULL for mismatched row names", {
  bad_catalog <- runif(96)
  names(bad_catalog) <- paste0("wrong_", seq_len(96))
  expect_warning(
    result <- normalize_catalog(
      bad_catalog, expected_nrow = 96,
      canonical_order = catalog_row_order()$SBS96, type_label = "SBS96"
    ),
    "do not match canonical SBS96 row names"
  )
  expect_null(result)
})

test_that("normalize_catalog accepts correct input", {
  sig <- runif(96)
  names(sig) <- catalog_row_order()$SBS96
  result <- normalize_catalog(
    sig, expected_nrow = 96,
    canonical_order = catalog_row_order()$SBS96, type_label = "SBS96"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 96)
})

test_that("plot functions return NULL for wrong row names", {
  bad_catalog <- runif(96)
  names(bad_catalog) <- paste0("wrong_", seq_len(96))
  expect_warning(
    result <- plot_SBS96(bad_catalog),
    "do not match canonical SBS96 row names"
  )
  expect_null(result)
})

test_that("plot functions return NULL for wrong-sized catalog", {
  expect_warning(
    result <- plot_SBS96(runif(50)),
    "Expected 96 rows but got 50 rows"
  )
  expect_null(result)
})
