test_that("plot_guess dispatches to plot_476 for 476-row catalog", {
  catalog <- data.frame(sample1 = runif(476))
  p <- plot_guess(catalog)
  expect_s3_class(p, "ggplot")
})

test_that("plot_guess dispatches to plot_89 for 89-row catalog", {
  catalog <- data.frame(sample1 = runif(89))
  p <- plot_guess(catalog)
  expect_s3_class(p, "ggplot")
})

test_that("plot_guess dispatches to plot_83 for 83-row catalog", {
  catalog <- data.frame(sample1 = runif(83))
  p <- plot_guess(catalog)
  expect_s3_class(p, "ggplot")
})

test_that("plot_guess errors for unsupported row counts", {
  catalog <- data.frame(sample1 = runif(100))
  expect_error(plot_guess(catalog), "Unexpected number of rows")
})

test_that("plot_guess passes additional arguments", {
  catalog <- data.frame(sample1 = runif(476))
  p <- plot_guess(catalog, plot_title = "Test Title")
  expect_s3_class(p, "ggplot")
})

test_that("plot_guess_pdf creates a PDF file", {
  catalog <- data.frame(
    sample1 = runif(476),
    sample2 = runif(476)
  )
  tmp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp_file), add = TRUE)

  plot_guess_pdf(catalog, tmp_file)

  expect_true(file.exists(tmp_file))
  expect_gt(file.size(tmp_file), 0)
})

test_that("plot_guess_pdf works with 89-channel catalogs", {
  catalog <- data.frame(
    sample1 = runif(89),
    sample2 = runif(89)
  )
  tmp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp_file), add = TRUE)

  plot_guess_pdf(catalog, tmp_file)

  expect_true(file.exists(tmp_file))
})

test_that("plot_guess_pdf passes additional arguments", {
  catalog <- data.frame(sample1 = runif(476))
  tmp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp_file), add = TRUE)

  plot_guess_pdf(catalog, tmp_file, num_labels = 2)

  expect_true(file.exists(tmp_file))
})
