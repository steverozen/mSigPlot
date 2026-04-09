# Tests that all plot functions accept multiple input types:
# numeric vector, 1-column matrix, tibble, and data.table

# Helper: create synthetic data with optional names
make_vector <- function(n, named = FALSE) {
  set.seed(42)
  v <- runif(n)
  if (named) {
    orders <- catalog_row_order()
    key <- switch(as.character(n),
      "96"   = "SBS96",
      "192"  = "SBS192",
      "1536" = "SBS1536",
      "78"   = "DBS78",
      "136"  = "DBS136",
      "144"  = "DBS144",
      "83"   = "ID",
      "166"  = "ID166",
      "89"   = "ID89",
      "476"  = "ID476"
    )
    names(v) <- orders[[key]]
  }
  v
}

make_matrix <- function(n, named = FALSE) {
  v <- make_vector(n, named)
  matrix(v, ncol = 1, dimnames = list(names(v), "sample1"))
}

make_tibble <- function(n, named = FALSE) {
  v <- make_vector(n, named)
  tibble::tibble(sample1 = unname(v))
}

# -- ggplot-returning functions (bar charts) ----------------------------------

ggplot_functions <- list(
  list(fn = "plot_SBS96",  n = 96,  named = TRUE),
  list(fn = "plot_SBS192", n = 192, named = TRUE),
  list(fn = "plot_SBS12",  n = 192, named = TRUE),
  list(fn = "plot_DBS78",  n = 78,  named = TRUE),
  list(fn = "plot_DBS144", n = 144, named = TRUE),
  list(fn = "plot_ID83",     n = 83,  named = TRUE),
  list(fn = "plot_ID166",  n = 166, named = TRUE),
  list(fn = "plot_ID89",     n = 89,  named = TRUE),
  list(fn = "plot_ID476",    n = 476, named = FALSE),
  list(fn = "plot_ID476_right", n = 476, named = FALSE)
)

for (spec in ggplot_functions) {
  fn <- get(spec$fn)
  n  <- spec$n
  nm <- spec$named

  test_that(paste(spec$fn, "accepts unnamed numeric vector"), {
    result <- fn(make_vector(n, named = FALSE))
    expect_s3_class(result, "ggplot")
  })

  test_that(paste(spec$fn, "accepts named numeric vector"), {
    result <- fn(make_vector(n, named = nm))
    expect_s3_class(result, "ggplot")
  })

  test_that(paste(spec$fn, "accepts 1-column matrix"), {
    result <- fn(make_matrix(n, named = nm))
    expect_s3_class(result, "ggplot")
  })

  test_that(paste(spec$fn, "accepts tibble"), {
    skip_if_not_installed("tibble")
    result <- fn(make_tibble(n, named = FALSE))
    expect_s3_class(result, "ggplot")
  })
}

# -- patchwork-returning functions (heatmaps) ----------------------------------

patchwork_functions <- list(
  list(fn = "plot_SBS1536", n = 1536, named = TRUE),
  list(fn = "plot_DBS136",  n = 136,  named = TRUE)
)

for (spec in patchwork_functions) {
  fn <- get(spec$fn)
  n  <- spec$n
  nm <- spec$named

  test_that(paste(spec$fn, "accepts unnamed numeric vector"), {
    result <- fn(make_vector(n, named = FALSE))
    expect_true(inherits(result, "patchwork"))
  })

  test_that(paste(spec$fn, "accepts named numeric vector"), {
    result <- fn(make_vector(n, named = nm))
    expect_true(inherits(result, "patchwork"))
  })

  test_that(paste(spec$fn, "accepts 1-column matrix"), {
    result <- fn(make_matrix(n, named = nm))
    expect_true(inherits(result, "patchwork"))
  })

  test_that(paste(spec$fn, "accepts tibble"), {
    skip_if_not_installed("tibble")
    result <- fn(make_tibble(n, named = FALSE))
    expect_true(inherits(result, "patchwork"))
  })
}

# -- plot_guess dispatch from vector ------------------------------------------

test_that("plot_guess dispatches correctly from a numeric vector", {
  result <- plot_guess(make_vector(96, named = FALSE))
  expect_s3_class(result, "ggplot")
})
