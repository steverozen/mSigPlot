# Tests that user-facing plot functions reject a malformed `ylim`
# (scalar, length != 2, or non-numeric) and accept NULL or a length-2
# numeric vector. Matches ggplot2's `scale_y_continuous(limits = ...)`
# contract.

make_named_vec <- function(n, key) {
  set.seed(42)
  v <- runif(n)
  names(v) <- catalog_row_order()[[key]]
  v
}

# (plot_fn, channel_count, row-order key) for every user-facing plot
# function that exposes a `ylim` parameter.
cases <- list(
  list(plot_ID89,   89,  "ID89"),
  list(plot_ID83,   83,  "ID"),
  list(plot_ID166, 166,  "ID166"),
  list(plot_SBS96,  96,  "SBS96"),
  list(plot_SBS192, 192, "SBS192"),
  list(plot_SBS12,  192, "SBS192"),
  list(plot_DBS78,  78,  "DBS78"),
  list(plot_DBS144, 144, "DBS144")
)

test_that("scalar ylim is rejected with a length-2 message", {
  for (case in cases) {
    fn <- case[[1]]
    catalog <- make_named_vec(case[[2]], case[[3]])
    expect_error(
      fn(catalog, ylim = 350),
      "must be a numeric vector of length 2"
    )
  }
})

test_that("non-numeric ylim is rejected", {
  catalog <- make_named_vec(83, "ID")
  expect_error(
    plot_ID83(catalog, ylim = c("a", "b")),
    "must be a numeric vector of length 2"
  )
})

test_that("length-3 ylim is rejected", {
  catalog <- make_named_vec(89, "ID89")
  expect_error(
    plot_ID89(catalog, ylim = c(0, 100, 200)),
    "must be a numeric vector of length 2"
  )
})

test_that("NULL ylim is accepted (auto)", {
  for (case in cases) {
    fn <- case[[1]]
    catalog <- make_named_vec(case[[2]], case[[3]])
    expect_no_error(fn(catalog, ylim = NULL))
  }
})

test_that("length-2 numeric ylim is accepted", {
  for (case in cases) {
    fn <- case[[1]]
    catalog <- make_named_vec(case[[2]], case[[3]])
    expect_no_error(fn(catalog, ylim = c(0, 1)))
  }
})

test_that("check_ylim helper itself enforces the contract", {
  expect_error(check_ylim(350), "must be a numeric vector of length 2")
  expect_error(check_ylim(c(1, 2, 3)), "must be a numeric vector of length 2")
  expect_error(check_ylim("a"), "must be a numeric vector of length 2")
  expect_silent(check_ylim(NULL))
  expect_silent(check_ylim(c(0, 100)))
})
