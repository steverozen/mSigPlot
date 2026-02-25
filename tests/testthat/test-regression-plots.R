# Regression tests for plot functions via PNG hashing.
#
# SBS1536 and DBS136 are excluded because their PNG hashes are not stable
# across environments (font rendering, Cairo versions, etc.).
#
# On first run, reference hash files are created and tests skip.
# On subsequent runs, hashes are compared against references.
# To re-bless after intentional visual changes:
#   rm tests/testthat/fixtures/reference_hashes/*.hash
#   R -e "devtools::test(filter='regression')"
#   git add tests/testthat/fixtures/reference_hashes/

# -- Helpers ------------------------------------------------------------------

load_icams_catalog <- function(filename, type) {
  df <- read.csv(test_path("fixtures", filename))
  orders <- catalog_row_order()

  if (type == "SBS96") {
    vals <- df[, 3]
    out <- data.frame(sample1 = vals, row.names = orders$SBS96)

  } else if (type == "SBS192") {
    vals <- df[, 4]
    out <- data.frame(sample1 = vals, row.names = orders$SBS192)

  } else if (type == "DBS78") {
    rn <- paste0(df$Ref, df$Var)
    vals <- df[, 3]
    out <- data.frame(sample1 = vals, row.names = rn)

  } else if (type == "DBS144") {
    rn <- paste0(df$Ref, df$Var)
    vals <- df[, 3]
    out <- data.frame(sample1 = vals, row.names = rn)
  }

  out
}

skip_if_no_pixi <- function() {
  pixi_toml <- test_path("..", "..", "pixi.toml")
  skip_if_not(file.exists(pixi_toml), "pixi.toml not found (running inside R CMD check?)")
  skip_if(Sys.which("pixi") == "", "pixi not on PATH")
}

render_and_hash <- function(plot_obj, name) {
  tmpfile <- tempfile(fileext = ".png")

  grDevices::png(tmpfile, width = 800, height = 600, type = "cairo")

  suppressWarnings(
    if (inherits(plot_obj, "ggplot")) {
      print(plot_obj)
    } else {
      grid::grid.draw(plot_obj)
    }
  )
  grDevices::dev.off()

  hash <- system2(
    "pixi",
    c("run", "--manifest-path",
      normalizePath(test_path("..", "..", "pixi.toml")),
      "python3", test_path("hash_png.py"), tmpfile),
    stdout = TRUE
  )
  unlink(tmpfile)
  hash
}

check_regression <- function(hash, name) {
  ref_dir <- test_path("fixtures", "reference_hashes")
  ref_path <- file.path(ref_dir, paste0(name, ".hash"))

  if (!file.exists(ref_path)) {
    writeLines(hash, ref_path)
    skip(paste0("Reference hash created for ", name, ". Run tests again after blessing."))
  }

  ref_hash <- readLines(ref_path)
  expect_equal(hash, ref_hash, label = paste0("PNG hash for ", name))
}

# -- Tests --------------------------------------------------------------------

test_that("plot_SBS96 regression", {
  skip_if_no_pixi()
  catalog <- load_icams_catalog("regress.cat.sbs.96.csv", "SBS96")
  p <- plot_SBS96(catalog)
  hash <- render_and_hash(p, "plot_SBS96")
  check_regression(hash, "plot_SBS96")
})

test_that("plot_SBS192 regression", {
  skip_if_no_pixi()
  catalog <- load_icams_catalog("regress.cat.sbs.192.csv", "SBS192")
  p <- plot_SBS192(catalog)
  hash <- render_and_hash(p, "plot_SBS192")
  check_regression(hash, "plot_SBS192")
})

test_that("plot_SBS12 regression", {
  skip_if_no_pixi()
  catalog <- load_icams_catalog("regress.cat.sbs.192.csv", "SBS192")
  p <- plot_SBS12(catalog)
  hash <- render_and_hash(p, "plot_SBS12")
  check_regression(hash, "plot_SBS12")
})

test_that("plot_DBS78 regression", {
  skip_if_no_pixi()
  catalog <- load_icams_catalog("regress.cat.dbs.78.csv", "DBS78")
  p <- plot_DBS78(catalog)
  hash <- render_and_hash(p, "plot_DBS78")
  check_regression(hash, "plot_DBS78")
})

test_that("plot_DBS144 regression", {
  skip_if_no_pixi()
  catalog <- load_icams_catalog("regress.cat.dbs.144.csv", "DBS144")
  p <- plot_DBS144(catalog)
  hash <- render_and_hash(p, "plot_DBS144")
  check_regression(hash, "plot_DBS144")
})

test_that("plot_83 regression", {
  skip_if_no_pixi()
  sig_data <- read.table(
    test_path("fixtures", "COSMIC_v3.5_ID_GRCh37_signatures.tsv"),
    header = TRUE, sep = "\t", row.names = 1, check.names = FALSE
  )
  catalog <- sig_data[, 1, drop = FALSE]
  p <- plot_83(catalog)
  hash <- render_and_hash(p, "plot_83")
  check_regression(hash, "plot_83")
})

test_that("plot_89 regression", {
  skip_if_no_pixi()
  sig_data <- read.table(
    test_path("fixtures", "type89_liu_et_al_sigs.tsv"),
    header = TRUE, sep = "\t", row.names = 1, check.names = FALSE
  )
  catalog <- as.numeric(sig_data[, 1])
  p <- plot_89(catalog, plot_title = "SBS89_regress")
  hash <- render_and_hash(p, "plot_89")
  check_regression(hash, "plot_89")
})

test_that("plot_476 regression", {
  skip_if_no_pixi()
  sig_data <- read.table(
    test_path("fixtures", "type476_liu_et_al_sigs.tsv"),
    header = TRUE, sep = "\t", row.names = 1, check.names = FALSE
  )
  catalog <- as.numeric(sig_data[, 1])
  p <- plot_476(catalog, plot_title = "ID476_regress")
  set.seed(1) # ggrepel uses randomness at render time
  hash <- render_and_hash(p, "plot_476")
  check_regression(hash, "plot_476")
})

test_that("plot_476_right regression", {
  skip_if_no_pixi()
  sig_data <- read.table(
    test_path("fixtures", "type476_liu_et_al_sigs.tsv"),
    header = TRUE, sep = "\t", row.names = 1, check.names = FALSE
  )
  catalog <- as.numeric(sig_data[, 1])
  p <- plot_476_right(catalog, plot_title = "ID476R_regress")
  set.seed(1) # ggrepel uses randomness at render time
  hash <- render_and_hash(p, "plot_476_right")
  check_regression(hash, "plot_476_right")
})

test_that("plot_ID166 regression", {
  skip_if_no_pixi()
  set.seed(42)
  orders <- catalog_row_order()
  vals <- runif(166)
  catalog <- data.frame(sample1 = vals, row.names = orders$ID166)
  p <- plot_ID166(catalog)
  hash <- render_and_hash(p, "plot_ID166")
  check_regression(hash, "plot_ID166")
})
