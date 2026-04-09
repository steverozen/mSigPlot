#!/usr/bin/env Rscript
#
# Generate visual regression PNGs for all plot types.
#
# Usage:
#   Rscript tests/visual/generate_visual_tests.R
#
# Outputs PNGs to tests/visual/new/. Compare against tests/visual/reference/.

devtools::load_all(here::here())

fixture_dir <- here::here("tests", "testthat", "fixtures")
out_dir     <- here::here("tests", "visual", "new")
ref_dir     <- here::here("tests", "visual", "reference")

# -- Helpers ------------------------------------------------------------------

load_csv_catalog <- function(filename, type) {
  df <- read.csv(file.path(fixture_dir, filename))
  orders <- catalog_row_order()

  if (type == "SBS96") {
    data.frame(sample1 = df[, 3], row.names = orders$SBS96)
  } else if (type == "SBS192") {
    data.frame(sample1 = df[, 4], row.names = orders$SBS192)
  } else if (type == "SBS1536") {
    data.frame(sample1 = df[, 3], row.names = orders$SBS1536)
  } else if (type == "DBS78") {
    data.frame(sample1 = df[, 3], row.names = paste0(df$Ref, df$Var))
  } else if (type == "DBS136") {
    data.frame(sample1 = df[, 2], row.names = df$Quad)
  } else if (type == "DBS144") {
    data.frame(sample1 = df[, 3], row.names = paste0(df$Ref, df$Var))
  }
}

load_tsv_catalog <- function(filename) {
  read.table(
    file.path(fixture_dir, filename),
    header = TRUE, sep = "\t", row.names = 1, check.names = FALSE
  )
}

save_ggplot <- function(p, name, width = 800, height = 600) {
  path <- file.path(out_dir, paste0(name, ".png"))
  ggplot2::ggsave(path, plot = p, width = width / 96, height = height / 96,
                  dpi = 96, device = grDevices::png, type = "cairo")
  message("  -> ", path)
}


# -- Generate plots -----------------------------------------------------------

message("Generating visual regression PNGs into ", out_dir, "/\n")

# 1. plot_SBS96
message("[1/13] plot_SBS96")
cat96 <- load_csv_catalog("regress.cat.sbs.96.csv", "SBS96")
save_ggplot(plot_SBS96(cat96), "plot_SBS96")

# 2. plot_SBS192
message("[2/13] plot_SBS192")
cat192 <- load_csv_catalog("regress.cat.sbs.192.csv", "SBS192")
save_ggplot(plot_SBS192(cat192), "plot_SBS192")

# 3. plot_SBS12
message("[3/13] plot_SBS12")
save_ggplot(plot_SBS12(cat192), "plot_SBS12")

# 4. plot_SBS288
message("[4/13] plot_SBS288")
sbs96 <- read.table(
  file.path(fixture_dir, "21BRCA.SBS96.tsv"),
  header = TRUE, sep = "\t", row.names = 1, check.names = FALSE
)
rn96 <- rownames(sbs96)
rn288 <- c(paste0("T:", rn96), paste0("U:", rn96), paste0("N:", rn96))
vals96 <- sbs96[, 1]
cat288 <- data.frame(
  sample1 = c(vals96, vals96 * 0.8, vals96 * 0.5),
  row.names = rn288
)
save_ggplot(plot_SBS288(cat288, plot_title = "SBS288"), "plot_SBS288")

# 5. plot_SBS1536
message("[5/13] plot_SBS1536")
cat1536 <- load_csv_catalog("regress.cat.sbs.1536.csv", "SBS1536")
save_ggplot(plot_SBS1536(cat1536), "plot_SBS1536", width = 1000, height = 800)

# 6. plot_DBS78
message("[6/13] plot_DBS78")
cat78 <- load_csv_catalog("regress.cat.dbs.78.csv", "DBS78")
save_ggplot(plot_DBS78(cat78), "plot_DBS78")

# 7. plot_DBS136
message("[7/13] plot_DBS136")
cat136 <- load_csv_catalog("regress.cat.dbs.136.csv", "DBS136")
save_ggplot(plot_DBS136(cat136), "plot_DBS136", width = 1000, height = 800)

# 8. plot_DBS144
message("[8/13] plot_DBS144")
cat144 <- load_csv_catalog("regress.cat.dbs.144.csv", "DBS144")
save_ggplot(plot_DBS144(cat144), "plot_DBS144")

# 9. plot_ID83
message("[9/13] plot_ID83")
sig83 <- load_tsv_catalog("COSMIC_v3.5_ID_GRCh37_signatures.tsv")
save_ggplot(plot_ID83(sig83[, 1, drop = FALSE]), "plot_ID83")

# 10. plot_ID89
message("[10/13] plot_ID89")
sig89 <- load_tsv_catalog("type89_liu_et_al_sigs.tsv")
save_ggplot(plot_ID89(as.numeric(sig89[, 1]), plot_title = "ID89"), "plot_ID89")

# 11. plot_ID476
message("[11/13] plot_ID476")
sig476 <- load_tsv_catalog("type476_liu_et_al_sigs.tsv")
p476 <- plot_ID476(as.numeric(sig476[, 1]), plot_title = "ID476")
set.seed(1)
save_ggplot(p476, "plot_ID476")

# 12. plot_ID476_right
message("[12/13] plot_ID476_right")
p476r <- plot_ID476_right(as.numeric(sig476[, 1]), plot_title = "ID476R")
set.seed(1)
save_ggplot(p476r, "plot_ID476_right")

# 13. plot_ID166
message("[13/13] plot_ID166")
set.seed(42)
orders <- catalog_row_order()
cat166 <- data.frame(sample1 = runif(166), row.names = orders$ID166)
save_ggplot(plot_ID166(cat166), "plot_ID166")

# -- Summary ------------------------------------------------------------------

new_files <- list.files(out_dir, pattern = "\\.png$")
message("\nGenerated ", length(new_files), " PNGs in ", out_dir, "/")

ref_files <- list.files(ref_dir, pattern = "\\.png$")
if (length(ref_files) == 0) {
  message("\nNo reference PNGs found. To bootstrap reference/:")
  message("  cp ", out_dir, "/*.png ", ref_dir, "/")
} else {
  message("\nCompare ", out_dir, "/ against ", ref_dir, "/")
  message("To promote all new PNGs to reference:")
  message("  cp ", out_dir, "/*.png ", ref_dir, "/")
}
