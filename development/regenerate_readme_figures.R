# Regenerate the example PNGs referenced from README.md.
#
# Run from the package root after changing plot defaults:
#   R -e "devtools::load_all(); source('development/regenerate_readme_figures.R')"

library(ggplot2)

fig_dir <- here::here("man", "figures")
stopifnot(dir.exists(fig_dir))

save_fig <- function(plot_obj, name, width, height, dpi = 150) {
  path <- file.path(fig_dir, paste0(name, ".png"))
  ggsave(path, plot_obj, width = width, height = height,
         dpi = dpi, units = "in", bg = "white")
  message("wrote ", path)
}

# SBS96
sbs96_df <- read.csv(system.file("extdata", "sbs96_example.csv",
                                 package = "mSigPlot"))
catalog_sbs96 <- data.frame(sample1 = sbs96_df[, 3],
                            row.names = catalog_row_order()$SBS96)
save_fig(plot_SBS96(catalog_sbs96, plot_title = "HepG2 sample -- SBS96"),
         "example_SBS96", width = 10, height = 4)

# SBS192
sbs192_df <- read.csv(system.file("extdata", "regress.cat.sbs.192.csv",
                                  package = "mSigPlot"))
catalog_sbs192 <- data.frame(sample1 = sbs192_df[, 4],
                             row.names = catalog_row_order()$SBS192)
save_fig(plot_SBS192(catalog_sbs192, plot_title = "HepG2 -- SBS192"),
         "example_SBS192", width = 10, height = 4)

# SBS12 (derived from a 192-row catalog)
save_fig(plot_SBS12(catalog_sbs192, plot_title = "HepG2 -- SBS12 strand bias"),
         "example_SBS12", width = 6, height = 4)

# SBS1536
sbs1536_df <- read.csv(system.file("extdata", "regress.cat.sbs.1536.csv",
                                   package = "mSigPlot"))
catalog_sbs1536 <- data.frame(sample1 = sbs1536_df[, 3],
                              row.names = catalog_row_order()$SBS1536)
save_fig(plot_SBS1536(catalog_sbs1536, plot_title = "HepG2 -- SBS1536"),
         "example_SBS1536", width = 10, height = 8)

# SBS288
sbs288_df <- read.table(system.file("extdata",
                                    "SBS288_De-Novo_Signatures.txt",
                                    package = "mSigPlot"),
                        header = TRUE, sep = "\t", row.names = 1,
                        check.names = FALSE)
save_fig(plot_SBS288(sbs288_df[, 1, drop = FALSE], plot_title = "SBS288A"),
         "example_SBS288", width = 12, height = 14)

# DBS78
dbs78_df <- read.csv(system.file("extdata", "dbs78_example.csv",
                                 package = "mSigPlot"))
catalog_dbs78 <- data.frame(sample1 = dbs78_df[, 3],
                            row.names = paste0(dbs78_df$Ref, dbs78_df$Var))
save_fig(plot_DBS78(catalog_dbs78, plot_title = "HepG2 sample -- DBS78"),
         "example_DBS78", width = 10, height = 4)

# DBS144
dbs144_df <- read.csv(system.file("extdata", "regress.cat.dbs.144.csv",
                                  package = "mSigPlot"))
catalog_dbs144 <- data.frame(sample1 = dbs144_df[, 4],
                             row.names = catalog_row_order()$DBS144)
save_fig(plot_DBS144(catalog_dbs144, plot_title = "HepG2 -- DBS144"),
         "example_DBS144", width = 10, height = 4)

# DBS136
dbs136_df <- read.csv(system.file("extdata", "regress.cat.dbs.136.csv",
                                  package = "mSigPlot"), row.names = 1)
save_fig(plot_DBS136(dbs136_df[, 1, drop = FALSE],
                     plot_title = "HepG2 -- DBS136"),
         "example_DBS136", width = 10, height = 8)

# ID83
id83_sigs <- read.table(system.file("extdata", "id83_cosmic_v3.5.tsv",
                                    package = "mSigPlot"),
                        header = TRUE, sep = "\t", row.names = 1,
                        check.names = FALSE)
save_fig(plot_ID83(id83_sigs[, "ID1", drop = FALSE],
                   plot_title = "COSMIC ID1 signature"),
         "example_ID83", width = 10, height = 4)

# ID89
id89_sigs <- read.table(system.file("extdata", "type89_liu_et_al_sigs.tsv",
                                    package = "mSigPlot"),
                        header = TRUE, sep = "\t", row.names = 1,
                        check.names = FALSE)
save_fig(plot_ID89(id89_sigs[, 1, drop = FALSE],
                   plot_title = "ID89 signature"),
         "example_ID89", width = 10, height = 4)

# ID166 (synthetic — no real ID166 data ship with the package).
# Build by replicating an ID83 signature into genic/intergenic strata.
set.seed(1)
id166_genic <- as.numeric(id83_sigs[, "ID1"])
id166_intergenic <- id166_genic * runif(83, 0.4, 1.6)
id166_vec <- c(id166_genic, id166_intergenic)
catalog_id166 <- data.frame(sample1 = id166_vec,
                            row.names = catalog_row_order()$ID166)
save_fig(plot_ID166(catalog_id166, plot_title = "Example ID166 (synthetic)"),
         "example_ID166", width = 10, height = 4)

# ID476
id476_sigs <- read.table(system.file("extdata", "type476_liu_et_al_sigs.tsv",
                                     package = "mSigPlot"),
                         header = TRUE, sep = "\t", row.names = 1,
                         check.names = FALSE)
save_fig(plot_ID476(id476_sigs[, 1, drop = FALSE],
                    plot_title = "ID476 signature"),
         "example_ID476", width = 10, height = 4)
