# Manual test for plot_SBS288
# Run from the package root: source("development/tmp/test_plot_SBS288.R")

devtools::load_all()

sbs288 <- read.table(
  "tests/testthat/fixtures/SBS288_De-Novo_Signatures.txt",
  header = TRUE, sep = "\t", row.names = 1, check.names = FALSE
)

p <- plot_SBS288(sbs288[, "SBS288E", drop = FALSE],
                      plot_title = "SBS288E")

outfile <- "/tmp/test_SBS288.pdf"
ggplot2::ggsave(outfile, p, width = 12, height = 14)
system2("xdg-open", outfile)
cat("Saved and opened", outfile, "\n")
