t83 = read.delim(
  "tests/testthat/fixtures/COSMIC_v3.5_ID_GRCh37_signatures.tsv",
  sep = '\t'
)
plot_83(t83[, 9, drop = FALSE])

t89 = read.delim(
  "tests/testthat/fixtures/type89_liu_et_al_sigs.tsv",
  sep = '\t'
)
plot_89(t89[, 9, drop = FALSE])
plot_89(100 * t89[, 9, drop = FALSE])
