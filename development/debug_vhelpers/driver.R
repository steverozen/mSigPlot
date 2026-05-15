# Driver for debugging mSigPlot::plot_ID89 / plot_ID83 / plot_ID476.
#
# Loads the in-development mSigPlot source via devtools::load_all() and
# invokes generate_plots_to_files() for ONE signature, exercising the
# exact production call sites for p89(), p83(), and p476().
#
# Reads from ~/github/Code_Liu_2025/. Writes only to this directory.

DEBUG_DIR <- "/home/steve/github/mSigPlot/development/debug_vhelpers"

devtools::load_all("/home/steve/github/mSigPlot")

# vhelpers.R sets CODE_LIU_DIR / DEBUG_OUT_DIR and sources the support
# files from Code_Liu_2025.
source(file.path(DEBUG_DIR, "vhelpers.R"))

# Plotting parameters (defines ppar and getp()).
source(file.path(CODE_LIU_DIR, "vignette", "ppar.R"))

# Load signatures and spectra.
ID89_signatures <- read_finalized("89_signatures")
ID89_catalogs   <- read_finalized("89_spectra")
ID83_signatures <- read_finalized("83_signatures")
ID83_catalogs   <- read_finalized("83_spectra")
ID476_signatures <- read_finalized("476_signatures")
ID476_catalogs   <- read_finalized("476_spectra")

connection_table <- read_finalized("connection_table", row.names = NULL)

# Pick a signature present in all three signature catalogs. InsDel1a is a
# safe default — it is the first row of the connection table and has
# matches in 89, 83, and 476.
type89_sig_id <- "InsDel1a"
stopifnot(type89_sig_id %in% colnames(ID89_signatures))
stopifnot(type89_sig_id %in% colnames(ID476_signatures))

ct_row <- connection_table[connection_table$InDel89 == type89_sig_id, ][1, ]
ID83signature <- ct_row$InDel83
exemplar_89   <- ct_row$BestMatch89_1
exemplar_83   <- ct_row$BestMatch83_1
exemplar_476  <- ct_row$BestMatch476_1

stopifnot(ID83signature %in% colnames(ID83_signatures))
stopifnot(exemplar_89 %in% colnames(ID89_catalogs))
stopifnot(exemplar_83 %in% colnames(ID83_catalogs))
stopifnot(exemplar_476 %in% colnames(ID476_catalogs))

# Minimal sig_data — only the fields generate_plots_to_files reads. We
# bypass compute_sig_data() (which needs mSigBG and does extra work) by
# marking is_insdel15_16 = TRUE so the residual / partial-spectrum
# branches are skipped.
ID83_catalogs_no_polyT <- ID83_catalogs

sig_data <- list(
  type89_sig_id            = type89_sig_id,
  exemplar_89              = exemplar_89,
  exemplar_83              = exemplar_83,
  exemplar_476             = exemplar_476,
  ID83signature            = ID83signature,
  is_insdel15_16           = TRUE,
  is_polyT_removed         = FALSE,
  has_476_signature        = TRUE,
  has_83_signature         = TRUE,
  has_mapped_476_sig       = FALSE,
  has_83_mapped_signature  = FALSE,
  cosmic_matches           = NULL,
  jin_matches              = NULL,
  koh_matches              = NULL,
  residual_spectrum        = NULL,
  target_sig_partial_spectrum = NULL,
  cosine89                 = NA,
  cosine89_mapped          = NA,
  cosine476                = NA,
  cosine476_linking        = NA,
  cosine83                 = NA,
  cosine83_linking         = NA,
  cosine83_mapped          = NA
)

paths <- generate_plots_to_files(
  sig_data                = sig_data,
  ID89_signatures         = ID89_signatures,
  ID89_catalogs           = ID89_catalogs,
  ID83_signatures         = ID83_signatures,
  ID83_catalogs           = ID83_catalogs,
  ID83_catalogs_no_polyT  = ID83_catalogs_no_polyT,
  ID476_signatures        = ID476_signatures,
  ID476_catalogs          = ID476_catalogs,
  plot_dir                = DEBUG_OUT_DIR
)

message("Wrote plots:")
for (nm in names(paths)) {
  p <- paths[[nm]]
  if (is.character(p) && length(p) == 1 && file.exists(p)) {
    message("  ", nm, ": ", p)
  }
}

# Open the three primary outputs for visual inspection.
key_plots <- c(paths$id89_sig, paths$id83_sig, paths$id476_sig,
               paths$id89_catalog, paths$id83_catalog, paths$id476_catalog)
for (p in key_plots) {
  if (!is.null(p) && file.exists(p)) {
    system2("xdg-open", p, stdout = NULL, stderr = NULL, wait = FALSE)
  }
}
