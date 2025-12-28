# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

mSigPlot is an R package for visualizing mutational signatures, specifically indel (insertion/deletion) profiles. It creates publication-quality bar plots for two classification schemes:
- **89-channel scheme**: Simpler categorization with 89 indel types
- **476-channel scheme**: More granular with 476 indel types including flanking base context

## Development Commands

```bash
# Load package for development
R -e "devtools::load_all()"

# Run tests
R -e "devtools::test()"

# Generate documentation from roxygen2 comments
R -e "devtools::document()"

# Check package
R -e "devtools::check()"

# Build package
R -e "devtools::build()"

# Install locally
R -e "devtools::install()"
```

## Architecture

### Core Functions

- `plot_89()` - Main 89-channel plotting function (R/plot_89_functions.R)
- `plot_476()` - Main 476-channel plotting function with smart labeling (R/plot_476.R)
- `plot_89_pdf()` - Batch PDF export for 89-channel plots (R/plot_89_functions.R)
- `plot_476_pdf()` - Batch PDF export for 476-channel plots (R/plot_476_pdf.R)

### Data Objects

- `Koh89_indeltype` - Data frame defining 89 indel type classifications
- `Koh476_indeltype` - Data frame defining 476 indel type classifications

Both data objects contain columns: IndelType, Indel, Indel3, Figlabel

### Dependencies (used in code but not yet in DESCRIPTION)

ggplot2, dplyr, reshape2, ggrepel, magrittr, gridExtra, Cairo

## Key Implementation Notes

- Plotting functions expect catalog input as numeric vectors matching the 89 or 476 channel order
- Color palettes are hardcoded for consistent signature visualization
- PDF export functions arrange 5 plots per page vertically
- The 476-channel plot uses ggrepel for intelligent peak labeling
- Y-axis automatically switches between "Proportion" (sum < 1.1) and "Count"
