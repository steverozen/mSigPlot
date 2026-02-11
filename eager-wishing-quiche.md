# Plan: Create `plot_476_right` — right portion of 476-channel plot

## Context

The 476-channel indel plot has 8 category blocks. Positions 343–476 correspond to the last 4 blocks:
- Block 5: Del(2,):R(1,9) — positions 343–390 (48 channels)
- Block 6: Ins(2,):R(0,9) — positions 391–450 (60 channels)
- Block 7: Del(2,):M(1,) — positions 451–471 (21 channels)
- Block 8: Complex — positions 472–476 (5 channels)

The user wants a new function `plot_476_right` in `R/plot_476_right.R` that plots only these 134 channels, discarding everything else.

## Files to create

- **`R/plot_476_right.R`** — copy of `R/plot_476.R`, renamed to `plot_476_right`, with changes below

## Changes (relative to `plot_476`)

1. **Rename function** to `plot_476_right`

2. **Filter `muts_basis_melt`** to `x_pos >= 343` right after the `x_pos` column is added (after line 79)

3. **Re-index x positions** — subtract 342 so positions become 1–134:
   ```r
   muts_basis_melt$x_pos <- muts_basis_melt$x_pos - 342
   ```

4. **Reduce `indel_mypalette_fill`** to only the 4 colors for blocks 5–8:
   ```r
   c("#f14432", "#4a98c9", "#61409b", "#000000")
   ```

5. **Reduce `order_entry`** to only the 4 right-side categories:
   ```r
   c("Del(2,):R(1,9)", "Ins(2,):R(0,9)", "Del(2,):M(1,)", "Complex")
   ```

6. **Recompute `blocks`** using only these 4 entries — cumsum will naturally give xmin/xmax in 1–134 space

7. **Update block labels** to only the 4 right-side labels:
   ```r
   c("Del \u22652bp", "Ins \u22652bp", "Mh", "X")
   ```

8. **Update block text colors** (`cl`) to match (4 entries)

9. **Reduce `indel_mypalette_fill_all`** to only the 4 relevant fill colors

10. **Update `flanking_patterns` and `flanking_labels`** — keep only patterns relevant to positions >= 343:
    ```r
    flanking_patterns <- c("Del2:U1:R1", "Ins\\(2,4\\):M", "Del2:M1")
    flanking_labels <- c("", "", "")
    ```

11. **Filter `flanking_blocks`** — grep against only the right-side subset of `Koh476_indeltype$IndelType` (rows >= 343), and adjust positions to 1–134 space

12. **Update x-axis limits** to `c(0.5, 134 + 0.5)`

13. **Adjust `vline_positions`** — subtract 342 and keep only those >= 1

14. **Update roxygen docs** — describe as plotting only positions >= 343

## Verification

```r
devtools::load_all()
# Use a test catalog (e.g., random or from test data)
cat <- runif(476)
p <- plot_476_right(cat)
print(p)
# Should show 134 bars across 4 category blocks
```
