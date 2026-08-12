# Visual regression tests

Two independent systems guard plot appearance. A change to a label, colour,
or layout usually requires re-blessing **both**.

| | PNG-hash tests | Eyeball PNGs |
|---|---|---|
| Run by | `devtools::test()` | you, by hand |
| Code | `tests/testthat/test-regression-plots.R` | `tests/visual/generate_visual_tests.R` |
| Baselines | `tests/testthat/fixtures/reference_hashes/*.hash` | `tests/visual/reference/*.png` |
| Detects | any pixel change (pass/fail) | what actually changed (you look at it) |

This directory holds the second system. `tests/visual` is in `.Rbuildignore`,
so none of it ships in the package tarball.

## Directory layout

- `generate_visual_tests.R` , renders one PNG per plot type into `new/`.
  It never writes to `reference/`.
- `new/` , freshly generated output. Git-ignored (`tests/visual/new/*.png`
  in `.gitignore`), so these files never show up as modified.
- `reference/` , the blessed baseline, committed to git. What the plots are
  supposed to look like.
- `promote_visual_tests.sh` , copies PNGs from `new/` to `reference/`.

## Regenerating `new/`

From the top of the repo:

```bash
Rscript tests/visual/generate_visual_tests.R
```

The script uses `here::here()` for every path, so it works from any working
directory. Run it with `Rscript` from the repo root anyway, running it with
the working directory inside `tests/testthat/` has in the past left a stray
`tests/testthat/tests/visual/` tree behind.

## Comparing against `reference/`

```bash
cd tests/visual
for f in reference/*.png; do
  cmp -s "$f" "new/$(basename "$f")" || echo "DIFFER $(basename "$f")"
done
```

Then open the differing PNGs and confirm the change is the one you intended.
Do not promote a PNG you have not looked at.

## Promoting `new/` to `reference/`

Only after you have eyeballed the diffs:

```bash
tests/visual/promote_visual_tests.sh plot_ID89 plot_ID89_peaks  # named plots
tests/visual/promote_visual_tests.sh --all                      # everything
```

Then `git add tests/visual/reference/` and commit alongside the code change.

Promote only the plots your change was supposed to affect. `new/` can carry
stale PNGs from an earlier session, so `--all` may quietly bless something
unrelated.

## Re-blessing the PNG-hash tests

The other system, kept in step by hand:

```bash
rm tests/testthat/fixtures/reference_hashes/plot_ID89.hash
R -e "devtools::test(filter='regression')"   # recreates the hash, test SKIPs
R -e "devtools::test(filter='regression')"   # now it PASSes
git add tests/testthat/fixtures/reference_hashes/
```

Hashing needs `pixi` on `PATH` (it runs `hash_png.py` under Pillow). Without
it the regression tests skip rather than fail. `SBS1536` and `DBS136` are
excluded from hashing, their hashes are not stable across Cairo and font
versions, so `reference/` is their only coverage.

## Coverage gaps

- No `*_pdf()` function has a visual reference. `test-plot_89.R` only checks
  that a PDF file is created.
- `plot_SBS288` and `plot_DBS136` appear in `reference/` but have no hash test.
