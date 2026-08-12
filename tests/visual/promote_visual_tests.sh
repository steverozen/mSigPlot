#!/usr/bin/env bash
#
# Promote visual regression PNGs from tests/visual/new/ to tests/visual/reference/.
#
# Usage:
#   tests/visual/promote_visual_tests.sh plot_ID89 plot_ID89_peaks
#   tests/visual/promote_visual_tests.sh --all
#
# Regenerate new/ first:
#   Rscript tests/visual/generate_visual_tests.R
#
# Look at the PNGs before promoting them. See tests/visual/README.md.

set -euo pipefail

visual_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
new_dir="$visual_dir/new"
ref_dir="$visual_dir/reference"

if [ $# -eq 0 ]; then
  echo "usage: $(basename "$0") <plot_name>... | --all" >&2
  echo "       names are given without the .png suffix" >&2
  exit 2
fi

if [ "$1" = "--all" ]; then
  names=()
  for f in "$new_dir"/*.png; do
    names+=("$(basename "$f" .png)")
  done
else
  names=("$@")
fi

for name in "${names[@]}"; do
  src="$new_dir/${name%.png}.png"
  dest="$ref_dir/${name%.png}.png"
  if [ ! -f "$src" ]; then
    echo "missing: $src" >&2
    exit 1
  fi
  if cmp -s "$src" "$dest"; then
    echo "unchanged: $(basename "$dest")"
  else
    cp "$src" "$dest"
    echo "promoted:  $(basename "$dest")"
  fi
done

echo
echo "Now commit the baseline:"
echo "  git add tests/visual/reference/"
