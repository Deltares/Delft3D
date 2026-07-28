#!/bin/bash
set -euo pipefail

BIN_DIR="$1"
CONAN_GENERATORS_DIR="$2"
LIB_DIR="$(realpath --canonicalize-missing "${BIN_DIR}/../lib")"

# Activate the Conan run environment, which prepends every Conan package library
# directory to LD_LIBRARY_PATH, so `ldd` resolves the Conan libraries first.
# This scales automatically as more dependencies move to Conan.
if [[ -f "$CONAN_GENERATORS_DIR/conanrun.sh" ]]; then
    source "$CONAN_GENERATORS_DIR/conanrun.sh"
else
    echo "ERROR: $CONAN_GENERATORS_DIR/conanrun.sh not found; refusing to run as ldd" \
         "could resolve system libraries instead of the Conan-provided ones." >&2
    exit 1
fi

# Find the runtime dependencies of every executable and copy them to LIB_DIR.
mkdir --parents "$LIB_DIR"
find "$BIN_DIR" -type f -executable -print0 \
    | { xargs --null --no-run-if-empty ldd 2>/dev/null || true; } \
    | awk '/=>/ {print $3}' \
    | grep --invert-match '^$' \
    | sort --unique \
    | while read -r lib; do
          # Skip libraries that ldd already resolved inside LIB_DIR
          if [[ -f "$lib" && "$(dirname "$(realpath "$lib")")" != "$LIB_DIR" ]]; then
              cp --verbose --preserve=links "$lib" "$LIB_DIR/"
          fi
      done

echo "All dependencies copied to $LIB_DIR"
