#!/bin/bash

BIN_DIR="${1:-bin}"
LIB_DIR="${BIN_DIR}/../lib"

# Find all executables in BIN_DIR
find "$BIN_DIR" -type f -executable | while read -r exe; do
    ldd "$exe" 2>/dev/null | awk '/=>/ {print $3}' | grep -v '^$'
done | sort -u > /tmp/all_libs.txt

# Copy each library to LIB_DIR
while read -r lib; do
    if [ -f "$lib" ]; then
        cp -v --preserve=links "$lib" "$LIB_DIR/"
    fi
done < /tmp/all_libs.txt

echo "All dependencies copied to $LIB_DIR"