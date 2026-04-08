#!/bin/bash

# Common utilities for model processing scripts
# This file contains shared functions used by prepare_all_models.sh and run_all_models.sh

find_dimr_directories() {
    find . -type f \( -name "dimr.xml" -o -name "dimr_config.xml" \) -exec dirname {} \; | sort -u | while read dir; do
        dir_name=$(basename "$dir")
        parent_dir=$(dirname "$dir")
        # Skip non-_work directory when a _work sibling exists (prefer _work for execution)
        if [[ "$dir_name" != *_work ]] && [ -d "$parent_dir/${dir_name}_work" ]; then
            continue
        fi
        echo "$dir"
    done
}

is_supported_platform() {
    local platform="$1"
    case "$platform" in
        "h7"|"delftblue"|"snellius")
            return 0
            ;;
        *)
            return 1
            ;;
    esac
}