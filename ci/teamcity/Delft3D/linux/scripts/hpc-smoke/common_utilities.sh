#!/bin/bash

# Common utilities for model processing scripts
# This file contains shared functions used by prepare_all_models.sh and run_all_models.sh

find_dimr_directories() {
    # DVC checkouts live at <engine>/<feature>/<case>/input and a TestBench work
    # copy at <case>/input_work. Prefer the work copies so we do not submit both.
    local dvc_work
    dvc_work=$(find . -type d -name 'input_work' | sort)
    if [ -n "$dvc_work" ]; then
        printf '%s\n' "$dvc_work"
        return
    fi

    find . -type f \( -name "dimr.xml" -o -name "dimr_config.xml" \) -exec dirname {} \; | sort -u
}

# JSON model keys are DVC case folder names (parent of input_work), e.g. c010_weir_timeseries.
smoke_test_model_name() {
    local dir="$1"
    local base
    base=$(basename "$dir")
    if [ "$base" = "input_work" ]; then
        basename "$(dirname "$dir")"
    else
        echo "$base"
    fi
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