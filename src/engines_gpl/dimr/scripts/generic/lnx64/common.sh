#!/usr/bin/env bash

# Get the directory where the script is located
SCRIPT_DIR=$(dirname "$(realpath "$0")")

_debug() {
    shift  # Skip the "debug" argument
    echo ======================= DEBUG ==========================
    for prog in "$@"; do
        echo
        echo === ${prog} -v
        "${SCRIPT_DIR}/${prog}" -v
        echo
        echo === ldd ${prog}
        ldd "${SCRIPT_DIR}/${prog}"
        echo
        echo === ldd lib${prog}.so
        ldd "${SCRIPT_DIR}/../lib/lib${prog}.so"
        echo
    done
    echo ========================================================
}

# Check for debug parameter
if [ "$1" == "debug" ]; then
    _debug "$@"
fi