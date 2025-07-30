#!/usr/bin/env bash

_debug() {
    shift  # Skip the "debug" argument
    echo ======================= DEBUG ==========================
    for prog in "$@"; do
        echo
        echo === ${prog} -v
        ${prog} -v
        echo
        echo === ldd ${prog}
        ldd ${prog}
        echo
        echo === ldd lib${prog}.so
        ldd ../lib/lib${prog}.so
        echo
    done
    echo ========================================================
}

# Check for deactivation parameter
if [ "$1" == "debug" ]; then
    _debug "$@"
fi