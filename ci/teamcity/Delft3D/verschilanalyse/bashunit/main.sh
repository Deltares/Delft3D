#!/usr/bin/env bash
# shellcheck disable=SC1091
set -euo pipefail

# Source and set path
path=$(dirname "$(realpath "$0")")
source "${path}/config.sh"

# Run bashunit
echo "Running Unit tests..."
bashunit --env "${ROOT_DIR}/.bashunit.env" ./bashunit/tests/**
echo "All tests completed successfully."
