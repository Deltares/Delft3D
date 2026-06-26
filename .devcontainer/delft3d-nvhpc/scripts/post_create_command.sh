#!/usr/bin/env bash
# postCreateCommand for the nvfortran (NVHPC) proof-of-concept devcontainer.
# Runs once after a (re-)build, from the repository root, inside the container.
set -exo pipefail

# Create a .vscode/tasks.json only if the user doesn't have one yet.
mkdir -p .vscode

# Quick toolchain sanity check so build issues surface early.
echo "nvfortran: $(command -v nvfortran || echo 'NOT FOUND')"
nvfortran --version || true
echo "mpif90 -> $(mpif90 -show 2>/dev/null || echo 'NOT FOUND')"
