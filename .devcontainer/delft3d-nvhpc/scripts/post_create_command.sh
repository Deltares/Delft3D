#!/usr/bin/env bash
# postCreateCommand for the nvfortran (NVHPC) proof-of-concept devcontainer.
# Runs once after a (re-)build, from the repository root, inside the container.
set -exo pipefail

VSCODE_EXAMPLE='.devcontainer/delft3d-nvhpc/examples/.vscode-example'

# Create .vscode defaults only if the user doesn't have them yet.
mkdir -p .vscode
[[ ! -e ".vscode/tasks.json" ]] && cp "${VSCODE_EXAMPLE}/tasks.json" .vscode/tasks.json
[[ ! -e ".vscode/settings.json" ]] && cp "${VSCODE_EXAMPLE}/settings.json" .vscode/settings.json
[[ ! -e ".vscode/launch.json" ]] && cp "${VSCODE_EXAMPLE}/launch.json" .vscode/launch.json

# Quick toolchain sanity check so build issues surface early.
echo "nvfortran: $(command -v nvfortran || echo 'NOT FOUND')"
nvfortran --version || true
echo "mpif90 -> $(mpif90 -show 2>/dev/null || echo 'NOT FOUND')"
echo "python3.14: $(command -v python3.14 || echo 'NOT FOUND')"
python3.14 --version || true
echo "conan: $(command -v conan || echo 'NOT FOUND')"
conan --version || true
echo "cmake: $(command -v cmake || echo 'NOT FOUND')"
cmake --version | head -n 1 || true
