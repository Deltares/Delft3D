#!/usr/bin/env bash
# This script is used in the "postCreateCommand" of the `devcontainer.json` configuration file.
# It runs exactly once after a (re-)build of the devcontainer. It is run inside the container.
# the working directory is the repository root (/workspaces/delft3d).
# See: https://code.visualstudio.com/docs/devcontainers/create-dev-container#_rebuild
set -eo pipefail

pushd ./test/deltares_testbench
# Create virtual environment for TestBench.py and install the dependencies.
if [[ ! -d ".venv" ]]; then
    uv venv --python=3.12 .venv
    uv pip sync pip/lnx-dev-requirements.txt
fi

# For historical reasons, TestBench.py looks for binaries in the directory: ./data/engines/teamcity_artifacts/lnx64/
# The contents of this directory should be the result of a CMake 'install' of D-Hydro.
# In other words, it should have `bin`, `lib` and `share` subdirectories, with the required scripts and binaries.
# If this directory doesn't already exist, create it and put a symbolic link to the default install location.
mkdir -p data/engines/teamcity_artifacts/
if [[ ! -d "data/engines/teamcity_artifacts/lnx64" ]]; then
    ln -s -T ../../build_fm-suite/install/ data/engines/teamcity_artifacts/lnx64
fi
popd