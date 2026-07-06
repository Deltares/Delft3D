#!/usr/bin/env bash
# Wrapper for CMake/CTest integration: injects Conan runtime environment before ctest.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Default to the workspace debug build; can be overridden per shell/session.
build_dir="${DELFT3D_CTEST_BUILD_DIR:-${repo_root}/build_dflowfm_debug}"
conan_run_script="${build_dir}/conan/generators/conanrun.sh"

if [[ -f "${conan_run_script}" ]]; then
  # shellcheck disable=SC1090
  source "${conan_run_script}"
fi

exec ctest "$@"
