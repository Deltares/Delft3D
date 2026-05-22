from __future__ import annotations

import argparse
import os
import platform
import shutil
import subprocess
import sys
from pathlib import Path

CONFIGURATIONS = [
    "all",
    "all-testbench",
    "fm-suite",
    "fm-testbench",
    "d3d4-suite",
    "d3d4-testbench",
    "waq-testbench",
    "part-testbench",
    "rr-testbench",
    "wave-testbench",
    "swan-testbench",
    "dflowfm_interacter",
    "dflowfm",
    "dimr",
    "drr",
    "dwaq",
    "dwaves",
    "flow2d3d",
    "swan",
    "fbc",
    "tools",
    "tools_gpl",
]

VS_GENERATORS = {
    "2019": "Visual Studio 16 2019",
    "2022": "Visual Studio 17 2022",
    "2026": "Visual Studio 18 2026",
}

ROOT = Path(__file__).resolve().parent


def build_dir_name(config: str, build_type: str) -> str:
    """Build directory name: on Linux includes build type, on Windows does not (multi-config)."""
    if platform.system() == "Windows":
        return f"build_{config}"
    return f"build_{config}_{build_type.lower()}"


def install_dir_name(config: str, build_type: str) -> str:
    """Install directory name: on Linux includes build type, on Windows does not."""
    if platform.system() == "Windows":
        return f"install_{config}"
    return f"install_{config}_{build_type.lower()}"


def detect_visual_studio() -> str | None:
    """Detect Visual Studio version from a Developer Command Prompt."""
    vs_version = os.environ.get("VisualStudioVersion", "")
    version_map = {"18.0": "2026", "17.0": "2022", "16.0": "2019"}
    return version_map.get(vs_version)


def setup_vs_environment(vs_year: str) -> None:
    """Ensure we are in a Developer Command Prompt with the VS environment set."""
    if os.environ.get("VCINSTALLDIR"):
        return
    sys.exit(
        f"ERROR: Visual Studio {vs_year} environment not active. "
        "Please run this script from a Developer Command Prompt."
    )


def clean_directories(build_dir: Path, install_dir: Path) -> None:
    """Remove build and install directories."""
    for directory in (build_dir, install_dir):
        if directory.exists():
            print(f"Removing {directory}...")
            shutil.rmtree(directory)


def run_conan(
    build_dir: Path, *, ci: bool = False, build_dependencies: bool = False
) -> None:
    """Run run_conan.py to install dependencies."""
    output_folder = build_dir / "conan"
    cmd = [
        sys.executable,
        str(ROOT / "run_conan.py"),
        f"--output-folder={output_folder}",
    ]
    if ci:
        cmd.append("--ci")
    if build_dependencies:
        cmd.append("--build-missing")
    print("Running Conan dependency install...")
    subprocess.run(cmd, check=True)


def run_cmake_configure(
    config: str,
    *,
    vs_year: str | None,
    build_type: str,
    build_dir: Path,
    install_dir: Path,
) -> None:
    """Run CMake configure step."""
    build_dir.mkdir(exist_ok=True)

    cmd = [
        "cmake",
        "-S",
        str(ROOT / "src" / "cmake"),
        "-B",
        str(build_dir),
        f"-DCONFIGURATION_TYPE:STRING={config}",
        f"-DCMAKE_INSTALL_PREFIX={install_dir}",
    ]

    if platform.system() == "Windows":
        cmd += ["-T", "fortran=ifx", "-A", "x64"]
        if vs_year and vs_year in VS_GENERATORS:
            cmd += ["-G", VS_GENERATORS[vs_year]]
    else:
        # On Linux, single-config generator; pass build type directly
        cmd += [f"-DCMAKE_BUILD_TYPE={build_type}"]

    print(f"Running CMake configure for {config}...")
    subprocess.run(cmd, check=True)


def run_cmake_build(config: str, *, build_type: str, build_dir: Path) -> None:
    """Run CMake build step."""
    print(f"Building {config} ({build_type})...")
    subprocess.run(
        ["cmake", "--build", str(build_dir), "--config", build_type, "--parallel"],
        check=True,
    )


def run_cmake_install(config: str, *, build_type: str, build_dir: Path) -> None:
    """Run CMake install step."""
    print(f"Installing {config} ({build_type})...")
    subprocess.run(
        ["cmake", "--install", str(build_dir), "--config", build_type],
        check=True,
    )


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Configure, build, and install Delft3D.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "--config",
        default="fm-suite",
        choices=CONFIGURATIONS,
        help="Configuration type to build (default: fm-suite).",
    )
    parser.add_argument(
        "--build",
        action="store_true",
        help="Run build and install steps after CMake configure.",
    )
    parser.add_argument(
        "--build-type",
        default="Debug",
        choices=["Debug", "Release", "RelWithDebInfo"],
        help="CMake build type (default: Debug).",
    )
    parser.add_argument(
        "--vs",
        choices=list(VS_GENERATORS.keys()),
        help="Visual Studio version to use. Auto-detected if not specified.",
    )
    parser.add_argument(
        "--keep-build",
        action="store_true",
        help="Do not delete existing build and install directories.",
    )
    parser.add_argument(
        "--ci",
        action="store_true",
        help="Run in CI mode (non-interactive Conan, etc.).",
    )
    parser.add_argument(
        "--build-dependencies",
        action="store_true",
        help="Build third-party dependencies from source using local recipes, instead of downloading pre-built binaries. Use this when Nexus access is not available.",
    )
    parser.add_argument(
        "--build-dir",
        help="Override build directory path (default: build_<config> on Windows, build_<config>_<build_type> on Linux).",
    )
    parser.add_argument(
        "--install-dir",
        help="Override install directory path (default: install_<config> on Windows, install_<config>_<build_type> on Linux).",
    )
    args = parser.parse_args()

    # Visual Studio detection (Windows only)
    vs_year = None
    if platform.system() == "Windows":
        vs_year = args.vs or detect_visual_studio()
        if not vs_year:
            print("Warning: Visual Studio not detected. Using CMake default generator.")

    print(f"  config     : {args.config}")
    if platform.system() == "Windows":
        print(f"  vs         : {vs_year or 'default'}")
    print(f"  build_type : {args.build_type}")
    print(f"  build      : {args.build}")
    print(f"  keep_build : {args.keep_build}")
    print()

    # Resolve build and install directories
    build_dir = ROOT / (args.build_dir or build_dir_name(args.config, args.build_type))
    install_dir = ROOT / (
        args.install_dir or install_dir_name(args.config, args.build_type)
    )

    # Verify VS environment is active when building on Windows
    if platform.system() == "Windows" and args.build and vs_year:
        setup_vs_environment(vs_year)

    # Clean build directories
    if not args.keep_build:
        clean_directories(build_dir, install_dir)

    # Conan
    run_conan(build_dir, ci=args.ci, build_dependencies=args.build_dependencies)

    # CMake configure
    run_cmake_configure(
        args.config,
        vs_year=vs_year,
        build_type=args.build_type,
        build_dir=build_dir,
        install_dir=install_dir,
    )

    # Build and install
    if args.build:
        run_cmake_build(args.config, build_type=args.build_type, build_dir=build_dir)
        run_cmake_install(args.config, build_type=args.build_type, build_dir=build_dir)

    print()
    if platform.system() == "Windows":
        sln_ext = "slnx" if vs_year == "2026" else "sln"
        print(f"Generated solution: {build_dir / f'{args.config}.{sln_ext}'}")
    print("Finished")


if __name__ == "__main__":
    main()
