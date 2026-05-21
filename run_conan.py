from __future__ import annotations

import argparse
import platform
import subprocess
import sys
from pathlib import Path

CONFIG_DIR = Path("conan/config")
RECIPES_DIR = Path("conan/recipes")
LOCKFILE = Path("conan.lock")


def _conan_config_install(*, ci: bool) -> None:
    cmd = ["conan", "config", "install", "--type", "dir", str(CONFIG_DIR)]
    if ci:
        cmd += ["--core-conf", "core:non_interactive=True"]
    subprocess.run(cmd, check=True)


def _register_local_recipes() -> None:
    """Register the local recipes folder with highest priority."""
    subprocess.run(
        [
            "conan",
            "remote",
            "add",
            "local-recipes",
            str(RECIPES_DIR.parent.resolve()),
            "--type=local-recipes-index",
            "--index=0",
            "--force",
        ],
        check=True,
    )


def setup_conan_config_deltares(*, ci: bool = False) -> None:
    """Install full Conan configuration including Deltares Nexus remotes and register local recipes."""
    _conan_config_install(ci=ci)
    _register_local_recipes()


def setup_conan_config_external(*, ci: bool = False) -> None:
    """Install Conan configuration (profiles, settings) without Nexus remotes.

    Removes all remotes installed by the config (i.e. the Deltares Nexus instances)
    and registers only the local recipes folder.  Use this when Nexus is not accessible.
    """
    _conan_config_install(ci=ci)

    # Remove all remotes that were just installed from remotes.json so that no
    # network access to Nexus is attempted later.
    subprocess.run(["conan", "remote", "remove", "*"], check=True)

    _register_local_recipes()


def clean_conan_cache() -> None:
    subprocess.run(["conan", "remove", "*", "--confirm"], check=True)
    subprocess.run(["conan", "cache", "clean"], check=True)


def update_lockfile(
    profile: str, *, ci: bool = False, local_only: bool = False
) -> None:
    """Generate or update conan.lock from the current conanfile and recipes."""
    cmd = [
        "conan",
        "lock",
        "create",
        f"--profile:all={profile}",
        "--settings:all",
        "build_type=Release",
        f"--lockfile-out={LOCKFILE}",
    ]
    if local_only:
        cmd += ["--remote=local-recipes"]
    if ci:
        cmd += ["--core-conf", "core:non_interactive=True"]
    print(f"Updating lockfile {LOCKFILE}...")
    subprocess.run(cmd, check=True)


def conan_install(
    profile: str,
    output_folder: str,
    build_type: str,
    *,
    consumer_build_type: str | None = None,
    ci: bool = False,
    lockfile: Path | None = None,
    build_missing: bool = False,
    build_local: bool = False,
) -> None:
    cmd = [
        "conan",
        "install",
        f"--profile:all={profile}",
        "--settings:all",
        f"build_type={build_type}",
        f"--output-folder={output_folder}",
    ]

    if build_local:
        cmd += ["--build=*", "--remote=local-recipes"]
    elif build_missing:
        cmd += ["--build=missing"]

    if lockfile:
        cmd += [f"--lockfile={lockfile}"]

    if ci:
        cmd += ["--core-conf", "core:non_interactive=True"]

    if consumer_build_type:
        # Odd syntax explained here: https://github.com/conan-io/conan/issues/13478#issuecomment-1475389368
        cmd += ["--settings:all", f"&:build_type={consumer_build_type}"]

    subprocess.run(cmd, check=True)


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Installs conan-managed dependencies for the Delft3D repository."
    )
    parser.add_argument(
        "--initialize-conan",
        choices=["deltares", "external"],
        metavar="{deltares,external}",
        help=(
            "One-time Conan setup. "
            "'deltares': installs profiles, settings and Deltares Nexus remotes (default for Deltares developers). "
            "'external': installs profiles and settings only, without any Nexus remotes (for open-source developers without Nexus access)."
        ),
    )
    parser.add_argument(
        "--ci",
        action="store_true",
        help="Run in non-interactive mode (adds core:non_interactive=True to Conan commands).",
    )
    parser.add_argument(
        "--clean",
        action="store_true",
        help="Clean the local Conan cache before exporting and installing.",
    )
    parser.add_argument(
        "--update-lockfile",
        action="store_true",
        help="Regenerate conan.lock from the current conanfile and recipes.",
    )
    parser.add_argument(
        "--build-missing",
        action="store_true",
        help="Build packages from source if a pre-built binary is not available.",
    )
    parser.add_argument(
        "--rebuild-recipes",
        action="store_true",
        help="Regenerate the lockfile and rebuild all packages from local recipes only. Use in CI to validate recipes.",
    )
    parser.add_argument(
        "--build-type",
        default="Release",
        choices=["Release", "Debug", "RelWithDebInfo"],
        help=(
            "CMake build type for the consumer. "
            "On Linux (single-config generator), determines which CMakeDeps files are generated. "
            "Ignored on Windows, where all three configurations are always generated."
        ),
    )
    parser.add_argument(
        "--output-folder",
        default="build/conan",
        help="Output folder for Conan install files.",
    )
    args = parser.parse_args()

    os_name = platform.system()
    if os_name == "Windows":
        profile = "delft3d_windows"
    elif os_name == "Linux":
        profile = "delft3d_linux"
    else:
        raise RuntimeError(f"Unsupported OS: {os_name}")

    if args.initialize_conan == "deltares":
        setup_conan_config_deltares(ci=args.ci)
        return
    if args.initialize_conan == "external":
        setup_conan_config_external(ci=args.ci)
        return

    # Verify the profile is available (installed via --initialize-conan)
    result = subprocess.run(
        ["conan", "profile", "path", profile],
        capture_output=True,
    )
    if result.returncode != 0:
        sys.exit(
            f"ERROR: Conan profile '{profile}' not found.\n"
            "       Run 'python run_conan.py --initialize-conan=deltares' (or =external) first "
            "to install profiles, configure settings and set up remotes."
        )

    if args.clean:
        clean_conan_cache()
        return

    if args.update_lockfile:
        update_lockfile(profile, ci=args.ci, local_only=args.rebuild_recipes)

    # Use lockfile for reproducible installs if one exists
    lockfile = LOCKFILE if LOCKFILE.exists() else None

    if os_name == "Windows":
        # Multi-config generator: generate CMakeDeps for all three configurations.
        # Only the first install builds packages; the other two reuse the cache.
        conan_install(
            profile,
            args.output_folder,
            "Release",
            ci=args.ci,
            lockfile=lockfile,
            build_missing=args.build_missing,
            build_local=args.rebuild_recipes,
        )
        conan_install(
            profile,
            args.output_folder,
            "Release",
            consumer_build_type="Debug",
            ci=args.ci,
            lockfile=lockfile,
        )
        conan_install(
            profile,
            args.output_folder,
            "Release",
            consumer_build_type="RelWithDebInfo",
            ci=args.ci,
            lockfile=lockfile,
        )
    else:
        # Single-config generator: one install for the requested build type.
        # Packages are always built as Release; consumer_build_type controls the CMakeDeps output.
        conan_install(
            profile,
            args.output_folder,
            "Release",
            consumer_build_type=args.build_type,
            ci=args.ci,
            lockfile=lockfile,
            build_missing=args.build_missing,
            build_local=args.rebuild_recipes,
        )


if __name__ == "__main__":
    main()
