from __future__ import annotations

import argparse
import platform
import subprocess
import sys
from pathlib import Path

CONFIG_DIR = Path("conan/config")
RECIPES_DIR = Path("conan/recipes")
LOCKFILE = Path("conan.lock")


def setup_conan_config(*, ci: bool = False) -> None:
    """Install Conan configuration (profiles, settings, remotes) and register local recipes."""
    cmd = ["conan", "config", "install", "--type", "dir", str(CONFIG_DIR)]
    if ci:
        cmd += ["--core-conf", "core:non_interactive=True"]
    subprocess.run(cmd, check=True)

    # Register the local recipes folder with highest priority so local recipe
    # changes are always preferred over whatever is cached on the remotes.
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


def clean_conan_cache() -> None:
    subprocess.run(["conan", "remove", "*", "--confirm"], check=True)
    subprocess.run(["conan", "cache", "clean"], check=True)


def update_lockfile(profile: str, *, ci: bool = False, local_only: bool = False) -> None:
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
        "--system-setup",
        action="store_true",
        help="Install Conan configuration (profiles, settings, remotes) from conan/config.",
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

    if args.system_setup:
        setup_conan_config(ci=args.ci)
        return

    # Verify the profile is available (installed via --system-setup)
    result = subprocess.run(
        ["conan", "profile", "path", profile],
        capture_output=True,
    )
    if result.returncode != 0:
        sys.exit(
            f"ERROR: Conan profile '{profile}' not found.\n"
            "       Run 'python run_conan.py --system-setup' first to install profiles, set remotes and configure settings."
        )

    if args.clean:
        clean_conan_cache()

    if args.update_lockfile:
        update_lockfile(profile, ci=args.ci, local_only=args.rebuild_recipes)

    # Use lockfile for reproducible installs if one exists
    lockfile = LOCKFILE if LOCKFILE.exists() else None

    # Install dependencies and generate CMakeDeps metadata for Debug, Release and RelWithDebInfo
    # Note that they effectively all use release binaries
    conan_install(profile, args.output_folder, "Release", ci=args.ci, lockfile=lockfile, build_missing=args.build_missing, build_local=args.rebuild_recipes)
    conan_install(
        profile,
        args.output_folder,
        "Release",
        consumer_build_type="Debug",
        ci=args.ci,
        lockfile=lockfile,
        build_missing=args.build_missing,
        build_local=args.rebuild_recipes,
    )
    conan_install(
        profile,
        args.output_folder,
        "Release",
        consumer_build_type="RelWithDebInfo",
        ci=args.ci,
        lockfile=lockfile,
        build_missing=args.build_missing,
        build_local=args.rebuild_recipes,
    )


if __name__ == "__main__":
    main()
