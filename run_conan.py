from __future__ import annotations

import argparse
import enum
import json
import platform
import subprocess
import sys
from collections.abc import Generator
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


class BuildPolicy(enum.Enum):
    NONE = "none"  # Build no packages from source, only use pre-built binaries from remotes.
    MISSING = "missing"  # Build packages from source if a pre-built binary is not available from remotes.
    ALL = "all"  # Build all packages from source using local recipes only, do not use any pre-built binaries from remotes.


def clean_conan_cache() -> None:
    subprocess.run(["conan", "remove", "*", "--confirm"], check=True)
    subprocess.run(["conan", "cache", "clean"], check=True)


def update_lockfile(profile: str) -> None:
    """Generate or update conan.lock from the current conanfile and recipes."""
    cmd = [
        "conan",
        "lock",
        "create",
        f"--profile:all={profile}",
        "--settings:all",
        "build_type=Release",
        f"--lockfile-out={LOCKFILE}",
        "--remote=local-recipes",
    ]

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
    build_policy: BuildPolicy = BuildPolicy.NONE,
) -> None:
    cmd = [
        "conan",
        "install",
        f"--profile:all={profile}",
        "--settings:all",
        f"build_type={build_type}",
        f"--output-folder={output_folder}",
    ]

    if build_policy == BuildPolicy.ALL:
        cmd += ["--build=*", "--remote=local-recipes"]
    elif build_policy == BuildPolicy.MISSING:
        cmd += ["--build=missing"]

    if lockfile:
        cmd += [f"--lockfile={lockfile}"]

    if ci:
        cmd += ["--core-conf", "core:non_interactive=True"]

    if consumer_build_type:
        # Odd syntax explained here: https://github.com/conan-io/conan/issues/13478#issuecomment-1475389368
        cmd += ["--settings:all", f"&:build_type={consumer_build_type}"]

    subprocess.run(cmd, check=True)


def _iter_packages(data: dict) -> Generator[tuple[str, str, str], None, None]:
    return (
        (ref, rrev, pkg_id)
        for ref, ref_data in data.items()
        for rrev, rrev_data in ref_data.get("revisions", {}).items()
        for pkg_id in rrev_data.get("packages", {})
    )


def upload_new_packages(remote: str, *, ci: bool = False) -> None:
    """Upload only packages whose recipe_revision + package_id don't exist on the remote yet."""
    local_json = subprocess.run(
        ["conan", "list", "*:*", "--format=json"],
        capture_output=True,
        text=True,
        check=True,
    ).stdout
    remote_json = subprocess.run(
        ["conan", "list", "*:*", f"--remote={remote}", "--format=json"],
        capture_output=True,
        text=True,
        check=True,
    ).stdout

    local_data = json.loads(local_json).get("Local Cache", {})
    remote_data = json.loads(remote_json).get(remote, {})

    remote_packages = set(_iter_packages(remote_data))

    uploaded = 0
    skipped = 0

    for ref, rrev, pkg_id in _iter_packages(local_data):
        if (ref, rrev, pkg_id) in remote_packages:
            print(f"SKIP (already on remote): {ref}#{rrev}:{pkg_id}")
            skipped += 1
        else:
            print(f"UPLOAD: {ref}#{rrev}:{pkg_id}")
            cmd = ["conan", "upload", f"{ref}#{rrev}:{pkg_id}", f"--remote={remote}", "--confirm", "--check"]
            if ci:
                cmd += ["--core-conf", "core:non_interactive=True"]
            subprocess.run(cmd, check=True)
            uploaded += 1

    print(f"\nDone. Uploaded: {uploaded}, skipped: {skipped}")


def _get_profile() -> str:
    os_name = platform.system()
    if os_name == "Windows":
        return "delft3d_windows"
    elif os_name == "Linux":
        return "delft3d_linux"
    else:
        raise RuntimeError(f"Unsupported OS: {os_name}")


def _require_profile(profile: str) -> None:
    result = subprocess.run(
        ["conan", "profile", "path", profile],
        capture_output=True,
    )
    if result.returncode != 0:
        sys.exit(
            f"ERROR: Conan profile '{profile}' not found.\n"
            "       Run 'python run_conan.py initialize deltares' (or 'initialize external') first "
            "to install profiles, configure settings and set up remotes."
        )


def _do_install(
    profile: str,
    output_folder: str,
    build_type: str,
    *,
    ci: bool = False,
    build_policy: BuildPolicy = BuildPolicy.NONE,
) -> None:
    os_name = platform.system()
    if os_name == "Windows":
        # Multi-config generator: generate CMakeDeps for all three configurations.
        # Only the first install builds packages; the other two reuse the cache.
        conan_install(
            profile,
            output_folder,
            "Release",
            ci=ci,
            lockfile=LOCKFILE,
            build_policy=build_policy,
        )
        conan_install(
            profile,
            output_folder,
            "Release",
            consumer_build_type="Debug",
            ci=ci,
            lockfile=LOCKFILE,
        )
        conan_install(
            profile,
            output_folder,
            "Release",
            consumer_build_type="RelWithDebInfo",
            ci=ci,
            lockfile=LOCKFILE,
        )
    else:
        # Single-config generator: one install for the requested build type.
        # Packages are always built as Release; consumer_build_type controls the CMakeDeps output.
        conan_install(
            profile,
            output_folder,
            "Release",
            consumer_build_type=build_type,
            ci=ci,
            lockfile=LOCKFILE,
            build_policy=build_policy,
        )


def cmd_init(args: argparse.Namespace) -> None:
    if args.mode == "deltares":
        setup_conan_config_deltares(ci=args.ci)
    else:
        setup_conan_config_external(ci=args.ci)


def cmd_clean_cache(args: argparse.Namespace) -> None:
    clean_conan_cache()


def cmd_update_lockfile(args: argparse.Namespace) -> None:
    profile = _get_profile()
    _require_profile(profile)
    update_lockfile(profile)


def cmd_install(args: argparse.Namespace) -> None:
    profile = _get_profile()
    _require_profile(profile)

    if args.rebuild_packages:
        build_policy = BuildPolicy.ALL
    elif args.build_missing:
        build_policy = BuildPolicy.MISSING
    else:
        build_policy = BuildPolicy.NONE

    _do_install(
        profile,
        args.output_folder,
        args.build_type,
        ci=args.ci,
        build_policy=build_policy,
    )


def cmd_upload(args: argparse.Namespace) -> None:
    upload_new_packages(args.remote, ci=args.ci)


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Manage Conan dependencies for the Delft3D repository.",
    )
    subparsers = parser.add_subparsers(dest="command")

    # --- initialize ---
    parser_init = subparsers.add_parser(
        "initialize",
        help="One-time Conan setup (profiles, settings, remotes).",
    )
    parser_init.add_argument(
        "mode",
        choices=["deltares", "external"],
        help=(
            "'deltares': installs profiles, settings and Deltares Nexus remotes. "
            "'external': installs profiles and settings only, without Nexus remotes."
        ),
    )
    parser_init.add_argument("--ci", action="store_true", help="Non-interactive mode.")
    parser_init.set_defaults(func=cmd_init)

    # --- clean-cache ---
    parser_clean_cache = subparsers.add_parser(
        "clean-cache",
        help="Clean the local Conan cache.",
    )
    parser_clean_cache.set_defaults(func=cmd_clean_cache)

    # --- update-lockfile ---
    parser_update_lockfile = subparsers.add_parser(
        "update-lockfile",
        help="Regenerate conan.lock from the current conanfile and recipes.",
    )
    parser_update_lockfile.set_defaults(func=cmd_update_lockfile)

    # --- install ---
    parser_install = subparsers.add_parser(
        "install",
        help="Install Conan-managed dependencies.",
    )
    parser_install.add_argument("--ci", action="store_true", help="Non-interactive mode.")
    build_group = parser_install.add_mutually_exclusive_group()
    build_group.add_argument(
        "--build-missing",
        action="store_true",
        help="Build packages from source if a pre-built binary is not available.",
    )
    build_group.add_argument(
        "--rebuild-packages",
        action="store_true",
        help="Rebuild all packages from local recipes only.",
    )
    parser_install.add_argument(
        "--build-type",
        default="Release",
        choices=["Release", "Debug", "RelWithDebInfo"],
        help=(
            "CMake build type for the consumer. "
            "On Linux, determines which CMakeDeps files are generated. "
            "Ignored on Windows (all configurations are always generated)."
        ),
    )
    parser_install.add_argument(
        "--output-folder",
        default="build/conan",
        help="Output folder for Conan install files.",
    )
    parser_install.set_defaults(func=cmd_install)

    # --- upload ---
    parser_upload = subparsers.add_parser(
        "upload",
        help="Upload packages to a remote, skipping those already present (same recipe revision + package id).",
    )
    parser_upload.add_argument(
        "--remote",
        required=True,
        help="Name of the Conan remote to upload to.",
    )
    parser_upload.add_argument("--ci", action="store_true", help="Non-interactive mode.")
    parser_upload.set_defaults(func=cmd_upload)

    args = parser.parse_args()
    if not args.command:
        parser.print_help()
        sys.exit(1)
    args.func(args)


if __name__ == "__main__":
    main()
