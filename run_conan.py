from __future__ import annotations

import argparse
from dataclasses import dataclass
import enum
import json
import os
import platform
import shutil
import subprocess
import sys
import tempfile
from collections.abc import Generator
from contextlib import contextmanager
from pathlib import Path
from typing import Any, Iterator

WINDOWS_PROFILE = "delft3d_windows_msvc_194"
LINUX_PROFILE = os.environ.get("DELFT3D_CONAN_PROFILE", "delft3d_alma8_intel_2024")

ROOT = Path(__file__).resolve().parent
CONFIG_DIR = ROOT / "conan/config"
RECIPES_DIR = ROOT / "conan/recipes"
LOCKFILE = ROOT / "conan.lock"


@dataclass(frozen=True)
class PackageInfo:
    name_version: str
    revision_id: str
    package_id: str

    @property
    def identifier(self) -> str:
        return f"{self.name_version}#{self.revision_id}:{self.package_id}"


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


@contextmanager
def _isolated_conan_home() -> Generator[Path, None, None]:
    """Run a block with CONAN_HOME pointed at a fresh, temporary directory.

    Ensures that the lockfile can be deterministically generated and
    that the user's real Conan cache is left untouched.
    """
    tmp = Path(tempfile.mkdtemp(prefix="conan-lockgen-"))
    prev = os.environ.get("CONAN_HOME")
    os.environ["CONAN_HOME"] = str(tmp)
    try:
        yield tmp
    finally:
        if prev is None:
            os.environ.pop("CONAN_HOME", None)
        else:
            os.environ["CONAN_HOME"] = prev
        shutil.rmtree(tmp, ignore_errors=True)


def update_lockfile(profile: str) -> None:
    """Regenerate conan.lock from the current conanfile and local recipes.

    Runs `conan lock create` against an isolated, throw-away `CONAN_HOME` so
    that only the `local-recipes` remote can influence dependency resolution
    (the user's existing cache cannot leak versions or revisions into the
    lockfile via newer timestamps / higher version-range matches).
    """
    if LOCKFILE.exists():
        print(f"Removing existing lockfile {LOCKFILE}...")
        LOCKFILE.unlink()

    with _isolated_conan_home() as home:
        print(f"Using isolated CONAN_HOME={home}")
        setup_conan_config_external()

        cmd = [
            "conan",
            "lock",
            "create",
            ".",
            f"--profile:all={profile}",
            "--settings:all",
            "build_type=Release",
            f"--lockfile-out={LOCKFILE}",
            "--remote=local-recipes",
            "--update",
        ]

        print(f"Generating lockfile {LOCKFILE}...")
        try:
            subprocess.run(cmd, check=True)
        except subprocess.CalledProcessError:
            # Do not leave a partial/inconsistent lockfile behind.
            if LOCKFILE.exists():
                LOCKFILE.unlink()
            raise


def conan_install(
    profile: str,
    output_folder: Path,
    build_type: str,
    *,
    consumer_build_type: str | None = None,
    ci: bool = False,
    build_policy: BuildPolicy = BuildPolicy.NONE,
) -> None:
    cmd = [
        "conan",
        "install",
        f"--profile:all={profile}",
        "--settings:all",
        f"build_type={build_type}",
        f"--output-folder={output_folder}",
        f"--lockfile={LOCKFILE}",
    ]

    if build_policy == BuildPolicy.ALL:
        cmd += ["--build=*", "--remote=local-recipes"]
    elif build_policy == BuildPolicy.MISSING:
        cmd += ["--build=missing"]

    if ci:
        cmd += ["--core-conf", "core:non_interactive=True"]

    if consumer_build_type:
        # Odd syntax explained here: https://github.com/conan-io/conan/issues/13478#issuecomment-1475389368
        cmd += ["--settings:all", f"&:build_type={consumer_build_type}"]

    subprocess.run(cmd, check=True)


def _list_conan_packages(pattern: str, remote: str | None = None) -> dict[str, Any]:
    """Invoke `conan list` with the given pattern and remote."""
    args = ["conan", "list", pattern, "--format=json"]
    if remote is not None:
        args.append(f"--remote={remote}")

    process = subprocess.run(args, capture_output=True, text=True, check=True)
    obj = json.loads(process.stdout)
    repo_key = remote or "Local Cache"
    if not isinstance(obj, dict):
        raise ValueError(f"Failed to list {repo_key}. Response: {obj}")

    repo_object = obj.get(repo_key)
    if not isinstance(repo_object, dict):
        raise ValueError(f"Failed to list {repo_key}. Response: {obj})")

    error = repo_object.get("error")
    if error is not None:
        # If `pattern` matches no packages/revisions, the "error" property is set with this content:
        # - package name/version can't be found: "404: Not Found. [Remote: remote]"
        # - package revision can't be found: "Recipe revision 'name/version#revision_id' not found"
        # - package id can't be found: "Package ID 'name/version#revision_id:package_id' not found"
        if "not found" in str(error).lower():
            # Not an error: We use this to test if a package is present on the remote.
            return {}
        raise ValueError(f"Failed to list {repo_key}. Error message in reponse: {error}")

    return repo_object


def _iter_revisions(name_version: str, revisions_map: dict[str, Any]) -> Iterator[tuple[str, str]]:
    """Walks through `conan list` revision object and yields `(revision_id, package_id)` pairs."""
    for revision_id, revision_obj in revisions_map.items():
        package_map = revision_obj.get("packages")
        if not isinstance(package_map, dict):
            raise ValueError(f".{name_version}.revisions.{revision_id}.packages must be an object. Got: {package_map}")

        for package_id in package_map.keys():
            yield revision_id, package_id


def _iter_packages(repo_object: dict[str, Any]) -> Iterator[PackageInfo]:
    """Walks through `conan list` repo object and yields `PackageInfo`s."""
    for name_version, revisions_data in repo_object.items():
        if not isinstance(revisions_data, dict):
            raise ValueError(f".{name_version} be an object. Got: {revisions_data}")

        revisions_map = revisions_data.get("revisions")
        if not isinstance(revisions_map, dict):
            raise ValueError(f".{name_version}.revisions must be an object. Got: {revisions_map}")

        for revision_id, package_id in _iter_revisions(name_version, revisions_map):
            yield PackageInfo(name_version=name_version, revision_id=revision_id, package_id=package_id)


def _remote_contains_package(remote: str, package_info: PackageInfo) -> bool:
    """Test membership of package in remote Conan repository."""
    repo_object = _list_conan_packages(package_info.identifier, remote)
    return next(_iter_packages(repo_object), None) is not None


def upload_new_packages(remote: str, *, ci: bool = False) -> None:
    """Upload only packages whose recipe_revision + package_id don't exist on the remote yet."""
    local_cache_packages = _list_conan_packages("*:*")

    uploaded = 0
    skipped = 0
    for package_info in _iter_packages(local_cache_packages):
        if _remote_contains_package(remote, package_info):
            print(f"SKIP (already on remote): {package_info.identifier}")
            skipped += 1
        else:
            print(f"UPLOAD: {package_info.identifier}")
            cmd = ["conan", "upload", package_info.identifier, f"--remote={remote}", "--confirm", "--check"]
            if ci:
                cmd += ["--core-conf", "core:non_interactive=True"]
            subprocess.run(cmd, check=True)
            uploaded += 1

    print(f"\nDone. Uploaded: {uploaded}, skipped: {skipped}")


def _get_profile() -> str:
    os_name = platform.system()
    if os_name == "Windows":
        return WINDOWS_PROFILE
    elif os_name == "Linux":
        return LINUX_PROFILE
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
    output_folder: Path,
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
            build_policy=build_policy,
        )
        conan_install(
            profile,
            output_folder,
            "Release",
            consumer_build_type="Debug",
            ci=ci,
        )
        conan_install(
            profile,
            output_folder,
            "Release",
            consumer_build_type="RelWithDebInfo",
            ci=ci,
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
        type=Path,
        required=True,
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
