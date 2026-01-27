"""Helpers for migrating data to DVC."""

from pathlib import Path
from typing import List

from dvc.repo import Repo


def find_dvc_root_in_parent_directories(start_path: Path) -> Path:
    """Find the DVC repository root by looking for .dvc directory.

    Parameters
    ----------
    start_path : Path
        Starting path to search from.

    Returns
    -------
    Path
        Path to the DVC repository root.
    """
    current = start_path.expanduser().resolve()
    if current.is_file():
        current = current.parent

    for candidate in [current, *current.parents]:
        if (candidate / ".dvc").is_dir():
            return candidate
    raise ValueError("Could not find DVC repository root (.dvc directory)")


def add_directory_to_dvc(path: Path, repo: Repo) -> List[Path]:
    """Add downloaded case and reference data to DVC tracking and return created .dvc files."""
    if not path.exists():
        raise FileNotFoundError(f"Path does not exist: {path}")

    try:
        stages = repo.add(targets=[str(path)])  # type: ignore[call-arg]
    except Exception as e:
        print(f"Error adding to DVC: {e}")
        raise

    dvc_files: List[Path] = []
    for s in stages:
        if hasattr(s, "dvcfile") and hasattr(s.dvcfile, "path"):
            dvc_files.append(Path(s.dvcfile.path))

    return dvc_files


def push_dvc_files_to_remote(repo: Repo, dvc_files: list[Path]) -> None:
    """Push all .dvc files to remote S3 storage."""
    print(f"Pushing {len(dvc_files)} *.dvc files to S3 storage...")

    successful_targets: set[Path] = set()
    failed_targets: set[Path] = set()
    for i, dvc_file in enumerate(dvc_files, 1):
        try:
            print(f"Pushing dvc file {i}/{len(dvc_files)}: {dvc_file}")
            path = str(dvc_file)
            pushed_files = repo.push(targets=[path], remote="storage")

            if pushed_files:
                successful_targets.add(dvc_file)
                print(f"  SUCCESS: {pushed_files} files pushed")
            else:
                successful_targets.add(dvc_file)
                print("  SUCCESS: Already up to date")

        except Exception as push_error:
            failed_targets.add(dvc_file)
            print(f"  FAILED: {str(push_error)}")

    failed_count = len(failed_targets)
    success_count = len(successful_targets)

    print(f"\nPush summary: {success_count} successful, {failed_count} failed")

    if success_count > 0:
        print("Successful targets:")
        for target in sorted(successful_targets, key=str):
            print(f"  - {target}")

    if failed_count > 0:
        print("Failed targets:")
        for target in sorted(failed_targets, key=str):
            print(f"  - {target}")
        print("Warning: some files failed to push. Check errors above.")
