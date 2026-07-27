#!/usr/bin/env python3
"""Script to migrate entire directories from MinIO S3 storage to DVC storage."""

import shutil
from pathlib import Path
from typing import List

from dvc.repo import Repo

from tools.minio_dvc_migration.dvc_utils import (
    add_directory_to_dvc,
    find_dvc_root_in_parent_directories,
    push_dvc_files_to_remote,
)
from tools.minio_dvc_migration.s3_client import MinioBucketUtils, setup_minio_rewinder
from tools.minio_dvc_migration.testcase_data import (
    is_case_with_doc_folder,
    move_doc_folder_to_parent,
)

BASE_URL = "https://s3.deltares.nl"
S3_BUCKET = "dsc-testbench"
SUBDIRS_TO_CHECK = ["cases", "references/lnx64", "references/win64"]
MIGRATION_PROGRESS_FILE = "migrated_directories.txt"


def main() -> None:
    """Execute main functionality for the minio to DVC migration tool."""
    print("Fetching S3 directory listing...")
    directories_to_migrate = MinioBucketUtils().fetch_directories_to_migrate(BASE_URL, S3_BUCKET, SUBDIRS_TO_CHECK)
    print(f"Found {len(directories_to_migrate)} directories in S3")

    migrated_directories = set()
    if Path(MIGRATION_PROGRESS_FILE).exists():
        with open(MIGRATION_PROGRESS_FILE, "r") as f:
            for line in f:
                migrated_directories.add(line.strip())

    # filter directories_to_migrate to only include those not in migrated_directories
    directories_to_migrate = [d for d in directories_to_migrate if d.path not in migrated_directories]
    print(f"{len(directories_to_migrate)} directories to migrate after filtering already migrated ones")

    rewinder = setup_minio_rewinder(BASE_URL)

    repo_root = find_dvc_root_in_parent_directories(Path(__file__).resolve())
    print(f"Found existing DVC repo at: {repo_root}")
    repo = Repo(str(repo_root))

    for i, directory in enumerate(directories_to_migrate, start=1):
        print(f"Migrating {i}/{len(directories_to_migrate)}: {directory.path}")
        dvc_paths: List[Path] = []
        local_directory = directory.to_local()

        rewinder.download(directory.bucket, directory.path, local_directory)
        if is_case_with_doc_folder(local_directory):
            doc_folder = move_doc_folder_to_parent(local_directory)
            dvc_paths.extend(add_directory_to_dvc(doc_folder, repo))

        dvc_paths.extend(add_directory_to_dvc(local_directory, repo))
        push_dvc_files_to_remote(repo, dvc_paths)

        for dvc_path in dvc_paths:
            tracked_path = dvc_path.with_suffix("")
            shutil.rmtree(tracked_path)

        with open(MIGRATION_PROGRESS_FILE, "a") as f:
            f.write(f"{directory.path}\n")


if __name__ == "__main__":
    main()
