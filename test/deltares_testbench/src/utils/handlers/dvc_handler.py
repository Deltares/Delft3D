"""Executes DVC commands.

Copyright (C)  Stichting Deltares, 2025
"""

import os
from typing import Optional

from dvc.repo import Repo
from dvc.scm import NoSCM

from src.config.credentials import Credentials
from src.utils.handlers.i_handler import IHandler
from src.utils.logging.i_logger import ILogger


class DvcHandler(IHandler):
    """DVC wrapper, has handler interface."""

    def __init__(self, credentials: Credentials | None = None, repo: Repo | None = None) -> None:
        self.__repo = repo
        if self.__repo is None:
            current_file_path = os.getcwd()
            repo_root = self.__find_dvc_root(current_file_path)
            self.__repo = Repo(repo_root, scm=NoSCM())

        if credentials is not None:
            os.environ["AWS_ACCESS_KEY_ID"] = credentials.username
            os.environ["AWS_SECRET_ACCESS_KEY"] = credentials.password

    def download(
        self, from_path: str, to_path: str, credentials: Credentials, version: Optional[str], logger: ILogger
    ) -> None:
        """Set up a DVC client connection.

        You can specify the download source and destination.

        Parameters
        ----------
        from_path : str
            dvc file path.
        to_path : str
            Deprecated: use from_path as the location of the .dvc file.
        credentials : Credentials
            DVC credentials (used for remote storage access).
        version : str
            Not used for DVC, version is already in md5 hash of the .dvc file.
        logger : ILogger
            The logger that logs to a file.
        """
        self.__download_with_dvc_pull(from_path, logger)

    def download_batch(self, dvc_files: list[str], logger: ILogger, jobs: Optional[int] = None) -> None:
        """Download multiple .dvc files in a single fetch + checkout operation.

        Parameters
        ----------
        dvc_files : list[str]
            Paths to .dvc files to download.
        credentials : Credentials
            DVC credentials (used for remote storage access).
        logger : ILogger
            Logger instance.
        jobs : Optional[int]
            Number of parallel jobs for DVC fetch. Similar to ``dvc pull -j``.
            When *None*, DVC uses its own default.
        """
        if not dvc_files:
            return

        all_targets = self.__resolve_dvc_targets(dvc_files, logger)
        self.__fetch_and_checkout(all_targets, logger, jobs)
        self.__verify_output_dirs(dvc_files, logger)
        logger.info(f"Batch DVC download complete for {len(dvc_files)} .dvc files")

    def __download_with_dvc_pull(self, dvc_file: str, logger: ILogger) -> None:
        """Download using DVC by reading the .dvc file and fetching from remote.

        Parameters
        ----------
        dvc_file : str
            Path to the .dvc file (e.g., "data/cases/e02_f002_c100.dvc").
        logger : ILogger
            Logger instance.
        """
        targets = self.__resolve_dvc_targets([dvc_file], logger)
        self.__fetch_and_checkout(targets, logger)
        self.__verify_output_dirs([dvc_file], logger)
        logger.info(f"Downloading DVC directory complete: {dvc_file}")

    def __resolve_dvc_targets(self, dvc_files: list[str], logger: ILogger) -> list[str]:
        """Resolve .dvc file paths to absolute paths, validating each exists."""
        targets: list[str] = []
        for dvc_file in dvc_files:
            resolved = os.path.realpath(dvc_file)
            logger.debug(f"Loading DVC file: {resolved}")
            if not os.path.isfile(resolved):
                raise FileNotFoundError(f"DVC file not found: {resolved}")
            targets.append(resolved)
        return targets

    def __fetch_and_checkout(self, targets: list[str], logger: ILogger, jobs: Optional[int] = None) -> None:
        """Run DVC fetch + checkout for the given targets."""
        logger.debug(f"DVC repo root: {self.__repo.root_dir}")
        logger.debug(f"DVC targets: {targets}")

        logger.info(f"Fetching {len(targets)} DVC targets (jobs={jobs})")
        fetch_result = self.__repo.fetch(targets=targets, jobs=jobs)
        logger.debug(f"Fetch result: {fetch_result}")

        logger.info(f"Checking out {len(targets)} DVC targets")
        checkout_result = self.__repo.checkout(targets=targets, force=True)
        logger.debug(f"Checkout result: {checkout_result}")

    @staticmethod
    def __verify_output_dirs(dvc_files: list[str], logger: ILogger) -> None:
        """Verify that DVC checkout produced the expected output directories."""
        missing = []
        for dvc_file in dvc_files:
            expected_dir = os.path.splitext(os.path.realpath(dvc_file))[0]
            if not os.path.isdir(expected_dir):
                missing.append(expected_dir)
        if missing:
            logger.error(f"DVC checkout completed but {len(missing)} expected directories are missing:")
            for m in missing:
                logger.error(f"  - {m}")
            raise FileNotFoundError(
                f"DVC checkout did not produce {len(missing)} expected directories. First missing: {missing[0]}"
            )

    @staticmethod
    def __find_dvc_root(path: str) -> str:
        """Find the DVC repository root by looking for .dvc directory.

        Parameters
        ----------
        path : str
            Starting path to search from.

        Returns
        -------
        str
            Path to the DVC repository root.
        """
        abspath = os.path.abspath(path)
        current = abspath if os.path.isdir(abspath) else os.path.dirname(abspath)
        while True:
            if os.path.isdir(os.path.join(current, ".dvc")):
                return os.path.realpath(current)
            parent = os.path.dirname(current)
            if parent == current:
                break
            current = parent
        raise ValueError("Could not find DVC repository root (.dvc directory)")
