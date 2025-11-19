"""Executes DVC commands.

Copyright (C)  Stichting Deltares, 2025
"""

import os
from typing import Optional

import yaml
from dvc.dvcfile import load_file
from dvc.repo import Repo

from src.config.credentials import Credentials
from src.utils.handlers.i_handler import IHandler
from src.utils.logging.i_logger import ILogger


class DVCHandler(IHandler):
    """DVC wrapper, has handler interface."""

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
            Depricated: use to_path as the location of the .dvc file.
        credentials : Credentials
            DVC credentials (used for remote storage access).
        version : str
            Not used for DVC, version is already in md5 hash of the .dvc file.
        logger : ILogger
            The logger that logs to a file.
        """
        self._download_with_dvc_pull(from_path, logger)

    def _download_with_dvc_pull(
        self, dvc_file: str, logger: ILogger
    ) -> None:
        """Download using DVC by reading the .dvc file and fetching from remote.

        Parameters
        ----------
        dvc_file : str
            Path to the .dvc file (e.g., "data/cases/e02_f002_c100.dvc").
        logger : ILogger
            Logger instance.
        """
        try:
            logger.debug(f"Downloading DVC directory with file: {dvc_file}")


            # Check if .dvc file exists
            if not os.path.isfile(dvc_file):
                raise FileNotFoundError(f"DVC file not found: {dvc_file}")

            # Open the DVC repository
            repo_root = self._find_dvc_root(dvc_file)
            repo = Repo(repo_root)

            dvcfile = load_file(repo, dvc_file)

            # Fetch and checkout the data
            for stage in dvcfile.stages.values():
                repo.fetch(targets=[stage.addressing])

            for stage in dvcfile.stages.values():
                repo.checkout(targets=[stage.addressing], force=True)

            logger.info(f"Downloading DVC directory complete: {dvc_file}")

        except FileNotFoundError as e:
            logger.error(f"File not found: {str(e)}")
            raise
        except Exception as e:
            logger.error(f"Error during DVC pull: {str(e)}")
            raise

    def _find_dvc_root(self, path: str) -> str:
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
        current = os.path.dirname(os.path.abspath(path))
        while current != '/':
            if os.path.isdir(os.path.join(current, '.dvc')):
                return current
            current = os.path.dirname(current)
        raise ValueError("Could not find DVC repository root (.dvc directory)")

