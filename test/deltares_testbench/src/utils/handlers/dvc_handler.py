"""Executes DVC commands.

Copyright (C)  Stichting Deltares, 2025
"""

import os
from typing import Optional

from dvc.dvcfile import load_file
from dvc.repo import Repo
from dvc.scm import NoSCM

from src.config.credentials import Credentials
from src.utils.handlers.i_handler import IHandler
from src.utils.logging.i_logger import ILogger


class DvcHandler(IHandler):
    """DVC wrapper, has handler interface."""

    def __init__(self, repo: Repo | None = None) -> None:
        if repo is not None:
            self.__repo = repo
        else:
            current_file_path = os.getcwd()
            repo_root = self.__find_dvc_root(current_file_path)
            self.__repo = Repo(repo_root, scm=NoSCM())

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
        self.__download_with_dvc_pull(from_path, credentials, logger)

    def download_batch(
        self, dvc_files: list[str], credentials: Credentials, logger: ILogger, jobs: Optional[int] = None
    ) -> None:
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

        _aws_keys = ("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY")
        _prev_env = {k: os.environ.get(k) for k in _aws_keys}
        if credentials and credentials.username:
            os.environ["AWS_ACCESS_KEY_ID"] = credentials.username
            os.environ["AWS_SECRET_ACCESS_KEY"] = credentials.password

        try:
            # Collect all stage targets from all .dvc files
            all_targets: list[str] = []
            for dvc_file in dvc_files:
                logger.debug(f"Loading DVC file: {dvc_file}")
                if not os.path.isfile(dvc_file):
                    raise FileNotFoundError(f"DVC file not found: {dvc_file}")
                dvcfile = load_file(self.__repo, dvc_file)
                for stage in dvcfile.stages.values():
                    all_targets.append(stage.addressing)

            logger.info(f"Fetching {len(all_targets)} DVC targets in one batch (jobs={jobs})")
            self.__repo.fetch(targets=all_targets, jobs=jobs)

            logger.info(f"Checking out {len(all_targets)} DVC targets in one batch")
            self.__repo.checkout(targets=all_targets, force=True)

            logger.info(f"Batch DVC download complete for {len(dvc_files)} .dvc files")

        except FileNotFoundError as e:
            logger.error(f"File not found: {str(e)}")
            raise
        except Exception as e:
            logger.error(f"Error during DVC batch pull: {str(e)}")
            raise
        finally:
            for key, val in _prev_env.items():
                if val is None:
                    os.environ.pop(key, None)
                else:
                    os.environ[key] = val

    def __download_with_dvc_pull(self, dvc_file: str, credentials: Credentials, logger: ILogger) -> None:
        """Download using DVC by reading the .dvc file and fetching from remote.

        Parameters
        ----------
        dvc_file : str
            Path to the .dvc file (e.g., "data/cases/e02_f002_c100.dvc").
        credentials : Credentials
            Credentials whose username maps to the S3 access key ID and
            password maps to the S3 secret access key.
        logger : ILogger
            Logger instance.
        """
        # Temporarily inject S3/MinIO credentials as environment variables so
        # DVC can authenticate without touching the on-disk config.
        _aws_keys = ("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY")
        _prev_env = {k: os.environ.get(k) for k in _aws_keys}
        if credentials and credentials.username:
            os.environ["AWS_ACCESS_KEY_ID"] = credentials.username
            os.environ["AWS_SECRET_ACCESS_KEY"] = credentials.password

        try:
            logger.debug(f"Downloading DVC directory with file: {dvc_file}")

            # Check if .dvc file exists
            if not os.path.isfile(dvc_file):
                raise FileNotFoundError(f"DVC file not found: {dvc_file}")

            dvcfile = load_file(self.__repo, dvc_file)

            # Fetch and checkout the data
            for stage in dvcfile.stages.values():
                self.__repo.fetch(targets=[stage.addressing])
            for stage in dvcfile.stages.values():
                self.__repo.checkout(targets=[stage.addressing], force=True)

            logger.info(f"Downloading DVC directory complete: {dvc_file}")

        except FileNotFoundError as e:
            logger.error(f"File not found: {str(e)}")
            raise
        except Exception as e:
            logger.error(f"Error during DVC pull: {str(e)}")
            raise
        finally:
            # Restore original environment to avoid leaking credentials.
            for key, val in _prev_env.items():
                if val is None:
                    os.environ.pop(key, None)
                else:
                    os.environ[key] = val

    def __find_dvc_root(self, path: str) -> str:
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
        while current != "/":
            if os.path.isdir(os.path.join(current, ".dvc")):
                # Resolve to true filesystem casing so DVC/scmrepo path
                # comparisons work on case-insensitive (Windows) file systems.
                return os.path.realpath(current)
            current = os.path.dirname(current)
        raise ValueError("Could not find DVC repository root (.dvc directory)")
