"""DVC handler – direct S3 download (zero local cache)."""

import configparser
import json
import os
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path
from typing import List, Optional, Tuple

import boto3
import yaml
from botocore.exceptions import ClientError

from src.config.credentials import Credentials
from src.utils.handlers.i_handler import IHandler
from src.utils.logging.i_logger import ILogger


class DvcHandler(IHandler):
    """Downloads DVC-tracked files/directories directly from S3, skipping cache entirely."""

    def __init__(self) -> None:
        self.repo_root = self.__find_dvc_root(os.getcwd())
        self.__bucket, self.__endpoint_url = self.__read_remote_config()

    def download(
        self,
        from_path: str,
        to_path: str,  # deprecated – kept for interface compatibility
        credentials: Credentials,
        version: Optional[str],
        logger: ILogger,
    ) -> None:
        """Download one .dvc file (public API)."""
        self.__download_direct(from_path, credentials, logger)

    def download_batch(
        self,
        dvc_files: List[str],
        credentials: Credentials,
        logger: ILogger,
        jobs: Optional[int] = None,
    ) -> None:
        """Download multiple .dvc files (public API)."""
        if not dvc_files:
            return

        for dvc_file in dvc_files:
            self.__download_direct(dvc_file, credentials, logger)

    def __download_direct(self, dvc_file: str, credentials: Credentials, logger: ILogger) -> None:
        """Core: parse .dvc → download directly from S3."""
        dvc_path = Path(dvc_file).resolve()
        if not dvc_path.is_file():
            raise FileNotFoundError(f"DVC file not found: {dvc_path}")

        logger.info(f"Direct S3 download started: {dvc_path.name}")

        # 1. Parse .dvc YAML
        with open(dvc_path, encoding="utf-8") as f:
            dvc_data = yaml.safe_load(f)

        outs = dvc_data.get("outs", [])
        if not outs:
            raise ValueError(f"No 'outs' section in {dvc_path}")

        # 2. Create S3 client using config from .dvc/config
        logger.debug(f"Using S3 bucket={self.__bucket}, endpoint={self.__endpoint_url}")
        s3_client = boto3.client(
            "s3",
            aws_access_key_id=credentials.username if credentials else None,
            aws_secret_access_key=credentials.password if credentials else None,
            endpoint_url=self.__endpoint_url,
        )

        # 3. Download every output (usually 1 per .dvc)
        target_base = dvc_path.with_suffix("")  # e.g. input.dvc → input/
        for out in outs:
            md5 = out.get("md5")
            if not md5:
                continue

            rel_path = out.get("path", "")
            hash_algo = out.get("hash", "md5")
            is_dir = out.get("isdir", False) or str(md5).endswith(".dir")

            target_path = target_base / rel_path if rel_path else target_base

            if is_dir:
                self.__download_directory(s3_client, self.__bucket, md5, target_path, logger, hash_algo)
            else:
                self.__download_file(s3_client, self.__bucket, md5, target_path, logger, hash_algo)

        logger.info(f"Direct S3 download complete: {dvc_path.name}")

    def __download_file(
        self,
        s3_client,
        bucket: str,
        md5: str,
        target_path: Path,
        logger: ILogger,
        hash_algo: str = "md5",
    ) -> None:
        """Download a single file using DVC's exact S3 key format."""
        clean_md5 = md5.removesuffix(".dir")
        s3_key = f"files/{hash_algo}/{clean_md5[:2]}/{clean_md5[2:]}"
        logger.debug(f"Downloading s3://{bucket}/{s3_key} → {target_path}")
        target_path.parent.mkdir(parents=True, exist_ok=True)

        try:
            s3_client.download_file(bucket, s3_key, str(target_path))
            logger.debug(f"Downloaded file → {target_path.name}")
        except ClientError as e:
            raise RuntimeError(f"Failed to download s3://{bucket}/{s3_key} (md5={md5})") from e

    def __download_directory(
        self,
        s3_client,
        bucket: str,
        dir_md5: str,
        target_dir: Path,
        logger: ILogger,
        hash_algo: str = "md5",
    ) -> None:
        """Download a full directory by first fetching its .dir metadata JSON."""
        # 1. Get .dir metadata (in-memory) — key uses the full md5 including .dir suffix
        dir_key = f"files/{hash_algo}/{dir_md5[:2]}/{dir_md5[2:]}"
        logger.debug(f"Fetching .dir metadata s3://{bucket}/{dir_key}")
        response = s3_client.get_object(Bucket=bucket, Key=dir_key)
        dir_json = json.loads(response["Body"].read().decode("utf-8"))

        target_dir.mkdir(parents=True, exist_ok=True)

        # 2. Parallel download of all files inside the directory
        max_workers = min(32, len(dir_json) or 1)  # safe concurrency
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            future_to_path = {}
            for entry in dir_json:
                file_md5 = entry["md5"]
                rel_path = entry.get("path") or entry.get("relpath", "")
                file_target = target_dir / rel_path

                future = executor.submit(
                    self.__download_file, s3_client, bucket, file_md5, file_target, logger, hash_algo
                )
                future_to_path[future] = rel_path

            for future in as_completed(future_to_path):
                rel_path = future_to_path[future]
                try:
                    future.result()
                except Exception as exc:
                    logger.error(f"Failed to download {rel_path}: {exc}")
                    raise

        logger.info(f"Directory downloaded ({len(dir_json)} files) → {target_dir}")

    def __read_remote_config(self) -> Tuple[str, str]:
        """Read bucket and endpoint_url from .dvc/config for the default remote."""
        config_path = Path(self.repo_root) / ".dvc" / "config"
        if not config_path.is_file():
            raise FileNotFoundError(f".dvc/config not found at {config_path}")

        config = configparser.ConfigParser()
        config.read(config_path)

        # Determine the default remote name from [core].remote
        remote_name = self.__normalize_section_name(config.get("core", "remote", fallback="storage"))

        section = self.__find_remote_section(config, remote_name)
        if not section:
            raise ValueError(
                f"Remote '{remote_name}' not found in {config_path}. "
                f"Available sections: {config.sections()}"
            )

        url = config.get(section, "url")  # e.g. s3://delft3d-testbench
        endpoint_url = config.get(section, "endpointurl", fallback=None)

        # Extract bucket name from s3:// URL
        if url.startswith("s3://"):
            bucket = url[len("s3://"):].rstrip("/")
        else:
            raise ValueError(f"Unsupported remote URL scheme: {url}")

        return bucket, endpoint_url

    def __find_remote_section(self, config: configparser.ConfigParser, remote_name: str) -> Optional[str]:
        """Resolve a DVC remote section, tolerating accidental extra quoting."""
        expected = f'remote "{remote_name}"'
        if config.has_section(expected):
            return expected

        normalized_expected = self.__normalize_section_name(expected)
        for section in config.sections():
            if self.__normalize_section_name(section) == normalized_expected:
                return section

        return None

    def __normalize_section_name(self, value: str) -> str:
        """Strip accidental wrappers (quotes/brackets) around section names."""
        text = (value or "").strip()
        while len(text) >= 2:
            if text.startswith("[") and text.endswith("]"):
                text = text[1:-1].strip()
                continue
            if (text.startswith("\"") and text.endswith("\"")) or (
                text.startswith("'") and text.endswith("'")
            ):
                text = text[1:-1].strip()
                continue
            break
        return text

    def __find_dvc_root(self, start_path: str) -> str:
        """Same helper you already have – finds the DVC repo root."""
        current = Path(start_path).resolve().parent
        while current != current.parent:
            if (current / ".dvc").is_dir():
                return str(current.resolve())
            current = current.parent
        raise ValueError("Could not find DVC repository root (.dvc directory)")
