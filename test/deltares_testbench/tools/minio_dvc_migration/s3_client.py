"""S3 client functionality for testing URL availability and directory listing."""

import configparser
import os
from shutil import Error
from typing import List

from minio import Minio
from minio.error import S3Error

from src.config.credentials import Credentials
from src.utils.logging.log_level import LogLevel
from src.utils.logging.logger import Logger
from src.utils.minio_rewinder import Rewinder
from tools.minio_dvc_migration.s3_url_info import S3UrlInfo


class MinioBucketUtils:
    """Data class to hold parsed S3 URL information."""

    def fetch_directories_to_migrate(self, base_url: str, bucket: str, subdirs: List[str]) -> List[S3UrlInfo]:
        """Fetch functional directory paths from S3 for given subdirectories.

        Args:
            base_url: The base S3 URL
            bucket: The S3 bucket name
            subdirs: List of subdirectories to check within each top-level dir
                (e.g., ['cases', 'ref/lnx64', 'ref/win64'])

        Returns
        -------
            List of functional directory paths
        """
        directories = []
        client = setup_minio_client(base_url)

        hostname = base_url.replace("https://", "")

        for subdir in subdirs:
            engine_directories = self.__fetch_s3_directories_with_minio(client, bucket, subdir)

            try:
                for engine_dir in engine_directories:
                    directories.extend(self.__parse_first_level_folders(hostname, bucket, client, subdir, engine_dir))
            except Exception as e:
                print(f"  Could not fetch subdirectories for {subdir}: {e}")

        return directories

    def __fetch_s3_directories_with_minio(self, client: Minio, bucket_name: str, path: str = "") -> List[str]:
        """Fetch directory listing from S3 bucket using Minio client."""
        directories = []

        try:
            # List objects to find directories (objects ending with /)
            full_prefix = f"{path}/" if path and not path.endswith("/") else path

            objects = client.list_objects(bucket_name=bucket_name, prefix=full_prefix, recursive=False)

            # Collect directory names from object names
            found_dirs = set()
            for obj in objects:
                obj_name = obj.object_name
                if obj_name and obj_name != full_prefix:
                    # Remove the prefix
                    relative_path = obj_name.removeprefix(full_prefix)
                    # Get the first directory component
                    if "/" in relative_path:
                        dir_name = relative_path.split("/")[0]
                        if dir_name:  # Not empty
                            found_dirs.add(dir_name)
                    elif obj_name.endswith("/"):
                        # This is a directory marker
                        dir_name = relative_path.rstrip("/")
                        if dir_name:
                            found_dirs.add(dir_name)

            directories = list(found_dirs)

        except S3Error as e:
            if "AccessDenied" in str(e) or e.code == "AccessDenied":
                print(f"✗ Access denied to bucket '{bucket_name}'")
            elif "NoSuchBucket" in str(e) or e.code == "NoSuchBucket":
                print(f"✗ Bucket '{bucket_name}' does not exist")
            elif "InvalidAccessKeyId" in str(e):
                print("✗ Invalid access key for profile")
            else:
                print(f"✗ S3 error: {e}")

        except Exception as e:
            print(f"✗ Error with: {e}")

        if not directories:
            print(f"  No directories found in {path} or access denied.")

        return sorted(list(set(directories)))

    def __parse_first_level_folders(
        self, hostname: str, bucket: str, client: Minio, subdir: str, engine_dir: str
    ) -> List[S3UrlInfo]:
        """Parse first-level folders to find engine and other directories."""
        engine_dir_url = f"{subdir}/{engine_dir}"
        directories = []

        if engine_dir.startswith("e500_FloodAdapt"):
            directories.append(S3UrlInfo(hostname=hostname, bucket=bucket, path=engine_dir_url))
        elif engine_dir == "doc":
            directories.append(S3UrlInfo(hostname=hostname, bucket=bucket, path=engine_dir_url))
        elif engine_dir.startswith("e"):
            functional_dirs = self.__fetch_s3_directories_with_minio(client, bucket, engine_dir_url)
            for func_dir in functional_dirs:
                directories.extend(
                    self.__parse_second_level_folders(hostname, bucket, client, engine_dir_url, func_dir)
                )
        else:
            print(f"  Skip non-engine directory: {engine_dir_url}")

        return directories

    def __parse_second_level_folders(
        self, hostname: str, bucket: str, client: Minio, engine_dir_url: str, func_dir: str
    ) -> List[S3UrlInfo]:
        """Parse second-level folders to find functional and other directories."""
        functional_path = f"{engine_dir_url}/{func_dir}"
        directories = []

        if func_dir in (
            "doc",
            "TeamCityScripts",
            "matlabTools",
            "scripts",
            "test_models",
            "Dbase_shared",
        ) or func_dir.startswith(("c01", "c02", "c03", "c04", "c05")):
            # This deviates from the usual second level structure but is valid
            directories.append(S3UrlInfo(hostname=hostname, bucket=bucket, path=functional_path))
        elif func_dir.startswith("f") or func_dir.endswith(("waqpb", "1d_non_linear_waves", "2d_backward_facing_step")):
            case_dirs = self.__fetch_s3_directories_with_minio(client, bucket, functional_path)
            for case_dir in case_dirs:
                directories.extend(self.__parse_third_level_folders(hostname, bucket, functional_path, case_dir))
        else:
            print(f"  Skip non-functional directory: {functional_path}")

        return directories

    def __parse_third_level_folders(
        self, hostname: str, bucket: str, functional_path: str, case_dir: str
    ) -> List[S3UrlInfo]:
        """Parse third-level folders to find case directories."""
        directories = []
        case_path = f"{functional_path}/{case_dir}"
        directories.append(S3UrlInfo(hostname=hostname, bucket=bucket, path=case_path))
        return directories


def get_s3_credentials() -> Credentials:
    """Get a configured MinIO handler and credentials from the home directory credential file."""
    config = {"profile": "default"}

    access_key, secret_key = load_aws_credentials(config["profile"])

    credentials = Credentials()
    credentials.username = access_key
    credentials.password = secret_key

    return credentials


def setup_minio_rewinder(base_url: str) -> Rewinder:
    """Set up MinIO rewinder with logging."""
    minio_client = setup_minio_client(base_url=base_url)

    logger = Logger(LogLevel.ERROR, False)
    rewinder = Rewinder(minio_client, logger)
    return rewinder


def setup_minio_client(base_url: str) -> Minio:
    """Set up MinIO client with credentials."""
    credentials = get_s3_credentials()
    if credentials is None:
        raise Error("Warning: Could not get credentials for MinIO testing.")

    client = None
    try:
        client = Minio(
            base_url.replace("https://", ""), access_key=credentials.username, secret_key=credentials.password
        )
    except Exception as e:
        raise Error(f"Warning: Could not create MinIO client: {e}") from e

    return client


def load_aws_credentials(profile_name: str = "default") -> tuple[str, str]:
    """Load AWS/Minio credentials from ~/.aws/credentials file in the home directory."""
    credentials_path = os.path.expanduser("~/.aws/credentials")

    if not os.path.exists(credentials_path):
        raise FileNotFoundError(f"AWS credentials file not found: {credentials_path}")

    config = configparser.ConfigParser()
    config.read(credentials_path)

    if profile_name not in config:
        available_profiles = list(config.sections())
        raise ValueError(f"Profile '{profile_name}' not found. Available profiles: {available_profiles}")

    profile = config[profile_name]
    access_key = profile.get("aws_access_key_id", None)
    secret_key = profile.get("aws_secret_access_key", None)

    if not access_key or not secret_key:
        raise ValueError(f"Missing credentials in profile '{profile_name}'")

    return access_key, secret_key
