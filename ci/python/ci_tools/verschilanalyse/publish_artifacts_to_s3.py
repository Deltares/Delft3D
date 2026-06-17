import argparse
import logging
from pathlib import Path

import boto3
from botocore.config import Config

from ci_tools.teamcity.log import TeamCityFormatter

logger = logging.getLogger(__name__)


def parse_args() -> argparse.Namespace:
    """Parse and return command-line arguments for the S3 artifact publisher.

    Returns
    -------
    argparse.Namespace
        Parsed arguments containing S3 connection settings, TeamCity build
        identifiers, and the local checkout directory.
    """
    parser = argparse.ArgumentParser(description="Publish build artifacts to an S3-compatible bucket.")
    parser.add_argument("--endpoint-url", required=True, type=str, help="S3 endpoint URL")
    parser.add_argument("--access-key-id", required=True, type=str, help="S3 access key ID")
    parser.add_argument("--secret-access-key", required=True, type=str, help="S3 secret access key")
    parser.add_argument("--bucket", required=True, type=str, help="S3 bucket name")
    parser.add_argument("--prefix", default=Path(""), type=Path, help="Prefix for the S3 object keys")
    parser.add_argument("--project-id", required=True, type=str, help="TeamCity project ID")
    parser.add_argument("--build-type-id", required=True, type=str, help="TeamCity build type ID")
    parser.add_argument("--build-id", required=True, type=str, help="TeamCity build ID")
    parser.add_argument("--checkout-dir", required=True, type=Path, help="Build checkout directory")

    return parser.parse_args()


def build_s3_key(prefix: Path, project_id: str, build_type_id: str, build_id: str, relative_path: Path) -> str:
    """Build the S3 object key from a prefix, TeamCity identifiers, and a relative file path.

    The resulting key has the following path structure:
        <prefix>/<project_id>/<build_type_id>/<build_id>/<relative_path>

    Parameters
    ----------
    prefix : Path
        Optional prefix for the S3 key (e.g. ``output``). Omitted if empty.
    project_id : str
        TeamCity project ID.
    build_type_id : str
        TeamCity build type ID.
    build_id : str
        TeamCity build ID.
    relative_path : Path
        Path of the files relative to the resulting s3 key path.

    Returns
    -------
    str
        The full S3 object key.
    """
    parts = [*([prefix.as_posix()] if prefix.parts else []), project_id, build_type_id, build_id]
    prefix_path = "/".join(parts)
    return f"{prefix_path}/{relative_path.as_posix()}"


def upload_file(s3_client: object, local_path: Path, bucket: str, s3_key: str) -> None:
    """Upload a local file to an S3 bucket.

    Parameters
    ----------
    s3_client : object
        S3 client for the target endpoint.
    local_path : Path
        Absolute path to the local file to upload.
    bucket : str
        Name of the target S3 bucket.
    s3_key : str
        S3 object key under which the file will be stored.

    Raises
    ------
    S3UploadFailedError
        If the upload fails after all retries are exhausted.
    """
    logger.info(f"Uploading {local_path} -> s3://{bucket}/{s3_key}")
    try:
        s3_client.upload_file(str(local_path), bucket, s3_key)  # type: ignore[attr-defined]
    except boto3.exceptions.S3UploadFailedError as e:
        raise RuntimeError(f"Failed to upload {local_path} to s3://{bucket}/{s3_key}: {e}") from e


def publish_artifacts(
    s3_client: object, bucket: str, prefix: Path, project_id: str, build_type_id: str, build_id: str, checkout_dir: Path
) -> None:
    """Publish Verschilanalyse build artifacts to an S3 bucket.

    Uploads the following artifacts from the checkout directory to the following path structure in the S3 bucket:
    ``<prefix>/<project_id>/<build_type_id>/<build_id>/``:

    - ``current_logs.zip``
    - ``reference_logs.zip``
    - ``verschillen.zip``
    - All files inside the ``summaries/`` directory.

    Parameters
    ----------
    s3_client : object
        S3 client for the target endpoint.
    bucket : str
        Name of the target S3 bucket.
    prefix : Path
        Optional prefix for the S3 key (e.g. ``output``). Omitted if empty.
    project_id : str
        TeamCity project ID.
    build_type_id : str
        TeamCity build type ID.
    build_id : str
        TeamCity build ID.
    checkout_dir : Path
        Path of the TeamCity build checkout directory.

    Raises
    ------
    FileNotFoundError
        If any of the required artifact files or the summaries directory are missing.
    RuntimeError
        If any upload operation fails after all retries are exhausted.
    """
    for zip_file in (
        "current_logs.zip",
        "reference_logs.zip",
        "verschillen.zip",
    ):
        local_path = checkout_dir / zip_file
        if not local_path.exists():
            raise FileNotFoundError(f"Required artifact not found: {local_path}")
        upload_file(
            s3_client, local_path, bucket, build_s3_key(prefix, project_id, build_type_id, build_id, Path(zip_file))
        )

    summaries_dir = checkout_dir / "summaries"
    if not summaries_dir.exists():
        raise FileNotFoundError(f"Required summaries directory not found: {summaries_dir}")
    for file in sorted(summaries_dir.rglob("*")):
        if file.is_file():
            upload_file(
                s3_client,
                file,
                bucket,
                build_s3_key(prefix, project_id, build_type_id, build_id, file.relative_to(summaries_dir)),
            )


def main() -> None:
    """Parse arguments, create the S3 client, and publish Verschilanalyse build artifacts."""
    handler = logging.StreamHandler()
    handler.setFormatter(TeamCityFormatter())
    logging.basicConfig(level=logging.INFO, handlers=[handler])

    args = parse_args()

    s3_client = boto3.client(
        "s3",
        endpoint_url=args.endpoint_url,
        aws_access_key_id=args.access_key_id,
        aws_secret_access_key=args.secret_access_key,
        region_name="us-east-1",
        config=Config(signature_version="s3v4"),
    )

    publish_artifacts(
        s3_client=s3_client,
        bucket=args.bucket,
        prefix=args.prefix,
        project_id=args.project_id,
        build_type_id=args.build_type_id,
        build_id=args.build_id,
        checkout_dir=args.checkout_dir,
    )


if __name__ == "__main__":
    main()
