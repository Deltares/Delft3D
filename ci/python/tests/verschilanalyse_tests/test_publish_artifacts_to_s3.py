"""Tests for publish_artifacts_to_s3.py."""

from argparse import Namespace
from pathlib import Path
from unittest.mock import Mock, call, patch

import pytest

from ci_tools.verschilanalyse.publish_artifacts_to_s3 import (
    build_s3_key,
    create_s3_client,
    publish_artifacts,
    upload_file,
)


class TestCreateS3Client:
    """Tests for create_s3_client."""

    @patch("ci_tools.verschilanalyse.publish_artifacts_to_s3.boto3.client")
    def test_create_client_with_correct_config(self, mock_boto_client: Mock) -> None:
        """Verify the S3 client is created with the correct endpoint, credentials and config."""
        args = Namespace(
            endpoint_url="https://minio.example.com",
            access_key_id="my-access-key",
            secret_access_key="my-secret-key",
        )

        create_s3_client(args)

        _, kwargs = mock_boto_client.call_args
        assert kwargs["endpoint_url"] == "https://minio.example.com"
        assert kwargs["aws_access_key_id"] == "my-access-key"
        assert kwargs["aws_secret_access_key"] == "my-secret-key"
        assert kwargs["region_name"] == "us-east-1"
        assert kwargs["config"].signature_version == "s3v4"


class TestBuildS3Key:
    """Tests for build_s3_key function."""

    def test_with_prefix(self) -> None:
        """Test S3 key with prefix."""
        result = build_s3_key(
            prefix="test",
            project_id="project",
            build_type_id="build",
            build_id="123",
            relative_path=Path("file.zip"),
        )

        assert result == "test/project/build/123/file.zip"

    def test_without_prefix(self) -> None:
        """Test S3 key without prefix."""
        result = build_s3_key(
            prefix="",
            project_id="project",
            build_type_id="build",
            build_id="123",
            relative_path=Path("file.zip"),
        )

        assert result == "project/build/123/file.zip"

    def test_with_nested_relative_path(self) -> None:
        """Test S3 key with a nested relative path."""
        result = build_s3_key(
            prefix="output",
            project_id="project",
            build_type_id="build",
            build_id="123",
            relative_path=Path("subdir/file.txt"),
        )

        assert result == "output/project/build/123/subdir/file.txt"


class TestUploadFile:
    """Tests for upload_file function."""

    def test_calls_upload(self) -> None:
        """Verify upload_file is called with correct arguments."""
        mock_s3 = Mock()
        local_path = Path("/tmp/test.zip")

        upload_file(
            mock_s3,
            local_path,
            "bucket-name",
            "some/key/test.zip",
        )

        mock_s3.upload_file.assert_called_once_with(
            str(local_path),
            "bucket-name",
            "some/key/test.zip",
        )

    def test_raises_on_failed_upload(self) -> None:
        """Verify exception propagates when upload_file fails."""
        mock_s3 = Mock()
        mock_s3.upload_file.side_effect = Exception("connection error")
        local_path = Path("/tmp/test.zip")

        with pytest.raises(Exception, match="connection error"):
            upload_file(
                mock_s3,
                local_path,
                "bucket-name",
                "some/key/test.zip",
            )


class TestPublishArtifacts:
    """Tests for publish_artifacts function."""

    @pytest.fixture
    def mock_s3(self) -> Mock:
        """Return a mock S3 client."""
        return Mock()

    @pytest.fixture
    def checkout_dir(self, tmp_path: Path) -> Path:
        """Return a checkout directory with all required artifacts present."""
        for filename in ["current_logs.zip", "reference_logs.zip", "verschillen.zip"]:
            (tmp_path / filename).write_text("dummy")

        summaries_dir = tmp_path / "summaries"
        summaries_dir.mkdir()
        (summaries_dir / "summary1.txt").write_text("summary")
        (summaries_dir / "summary2.txt").write_text("summary")

        return tmp_path

    def test_uploads_all_artifacts(self, mock_s3: Mock, checkout_dir: Path) -> None:
        """Test that all artifacts and summary files are uploaded."""
        publish_artifacts(
            s3_client=mock_s3,
            bucket="bucket",
            prefix="test",
            project_id="project",
            build_type_id="build-type",
            build_id="123",
            checkout_dir=checkout_dir,
        )

        assert mock_s3.upload_file.call_count == 5

    def test_uploads_in_correct_order(self, mock_s3: Mock, checkout_dir: Path) -> None:
        """Test that artifacts are uploaded to the correct S3 keys."""
        summaries_dir = checkout_dir / "summaries"

        publish_artifacts(
            s3_client=mock_s3,
            bucket="bucket",
            prefix="test",
            project_id="project",
            build_type_id="build-type",
            build_id="123",
            checkout_dir=checkout_dir,
        )

        expected_calls = [
            call(str(checkout_dir / "current_logs.zip"), "bucket", "test/project/build-type/123/current_logs.zip"),
            call(str(checkout_dir / "reference_logs.zip"), "bucket", "test/project/build-type/123/reference_logs.zip"),
            call(str(checkout_dir / "verschillen.zip"), "bucket", "test/project/build-type/123/verschillen.zip"),
            call(str(summaries_dir / "summary1.txt"), "bucket", "test/project/build-type/123/summary1.txt"),
            call(str(summaries_dir / "summary2.txt"), "bucket", "test/project/build-type/123/summary2.txt"),
        ]

        mock_s3.upload_file.assert_has_calls(expected_calls, any_order=False)

    def test_raises_when_zip_file_missing(self, mock_s3: Mock, tmp_path: Path) -> None:
        """Test failure when a required zip file is missing."""
        (tmp_path / "current_logs.zip").write_text("dummy")
        (tmp_path / "reference_logs.zip").write_text("dummy")
        # verschillen.zip intentionally missing

        with pytest.raises(FileNotFoundError, match="verschillen.zip"):
            publish_artifacts(
                s3_client=mock_s3,
                bucket="bucket",
                prefix="",
                project_id="project",
                build_type_id="build-type",
                build_id="123",
                checkout_dir=tmp_path,
            )

    def test_raises_when_summaries_directory_missing(self, mock_s3: Mock, tmp_path: Path) -> None:
        """Test failure when the summaries directory is missing."""
        for filename in ["current_logs.zip", "reference_logs.zip", "verschillen.zip"]:
            (tmp_path / filename).write_text("dummy")
        # summaries/ intentionally missing

        with pytest.raises(FileNotFoundError, match="Required summaries directory not found"):
            publish_artifacts(
                s3_client=mock_s3,
                bucket="bucket",
                prefix="",
                project_id="project",
                build_type_id="build-type",
                build_id="123",
                checkout_dir=tmp_path,
            )

    def test_raises_runtime_error_on_upload_failure(self, mock_s3: Mock, checkout_dir: Path) -> None:
        """Test that a RuntimeError is raised when an upload fails."""
        mock_s3.upload_file.side_effect = Exception("connection error")

        with pytest.raises(RuntimeError, match="Failed to upload"):
            publish_artifacts(
                s3_client=mock_s3,
                bucket="bucket",
                prefix="",
                project_id="project",
                build_type_id="build-type",
                build_id="123",
                checkout_dir=checkout_dir,
            )

    def test_no_uploads_when_summaries_empty(self, mock_s3: Mock, tmp_path: Path) -> None:
        """Test that no summary files are uploaded when summaries directory is empty."""
        for filename in ["current_logs.zip", "reference_logs.zip", "verschillen.zip"]:
            (tmp_path / filename).write_text("dummy")

        (tmp_path / "summaries").mkdir()

        publish_artifacts(
            s3_client=mock_s3,
            bucket="bucket",
            prefix="",
            project_id="project",
            build_type_id="build-type",
            build_id="123",
            checkout_dir=tmp_path,
        )

        assert mock_s3.upload_file.call_count == 3
