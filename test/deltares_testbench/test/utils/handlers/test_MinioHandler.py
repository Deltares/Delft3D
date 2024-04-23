import os
from datetime import datetime, timedelta, timezone

import pytest
from minio import Minio
from pytest_mock import MockerFixture

from src.utils.handlers.handler_factory import HandlerFactory
from src.utils.minio_rewinder import Rewinder
from src.utils.logging.i_logger import ILogger


class TestMinioHandler:
    @pytest.mark.parametrize(
        "from_path, expected_host_name, expected_src_path, expected_bucket_name",
        [
            ("https://s3.deltares.nl/bucket_name/path/to/object", "s3.deltares.nl", "path/to/object", "bucket_name"),
            ("https://minio.com/bucket_name/path/to/object", "minio.com", "path/to/object", "bucket_name"),
        ],
    )
    def test_download_from_bucket(
        self, from_path, expected_host_name, expected_src_path, expected_bucket_name, mocker: MockerFixture
    ):
        # Arrange
        minio_instance = mocker.Mock(spec=Minio)
        rewinder_instance = mocker.Mock(spec=Rewinder)
        minio_patch = mocker.patch(
            "src.utils.handlers.minio_handler.Minio",
            new_callable=lambda: mocker.Mock(return_value=minio_instance),
        )
        rewinder_patch = mocker.patch(
            "src.utils.handlers.minio_handler.Rewinder",
            new_callable=lambda: mocker.Mock(return_value=rewinder_instance),
        )

        minio_instance.list_objects.return_value = [{"key": "path_to_object"}]
        logger = mocker.Mock(spec=ILogger)
        version = "2023.10.20T09:00"

        # Act
        HandlerFactory.download(
            from_path,
            "test/data",
            programs=[],
            logger=logger,
            credentials=mocker.Mock(username="user", password="pass"),
            version=version,
        )

        # Assert
        expected_dest_path = os.path.join("test", "data")
        minio_patch.assert_called_once_with(expected_host_name, access_key="user", secret_key="pass")
        rewinder_patch.assert_called_once_with(minio_instance, logger, version)
        rewinder_instance.download.assert_called_once_with(
            expected_bucket_name,
            expected_src_path,
            expected_dest_path,
        )

    @pytest.mark.parametrize(
        "from_path",
        [
            "https://s3.deltares.nl/bucket_name",  # Path to object missing
            "https://minio.com",  # Path to bucket name missing
        ],
    )
    def test_download_from_bucket__invalid_from_path__raise_value_error(self, from_path: str, mocker: MockerFixture):
        # Arrange
        minio_instance = mocker.Mock(spec=Minio)
        rewinder_instance = mocker.Mock(spec=Rewinder)
        mocker.patch(
            "src.utils.handlers.minio_handler.Minio",
            new_callable=lambda: mocker.Mock(return_value=minio_instance),
        )
        mocker.patch(
            "src.utils.handlers.minio_handler.Rewinder",
            new_callable=lambda: mocker.Mock(return_value=rewinder_instance),
        )

        minio_instance.list_objects.return_value = [{"key": "path_to_object"}]

        # Act
        with pytest.raises(ValueError):
            HandlerFactory.download(
                from_path,
                "test/data",
                programs=[],
                logger=mocker.Mock(),
                credentials=mocker.Mock(),
                version="2023.10.20T09:00",
            )

    def test_download__version_is_none__set_version_to_current_time(self, mocker: MockerFixture) -> None:
        # Arrange
        minio_instance = mocker.Mock(spec=Minio)
        rewinder_instance = mocker.Mock(spec=Rewinder)
        minio_patch = mocker.patch(
            "src.utils.handlers.minio_handler.Minio",
            new_callable=lambda: mocker.Mock(return_value=minio_instance),
        )
        rewinder_patch = mocker.patch(
            "src.utils.handlers.minio_handler.Rewinder",
            new_callable=lambda: mocker.Mock(return_value=rewinder_instance),
        )

        minio_instance.list_objects.return_value = [{"key": "path_to_object"}]
        logger = mocker.Mock(spec=ILogger)

        # Act
        HandlerFactory.download(
            "https://s3.deltares.nl/bucket_name/prefix",
            "test/data",
            version=None,  # Version is not set.
            programs=[],
            credentials=mocker.Mock(username="user", password="pass"),
            logger=logger,
        )

        # Assert
        expected_dest_path = os.path.join("test", "data")
        minio_patch.assert_called_once_with("s3.deltares.nl", access_key="user", secret_key="pass")
        rewinder_patch.assert_called_once_with(minio_instance, logger, mocker.ANY)
        version = rewinder_patch.call_args.args[2]
        timestamp = datetime.fromisoformat(version).replace(tzinfo=timezone.utc)
        assert datetime.now(timezone.utc) - timestamp < timedelta(seconds=1)
        rewinder_instance.download.assert_called_once_with("bucket_name", "prefix", expected_dest_path)
