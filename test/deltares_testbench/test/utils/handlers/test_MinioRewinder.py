from datetime import datetime, timezone
import os
import pytest

from unittest.mock import ANY, Mock, patch
from src.utils.minio_rewinder import Rewinder


class TestMinioRewinder:
    @staticmethod
    @pytest.mark.parametrize(
        "version, expected_seconds",
        [
            ("2023.10.20T09:12", 0),
            ("2023.10.20T09:12:33", 33),
        ],
    )
    def test_rewind_correct_version_constructs_rewinder(version: str, expected_seconds):
        # Mock Minio and Rewinder
        with patch("src.utils.handlers.minio_handler.Minio") as MockMinio:
            minio_instance = Mock()
            MockMinio.return_value = minio_instance

            rewinder_instance = Rewinder(minio_instance, version)
            assert rewinder_instance.rewind.year == 2023
            assert rewinder_instance.rewind.month == 10
            assert rewinder_instance.rewind.day == 20
            assert rewinder_instance.rewind.hour == 9
            assert rewinder_instance.rewind.minute == 12
            assert rewinder_instance.rewind.second == expected_seconds

    @staticmethod
    @pytest.mark.parametrize(
        "version",
        [
            ("v1.2.3"),
            ("1.2.3"),
            ("abcd.ef.ghTij:kl"),
        ],
    )
    def test_rewind_incorrect_version_throws_value_error(version: str):
        # Mock Minio and Rewinder
        with patch("src.utils.handlers.minio_handler.Minio") as MockMinio:
            minio_instance = Mock()
            MockMinio.return_value = minio_instance

            with pytest.raises(ValueError):
                rewinder_instance = Rewinder(minio_instance, version)

    @staticmethod
    def test_rewind_download_delete_marker():
        # Arrange
        mock_minio_client = Mock()

        object1 = TestMinioRewinder.create_mock_object(
            "object1_name",
            "version1",
            datetime(2023, 10, 20, 10, 0, tzinfo=timezone.utc),
            False,
        )
        object2 = TestMinioRewinder.create_mock_object(
            "object2_name",
            "version1",
            datetime(2023, 10, 20, 10, 0, tzinfo=timezone.utc),
            True,
        )

        # Create a list of mock objects to be returned by the list_objects method
        object_list = [object1, object2]

        mock_minio_client.list_objects.return_value = object_list

        # Mock Minio and Rewinder
        with patch(
            "src.utils.handlers.minio_handler.Minio", return_value=mock_minio_client
        ):
            # Act
            rewinder_instance = Rewinder(mock_minio_client, "2023.10.20T12:00")
            rewinder_instance.download("my_bucket", "https://minio/browser", ".")

            # Assert
            expected_path = os.path.join('.', 'object1_name')
            mock_minio_client.fget_object.assert_called_once_with(
                "my_bucket", "object1_name", expected_path, version_id=ANY
            )

    @staticmethod
    def test_rewind_download_before_rewind():
        # Create a Mock for the Minio client
        mock_minio_client = Mock()

        object1 = TestMinioRewinder.create_mock_object(
            "object1_name",
            "version1",
            datetime(2023, 10, 20, 10, 0, tzinfo=timezone.utc),
            False,
        )
        object2 = TestMinioRewinder.create_mock_object(
            "object2_name",
            "version1",
            datetime(2023, 10, 20, 10, 0, tzinfo=timezone.utc),
            False,
        )

        # Create a list of mock objects to be returned by the list_objects method
        object_list = [object1, object2]

        mock_minio_client.list_objects.return_value = object_list

        # Mock Minio and Rewinder
        with patch(
            "src.utils.handlers.minio_handler.Minio", return_value=mock_minio_client
        ):
            rewinder_instance = Rewinder(mock_minio_client, "2022.10.20T12:00")
            rewinder_instance.download("my_bucket", "https://minio/browser", ".")

            mock_minio_client.fget_object.assert_not_called()

    @staticmethod
    def test_rewind_download_after_rewind():
        # Arrange
        mock_minio_client = Mock()

        object1 = TestMinioRewinder.create_mock_object(
            "object1_name",
            "version1",
            datetime(2023, 10, 20, 10, 0, tzinfo=timezone.utc),
            False,
        )
        object2 = TestMinioRewinder.create_mock_object(
            "object2_name",
            "version1",
            datetime(2023, 10, 20, 10, 0, tzinfo=timezone.utc),
            False,
        )

        # Create a list of mock objects to be returned by the list_objects method
        object_list = [object1, object2]

        mock_minio_client.list_objects.return_value = object_list

        # Act
        with patch(
            "src.utils.handlers.minio_handler.Minio", return_value=mock_minio_client
        ):
            rewinder_instance = Rewinder(mock_minio_client, "2024.10.20T12:00")
            rewinder_instance.download("my_bucket", "https://minio/browser", ".")

            # Assert
            expected_path1 = os.path.join('.', 'object1_name')
            mock_minio_client.fget_object.assert_any_call(
                "my_bucket",
                "object1_name",
                expected_path1,
                version_id=ANY
            )
            expected_path2 = os.path.join('.', 'object2_name')
            mock_minio_client.fget_object.assert_any_call(
                "my_bucket",
                "object2_name",
                expected_path2,
                version_id=ANY
            )

    @staticmethod
    def test_rewind_download_on_rewind():
        # Create a Mock for the Minio client
        mock_minio_client = Mock()

        object1 = TestMinioRewinder.create_mock_object(
            "object1_name",
            "version1",
            datetime(2023, 10, 20, 10, 0, tzinfo=timezone.utc),
            False,
        )
        object2 = TestMinioRewinder.create_mock_object(
            "object2_name",
            "version1",
            datetime(2023, 10, 20, 10, 0, tzinfo=timezone.utc),
            False,
        )

        # Create a list of mock objects to be returned by the list_objects method
        object_list = [object1, object2]

        mock_minio_client.list_objects.return_value = object_list

        # Mock Minio and Rewinder
        with patch(
            "src.utils.handlers.minio_handler.Minio", return_value=mock_minio_client
        ):
            rewinder_instance = Rewinder(mock_minio_client, "2023.10.20T10:00")
            rewinder_instance.download("my_bucket", "https://minio/browser", ".")

            mock_minio_client.fget_object.assert_not_called()

    @staticmethod
    def create_mock_object(
        name: str, version_id: str, last_modified: datetime, is_delete_marker: bool
    ):
        mock_object = Mock()
        mock_object._object_name = name
        mock_object.version_id = version_id
        mock_object._last_modified = last_modified
        mock_object._is_delete_marker = is_delete_marker
        return mock_object
