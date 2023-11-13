import pytest
import os
from unittest.mock import Mock, patch
from src.utils.handlers.handler_factory import HandlerFactory


class TestMinioHandler:
    @staticmethod
    @pytest.mark.parametrize("from_path, expected_bucket_name", [
        ("https://s3.deltares.nl/browser/bucket_name", "bucket_name"),
        ("https://minio.com/browser/bucket_name", "bucket_name"),
        ("https://minio.com/brwsr/bucket_name", ""),
    ])
    def test_download_from_bucket(from_path, expected_bucket_name):
        credentials = Mock()
        logger = Mock()
        programs = []

        # Arrange
        with patch('src.utils.handlers.minio_handler.Minio') as MockMinio, patch('src.utils.handlers.minio_handler.Rewinder') as MockRewinder:
            minio_instance = Mock()
            rewinder_instance = Mock()

            MockMinio.return_value = minio_instance   
            MockRewinder.return_value = rewinder_instance
            minio_instance.list_objects.return_value = [{'key': 'path_to_object'}]

            # Act
            HandlerFactory.download(
                from_path,
                "test/data",
                programs,
                logger,
                credentials,
                "2023.10.20T09:00",
            )

            # Assert
            expected_path = os.path.join('test', 'data')
            rewinder_instance.download.assert_called_once_with(expected_bucket_name, from_path, expected_path)