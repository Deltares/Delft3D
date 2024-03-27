from typing import List
import pytest
import os
from unittest.mock import Mock, patch
from src.utils.handlers.handler_factory import HandlerFactory
from src.suite.program import Program


class TestMinioHandler:
    @pytest.mark.parametrize("from_path, expected_src_path, expected_bucket_name", [
        ("https://s3.deltares.nl/bucket_name/path/to/object", "path/to/object", "bucket_name"),
        ("https://minio.com/bucket_name/path/to/object", "path/to/object", "bucket_name"),
    ])
    def test_download_from_bucket(self, from_path, expected_src_path, expected_bucket_name):
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
            expected_dest_path = os.path.join('test', 'data')
            rewinder_instance.download.assert_called_once_with(
                expected_bucket_name, 
                expected_src_path, 
                expected_dest_path,
            )

    @pytest.mark.parametrize(
        "from_path",
        [
            "https://s3.deltares.nl/bucket_name",  # Path to object missing
            "https://minio.com", # Path to bucket name missing
        ]
    )
    def test_download_from_bucket__invalid_from_path__raise_value_error(self, from_path: str):
        credentials = Mock()
        logger = Mock()
        programs: List[Program] = []

        # Arrange
        with patch('src.utils.handlers.minio_handler.Minio') as MockMinio, patch('src.utils.handlers.minio_handler.Rewinder') as MockRewinder:
            minio_instance = Mock()
            rewinder_instance = Mock()

            MockMinio.return_value = minio_instance   
            MockRewinder.return_value = rewinder_instance
            minio_instance.list_objects.return_value = [{'key': 'path_to_object'}]

            # Act
            with pytest.raises(ValueError):
                HandlerFactory.download(
                    from_path,
                    "test/data",
                    programs,
                    logger,
                    credentials,
                    "2023.10.20T09:00",
                )