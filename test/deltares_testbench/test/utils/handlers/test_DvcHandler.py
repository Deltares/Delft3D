from src.utils.handlers.dvc_handler import DvcHandler


class TestDvcHandler:
    def test_read_remote_config__standard_remote_section(self, tmp_path) -> None:
        # Arrange
        dvc_dir = tmp_path / ".dvc"
        dvc_dir.mkdir()
        (dvc_dir / "config").write_text(
            "\n".join(
                [
                    "[core]",
                    "    remote = storage",
                    "",
                    "[remote \"storage\"]",
                    "    url = s3://delft3d-testbench",
                    "    endpointurl = https://s3.deltares.nl",
                    "",
                ]
            ),
            encoding="utf-8",
        )

        handler = DvcHandler.__new__(DvcHandler)
        handler.repo_root = str(tmp_path)

        # Act
        bucket, endpoint_url = handler._DvcHandler__read_remote_config()

        # Assert
        assert bucket == "delft3d-testbench"
        assert endpoint_url == "https://s3.deltares.nl"

    def test_read_remote_config__single_quoted_remote_section(self, tmp_path) -> None:
        # Arrange
        dvc_dir = tmp_path / ".dvc"
        dvc_dir.mkdir()
        (dvc_dir / "config").write_text(
            "\n".join(
                [
                    "[core]",
                    "    remote = storage",
                    "",
                    "['remote \"storage\"']",
                    "    url = s3://delft3d-testbench",
                    "    endpointurl = https://s3.deltares.nl",
                    "",
                ]
            ),
            encoding="utf-8",
        )

        handler = DvcHandler.__new__(DvcHandler)
        handler.repo_root = str(tmp_path)

        # Act
        bucket, endpoint_url = handler._DvcHandler__read_remote_config()

        # Assert
        assert bucket == "delft3d-testbench"
        assert endpoint_url == "https://s3.deltares.nl"
