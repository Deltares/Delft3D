import pytest
import requests
from pytest_mock import MockerFixture

from ci_tools.harbor.harbor_version_checker import (
    SemVer,
    fetch_all_tags,
    parse_semver_tags,
)


class TestParseSemverTags:
    @pytest.mark.parametrize(
        ("tags", "expected_valid_semver_count", "expected_first"),
        [
            pytest.param(
                [
                    "2.30.06-development",
                    "development",
                    "2.30.05-development",
                    "2.30.04-development",
                    "2.30.03-development",
                    "2026.01-release",
                    "2.30.02-development",
                    "2.30.01-development",
                    "2.30.00-development",
                    "2.29.28-release",
                    "2.29.27-release",
                    "2.29.26-release",
                    "2.29.25-release",
                    "weekly-2.29.23",
                    "weekly-2.29.22",
                    "weekly-2.29.21",
                    "weekly-2.29.20",
                    "weekly-2.29.19",
                    "weekly-2.29.18",
                    "weekly-2.29.17",
                    "weekly-2.29.16",
                    "weekly-2.29.15",
                    "weekly-2.29.14",
                    "release-2025.02",
                    "alma9-release-2025.01",
                    "alma8-release-2025.01",
                    "release-2025.01",
                    "alma8-release-2024.03",
                    "release-2024.03",
                    "alma9-release-2024.03",
                    "alma8-release-2024.02",
                    "release-2024.02",
                    "alma9-release-2024.02",
                    "centos7-release-2024.01",
                    "centos7",
                ],
                21,
                SemVer(2, 30, 6, "2.30.06-development"),
                id="basic_semver",
            ),
            pytest.param(
                ["2.31.0", "2.30.99", "2.30.5"],
                3,
                SemVer(2, 31, 0, "2.31.0"),
                id="minor_version_takes_precedence",
            ),
            pytest.param(
                [],
                0,
                None,
                id="empty_list",
            ),
        ],
    )
    def test_parse_semver_tags(
        self,
        tags: list[str],
        expected_valid_semver_count: int,
        expected_first: SemVer | None,
    ) -> None:
        result = parse_semver_tags(tags)
        assert len(result) == expected_valid_semver_count
        if expected_first:
            assert result[0] == expected_first

    def test_mixed_labels(self) -> None:
        tags = ["2.30.06-development", "2.30.05-release", "2.30.04-alpha"]
        result = parse_semver_tags(tags)
        assert len(result) == 3
        assert result[0].patch == 6
        assert result[1].patch == 5
        assert result[2].patch == 4

    def test_sorting_comprehensive(self) -> None:
        tags = [
            "2.30.05",
            "3.0.0",
            "2.29.99",
            "2.30.06-development",
            "1.0.0",
        ]
        result = parse_semver_tags(tags)
        assert len(result) == 5
        assert result[0] == SemVer(3, 0, 0, "3.0.0")
        assert result[1] == SemVer(2, 30, 6, "2.30.06-development")
        assert result[2] == SemVer(2, 30, 5, "2.30.05")
        assert result[3] == SemVer(2, 29, 99, "2.29.99")
        assert result[4] == SemVer(1, 0, 0, "1.0.0")

    def test_semver_ignores_non_matching(self) -> None:
        tags = [
            "weekly-foo-bar",
            "2.30.06-development",
            "weekly-2.29.23",
            "alma9-release-2025.01",
            "2.30.05",
        ]
        result = parse_semver_tags(tags)
        assert len(result) == 3
        assert result[0].tag == "2.30.06-development"
        assert result[1].tag == "2.30.05"
        assert result[2].tag == "weekly-2.29.23"


class TestDataClassOrdering:
    """Test cases for dataclass comparison operators."""

    def test_semver_comparison(self) -> None:
        v1 = SemVer(2, 30, 6, "2.30.06")
        v2 = SemVer(2, 30, 5, "2.30.05")
        v3 = SemVer(3, 0, 0, "3.0.0")

        assert v1 > v2  # 2.30.6 > 2.30.5
        assert v3 > v1  # 3.0.0 > 2.30.6
        assert v3 > v2  # 3.0.0 > 2.30.5


class TestFetchAllTags:
    """Test cases for fetch_all_tags function."""

    def test_fetch_all_tags_single_page(self, mocker: MockerFixture) -> None:
        """Test fetching tags when all artifacts fit in a single page."""
        # Arrange
        username = "test_user"
        password = "test_pass"
        mock_response = mocker.Mock(spec=requests.Response)
        mock_response.status_code = 200
        mock_response.json.side_effect = [
            [
                {
                    "tags": [
                        {"name": "2.30.06-development"},
                        {"name": "development"},
                    ]
                },
                {
                    "tags": [
                        {"name": "2026.01-release"},
                    ]
                },
            ],
            [],  # Empty response to end pagination
        ]
        mock_get = mocker.patch("ci_tools.harbor.harbor_version_checker.requests.get", return_value=mock_response)

        # Act
        result = fetch_all_tags(username, password)

        # Assert
        assert len(result) == 3
        assert "2.30.06-development" in result
        assert "development" in result
        assert "2026.01-release" in result
        assert mock_get.call_count == 2

    def test_fetch_all_tags_multiple_pages(self, mocker: MockerFixture) -> None:
        """Test fetching tags across multiple pages."""
        # Arrange
        username = "test_user"
        password = "test_pass"
        mock_response = mocker.Mock(spec=requests.Response)
        mock_response.status_code = 200

        # Simulate pagination
        mock_response.json.side_effect = [
            [{"tags": [{"name": "tag1"}, {"name": "tag2"}]}],  # Page 1
            [{"tags": [{"name": "tag3"}]}],  # Page 2
            [],  # Empty page to end pagination
        ]
        mock_get = mocker.patch("ci_tools.harbor.harbor_version_checker.requests.get", return_value=mock_response)

        # Act
        result = fetch_all_tags(username, password)

        # Assert
        assert len(result) == 3
        assert "tag1" in result
        assert "tag2" in result
        assert "tag3" in result
        assert mock_get.call_count == 3

    def test_fetch_all_tags_with_artifacts_without_tags(self, mocker: MockerFixture) -> None:
        """Test handling artifacts that have no tags."""
        # Arrange
        username = "test_user"
        password = "test_pass"
        mock_response = mocker.Mock(spec=requests.Response)
        mock_response.status_code = 200
        mock_response.json.side_effect = [
            [
                {"tags": [{"name": "tag1"}]},
                {"tags": None},  # Artifact without tags
                {"tags": []},  # Artifact with empty tags
                {"tags": [{"name": "tag2"}]},
            ],
            [],
        ]
        mocker.patch("ci_tools.harbor.harbor_version_checker.requests.get", return_value=mock_response)

        # Act
        result = fetch_all_tags(username, password)

        # Assert
        assert len(result) == 2
        assert "tag1" in result
        assert "tag2" in result

    def test_fetch_all_tags_with_missing_tag_name(self, mocker: MockerFixture) -> None:
        """Test handling tags with missing name field."""
        # Arrange
        username = "test_user"
        password = "test_pass"
        mock_response = mocker.Mock(spec=requests.Response)
        mock_response.status_code = 200
        mock_response.json.side_effect = [
            [
                {
                    "tags": [
                        {"name": "tag1"},
                        {"name": None},  # Tag with None name
                        {},  # Tag without name field
                        {"name": "tag2"},
                    ]
                },
            ],
            [],
        ]
        mocker.patch("ci_tools.harbor.harbor_version_checker.requests.get", return_value=mock_response)

        # Act
        result = fetch_all_tags(username, password)

        # Assert
        assert len(result) == 2
        assert "tag1" in result
        assert "tag2" in result

    def test_fetch_all_tags_api_error(self, mocker: MockerFixture) -> None:
        """Test handling API error response."""
        # Arrange
        username = "test_user"
        password = "test_pass"
        mock_response = mocker.Mock(spec=requests.Response)
        mock_response.status_code = 401
        mock_response.text = "Unauthorized"
        mock_get = mocker.patch("ci_tools.harbor.harbor_version_checker.requests.get", return_value=mock_response)

        # Act & Assert
        with pytest.raises(SystemExit) as exc_info:
            fetch_all_tags(username, password)

        assert exc_info.value.code == 1
        mock_get.assert_called_once()

    def test_fetch_all_tags_uses_correct_auth(self, mocker: MockerFixture) -> None:
        """Test that correct authentication is used."""
        # Arrange
        username = "my_user"
        password = "my_pass"
        mock_response = mocker.Mock(spec=requests.Response)
        mock_response.status_code = 200
        mock_response.json.return_value = []
        mock_get = mocker.patch("ci_tools.harbor.harbor_version_checker.requests.get", return_value=mock_response)

        # Act
        fetch_all_tags(username, password)

        # Assert
        mock_get.assert_called_with(
            "https://containers.deltares.nl/api/v2.0/projects/delft3d/repositories/delft3dfm/artifacts?page=1&page_size=100",
            auth=(username, password),
            timeout=30,
        )

    def test_fetch_all_tags_empty_response(self, mocker: MockerFixture) -> None:
        """Test handling when no artifacts are returned."""
        # Arrange
        username = "test_user"
        password = "test_pass"
        mock_response = mocker.Mock(spec=requests.Response)
        mock_response.status_code = 200
        mock_response.json.return_value = []
        mock_get = mocker.patch("ci_tools.harbor.harbor_version_checker.requests.get", return_value=mock_response)

        # Act
        result = fetch_all_tags(username, password)

        # Assert
        assert len(result) == 0
        mock_get.assert_called_once()
