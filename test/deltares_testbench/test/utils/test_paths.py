import pytest

from src.utils.paths import Paths


@pytest.mark.parametrize(
    ("left", "right", "expected"),
    [
        pytest.param(None, "fruit", "fruit", id="none-left"),
        pytest.param("", "fruit", "fruit", id="empty-left"),
        pytest.param("/etc/path", "child", "/etc/path/child", id="linux-path"),
        pytest.param(r"C:\user\documents", "child", r"C:\user\documents\child", id="windows-path"),
    ],
)
def test_merge_path_elements(left: str | None, right: str, expected: str) -> None:
    # Arrange
    paths = Paths()

    # Act
    result = paths.mergePathElements(left, right)

    # Assert
    assert result == expected


@pytest.mark.parametrize(
    ("left", "segments", "expected"),
    [
        pytest.param(None, ("fruit", "apple"), "fruit/apple", id="no-base"),
        pytest.param("", ("fruit", "apple"), "fruit/apple", id="empty-base"),
        pytest.param("/etc", ("sub1", "sub2"), "/etc/sub1/sub2", id="linux-base"),
        pytest.param(r"C:\user", ("documents",), r"C:\user\documents", id="windows-base"),
    ],
)
def test_merge_full_path(left: str | None, segments: tuple[str, ...], expected: str) -> None:
    # Arrange
    paths = Paths()

    # Act
    result = paths.mergeFullPath(left, *segments)

    # Assert
    assert result == expected
