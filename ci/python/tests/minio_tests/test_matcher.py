import pytest

from ci_tools.minio.matcher import GlobMatcher, RegexMatcher
from tests.helpers import minio as helper


class TestGlobMatcher:
    @pytest.mark.parametrize(
        ("relative_path", "pattern"),
        [
            pytest.param("foo/bar/baz.txt", "*.txt", id="extension_glob"),
            pytest.param("foo/bar/baz.txt", "*.t?t", id="question_mark_glob"),
            pytest.param("foo/bar/baz.txt", "**/*", id="recursive_glob"),
            pytest.param("foo/bar/baz.txt", "foo/*/baz.txt", id="middle_star"),
            pytest.param("foo/bar/baz.txt", "foo/bar/*", id="full_path_star"),
            pytest.param("foo/bar/baz.txt", "*/*/baz.txt", id="multistar_glob"),
        ],
    )
    def test_match(self, relative_path: str, pattern: str) -> None:
        list_item = helper.make_list_item(relative_path)
        assert GlobMatcher(pattern).match(list_item)

    @pytest.mark.parametrize(
        ("relative_path", "pattern"),
        [
            pytest.param("foo/bar/baz.txt", "*.bat", id="extension_glob"),
            pytest.param("foo/bar/baz.tt", "*/bar.t?t", id="question_mark_glob"),
            pytest.param("foo/bar/baz.txt", "foo/**", id="recursive_glob"),
            pytest.param("foo/bar/baz.txt", "foo/*.txt", id="single_star_not_recursive"),
        ],
    )
    def test_match__mismatch(self, relative_path: str, pattern: str) -> None:
        list_item = helper.make_list_item(relative_path)
        assert not GlobMatcher(pattern).match(list_item)


class TestRegexMatcher:
    @pytest.mark.parametrize(
        ("relative_path", "pattern", "ignore_case"),
        [
            pytest.param("foo/bar/baz.txt", r"baz[.]txt", False, id="dont_require_match_from_begin"),
            pytest.param("foo/bar/baz.txt", r"o/bar/b", False, id="treat_slashes_like_characters"),
            pytest.param("foo/bar/baz.txt", r"^foo.*[.]txt$", False, id="full_match"),
            pytest.param("foo/bar/baz.txt", r"\D{3}/\w{2,4}/\S{3}[.]txt", False, id="character_classes"),
            pytest.param("foo/BAR/baz.txt", r"bar", True, id="ignore_case"),
        ],
    )
    def test_match(self, relative_path: str, pattern: str, ignore_case: bool) -> None:
        list_item = helper.make_list_item(relative_path)
        assert RegexMatcher(pattern, ignore_case).match(list_item)

    @pytest.mark.parametrize(
        ("relative_path", "pattern", "ignore_case"),
        [
            pytest.param("bbbbbabbbbb", r"^[^a]*$", False, id="inverse_character_class"),
            pytest.param("abcd-efgh", r"^\w*$", False, id="non-alphanumeric"),
            pytest.param("foo/BAR/baz.txt", r"bar", False, id="no_ignore_case"),
        ],
    )
    def test_match__mismatch(self, relative_path: str, pattern: str, ignore_case: bool) -> None:
        list_item = helper.make_list_item(relative_path)
        assert not RegexMatcher(pattern, ignore_case).match(list_item)
