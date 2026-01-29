from operator import itemgetter
from typing import Iterable

import pytest

from ci_tools.minio.list_item import ListItem
from ci_tools.minio.synchronize.sync_plan import Changes, Mode, SyncPlan, merge
from tests.helpers import minio as helper


class TestChanges:
    def test_from_object_listings__only_creations(self) -> None:
        # Arrange
        src_objects = [helper.make_list_item(path) for path in ("bar", "baz", "foo")]
        dst_objects: Iterable[ListItem] = []

        # Act
        changes = Changes.from_listings(src_objects, dst_objects)

        # Assert
        assert [obj.relative_path for obj in changes.creations] == ["bar", "baz", "foo"]
        assert not changes.updates
        assert not changes.deletions

    def test_from_object_listings__only_deletions(self) -> None:
        # Arrange
        src_objects: Iterable[ListItem] = []
        dst_objects = [helper.make_list_item(path) for path in ("bar", "baz", "foo")]

        # Act
        changes = Changes.from_listings(src_objects, dst_objects)

        # Assert
        assert not changes.creations
        assert not changes.updates
        assert [obj.relative_path for obj in changes.deletions] == ["bar", "baz", "foo"]

    def test_from_object_listings__src_and_dst_in_sync__no_changes(self) -> None:
        # Arrange
        src_objects = [helper.make_list_item(path) for path in ("bar", "baz", "foo")]
        dst_objects = [helper.make_list_item(path) for path in ("bar", "baz", "foo")]

        # Act
        changes = Changes.from_listings(src_objects, dst_objects)

        # Assert
        assert changes == Changes(creations=[], updates=[], deletions=[])

    def test_from_object_listings__one_file_updated(self) -> None:
        # Arrange
        common_objects = [helper.make_list_item(rel_path) for rel_path in ("bar", "baz")]
        src_objects = common_objects + [helper.make_list_item("foo", size=42, etag="badc0de")]
        dst_objects = common_objects + [helper.make_list_item("foo", size=41, etag="deadbeef")]

        # Act
        changes = Changes.from_listings(src_objects, dst_objects)
        update, *other_updates = changes.updates

        # Assert
        assert not changes.creations
        assert not other_updates
        assert not changes.deletions

        # Ensure the 'source object' is placed in the updates.
        assert update.relative_path == "foo"
        assert update.size == 42
        assert update.etag == "badc0de"

    def test_from_object_listings__interleaved_creations_and_deletions(self) -> None:
        # Arrange
        object_paths = ["bar", "baz", "corge", "foo", "garply", "grault", "quux", "qux"]
        src_objects = [helper.make_list_item(rel_path) for rel_path in object_paths[0::2]]
        dst_objects = [helper.make_list_item(rel_path) for rel_path in object_paths[1::2]]

        # Act
        changes = Changes.from_listings(src_objects, dst_objects)

        # Assert
        assert [obj.relative_path for obj in changes.creations] == object_paths[0::2]
        assert [obj.relative_path for obj in changes.deletions] == object_paths[1::2]
        assert not changes.updates


class TestSyncPlan:
    @pytest.mark.parametrize(
        ("mode", "expected_relative_paths"),
        [
            pytest.param(Mode.CREATE_ONLY, ["baz", "foo"], id="create_only"),
            pytest.param(Mode.UPDATE_ONLY, ["bar", "quux"], id="update_only"),
            pytest.param(Mode.NO_DELETE, ["bar", "baz", "foo", "quux"], id="no_delete"),
            pytest.param(Mode.DELETE, ["bar", "baz", "foo", "quux"], id="delete"),
        ],
    )
    def test_copies(self, mode: Mode, expected_relative_paths: list[str]) -> None:
        # Arrange
        changes = Changes(
            creations=[helper.make_list_item(path) for path in ("baz", "foo")],
            updates=[helper.make_list_item(path) for path in ("bar", "quux")],
            deletions=[helper.make_list_item("qux")],
        )
        plan = SyncPlan(mode=mode, changes=changes)

        # Act, Assert
        assert [obj.relative_path for obj in plan.copies] == expected_relative_paths

    @pytest.mark.parametrize(
        ("mode", "expected_relative_paths"),
        [
            pytest.param(Mode.CREATE_ONLY, [], id="create_only"),
            pytest.param(Mode.UPDATE_ONLY, [], id="update_only"),
            pytest.param(Mode.NO_DELETE, [], id="no_delete"),
            pytest.param(Mode.DELETE, ["qux"], id="delete"),
        ],
    )
    def test_deletions(self, mode: Mode, expected_relative_paths: list[str]) -> None:
        # Arrange
        changes = Changes(
            creations=[helper.make_list_item(path) for path in ("baz", "foo")],
            updates=[helper.make_list_item(path) for path in ("bar", "quux")],
            deletions=[helper.make_list_item("qux")],
        )
        plan = SyncPlan(mode=mode, changes=changes)

        # Act, Assert
        assert [obj.relative_path for obj in plan.deletions] == expected_relative_paths


@pytest.mark.parametrize(
    ("left", "right", "expected"),
    [
        pytest.param("abc", "def", "abcdef", id="left-right"),
        pytest.param("def", "abc", "abcdef", id="right-left"),
        pytest.param("ace", "bdf", "abcdef", id="interleaved"),
        pytest.param("", "abc", "abc", id="left-empty"),
        pytest.param("abc", "", "abc", id="right-empty"),
        pytest.param("", "", "", id="both-empty"),
    ],
)
def test_merge__with_strings(left: str, right: str, expected: str) -> None:
    assert "".join(merge(left, right)) == expected


def test_merge__with_tuples_and_key() -> None:
    left = enumerate(range(0, 10, 2))
    right = enumerate(range(1, 10, 2))

    merged = merge(left, right, key=itemgetter(1))

    assert list(merged) == [
        (0, 0),
        (0, 1),
        (1, 2),
        (1, 3),
        (2, 4),
        (2, 5),
        (3, 6),
        (3, 7),
        (4, 8),
        (4, 9),
    ]
