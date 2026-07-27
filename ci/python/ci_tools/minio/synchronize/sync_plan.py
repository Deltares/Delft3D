import abc
import functools
from dataclasses import dataclass
from enum import StrEnum
from typing import Callable, Iterable, Iterator, Protocol, Sequence, TypeVar

from ci_tools.minio import DEFAULT_MULTIPART_UPLOAD_PART_SIZE
from ci_tools.minio.list_item import ListItem


class Mode(StrEnum):
    """Synchronization modes.

    Limits what kind of operations are allowed during synchronization.
    - `create-only`: Only create new objects at the destination.
    - `update-only`: Only update existing objects at the destination.
    - `no-delete`: Create and download at the destination. Do not delete any existing objects.
    - `delete`: Create, download and delete objects at the destination.

    The `no-delete` mode is the default mode. The `delete` mode provides the most
    accurate synchronization, but it is also the most destructive. Use with caution.
    """

    CREATE_ONLY = "create-only"
    UPDATE_ONLY = "update-only"
    NO_DELETE = "no-delete"
    DELETE = "delete"


class Synchronizer(abc.ABC):
    """Abstract base class for synchronizers."""

    @abc.abstractmethod
    def synchronize(self) -> None:
        """Perform the synchronization."""


@dataclass
class Changes:
    """The result of comparing the source and destination locations.

    If the source and destination are already identical, no changes need
    to be made. Otherwise, there are three types of changes that can be made
    to make the source and destination identical:
    - `creations`: Objects that exist at the source but not at the destination.
    - `updates`: Objects that exist at both locations but differ in content.
    - `deletions`: Objects that exist at the destination but not at the source.

    To compare the content of files that exist at both the source and destination,
    we only need to compare the `ETag` of the objects, which is a hash of the
    content. MinIO stores the `Etag` alongside the objects and returns them in the
    `list_objects` API call. For local files, the `Etag` needs to be calculated by
    reading the entire contents of the file and calculating a hash.
    """

    creations: Sequence[ListItem]
    updates: Sequence[ListItem]
    deletions: Sequence[ListItem]

    @staticmethod
    def from_listings(source_items: Iterable[ListItem], destination_items: Iterable[ListItem]) -> "Changes":
        """Compute the changes between two object listings.

        Parameters
        ----------
        src_objects, dst_objects : Iterable[ListItem]
            The object listings to compare. The objects must be sorted in
            ascending order by their relative path.

        Returns
        -------
        Changes
        """
        creations: list[ListItem] = []
        updates: list[ListItem] = []
        deletions: list[ListItem] = []

        source_items = iter(source_items)
        destination_items = iter(destination_items)

        src_item: ListItem | None = next(source_items, None)
        dst_item: ListItem | None = next(destination_items, None)
        while src_item and dst_item:
            if src_item.relative_path < dst_item.relative_path:
                # Object exists at source but not at destination. Create it.
                creations.append(src_item)
                src_item = next(source_items, None)
            elif src_item.relative_path > dst_item.relative_path:
                # Object exists at destination but not at source. Delete it.
                deletions.append(dst_item)
                dst_item = next(destination_items, None)
            elif src_item.size != dst_item.size or src_item.etag != dst_item.etag:
                # Object exists in both locations but the contents differ. Update it.
                updates.append(src_item)
                dst_item = next(destination_items, None)
                src_item = next(source_items, None)
            else:
                # Object exists in both locations and are identical.
                dst_item = next(destination_items, None)
                src_item = next(source_items, None)

        if src_item:
            # Create objects that exist at source but not at destination.
            creations.append(src_item)
            for src_item in source_items:
                creations.append(src_item)

        if dst_item:
            # Delete objects that exist at destination but not at source.
            deletions.append(dst_item)
            for dst_item in destination_items:
                deletions.append(dst_item)

        return Changes(creations=creations, updates=updates, deletions=deletions)


@dataclass
class SyncPlan:
    """The plan for synchronizing two locations.

    The plan contains the `changes` that need to be made to make the source and
    destination identical. And also the `mode`, which limits what kind of
    operations are allowed during synchronization. The `part_size` is the
    size of the parts to use when uploading large objects.
    """

    mode: Mode
    changes: Changes
    part_size: int = DEFAULT_MULTIPART_UPLOAD_PART_SIZE

    @functools.cached_property
    def copies(self) -> Sequence[ListItem]:
        """Get the list of objects to copy.

        When synchronizing from local to remote, these are the files to upload.
        When synchronizing from remote to local, these are the objects to download.

        Returns
        -------
        Sequence[ListItem]
        """
        match self.mode:
            case Mode.CREATE_ONLY:
                return self.changes.creations
            case Mode.UPDATE_ONLY:
                return self.changes.updates
            case Mode.DELETE | Mode.NO_DELETE:
                return list(
                    merge(
                        self.changes.creations,
                        self.changes.updates,
                        key=lambda obj: obj.relative_path,
                    )
                )
            case _:
                return []

    @functools.cached_property
    def deletions(self) -> Sequence[ListItem]:
        """Get the list of files or objects to delete at the destination.

        When synchronizing from local to remote, these are the objects to delete.
        When synchronizing from remote to local, these are the files to delete.
        """
        if self.mode == Mode.DELETE:
            return self.changes.deletions
        return []


T = TypeVar("T", contravariant=True)


class Ordered(Protocol[T]):
    """Generic protocol for ordered types."""

    def __lt__(self, other: T) -> bool:
        """Less than operator."""


Ordered_T = TypeVar("Ordered_T", bound=Ordered)


def _identity(x: Ordered_T) -> Ordered_T:
    """Identity function for use as a default key function."""
    return x


def merge(
    left_iter: Iterable[T],
    right_iter: Iterable[T],
    key: Callable[[T], Ordered_T] = _identity,  # type: ignore[assignment]
) -> Iterator[T]:
    """Merge two sorted iterables into a single sorted iterator.

    This function takes two sorted iterables and a key function, and yields
    elements from both iterators in sorted order. The key function is used
    to compare the elements of the iterators.

    Parameters
    ----------
    left_iter, right_iter : Iterator[T]
        The iterables, both sorted in ascending order.
    key : Callable[[T], Ordered_T], optional
        A function that takes an element and returns a value for comparison.
        The default is the identity function, which returns the element itself.
        Note that in this case, the elements themselves must implement `__lt__`.

    Yields
    ------
    T
        The elements from both iterators, sorted in ascending order.
    """
    left_iter = iter(left_iter)
    right_iter = iter(right_iter)

    left = next(left_iter, None)
    right = next(right_iter, None)
    while left and right:
        if key(left) < key(right):
            yield left
            left = next(left_iter, None)
        elif key(right) < key(left):
            yield right
            right = next(right_iter, None)
        else:
            yield left
            yield right
            left = next(left_iter, None)
            right = next(right_iter, None)

    if left is not None:
        yield left
        yield from left_iter

    if right is not None:
        yield right
        yield from right_iter
