from dataclasses import dataclass
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Iterable, Iterator

from minio import Minio
from minio.datatypes import Object as MinioObject
from s3_path_wrangler.paths import S3Path

from ci_tools.minio import etag


@dataclass
class ListItem:
    """An item returned from a listing operation.

    Can be the result of either a listing of a local directory or
    from the `list_objects` method of the Minio client.
    """

    relative_path: str
    size: int
    timestamp: datetime
    version: str | None
    _etag: etag.ETag

    @property
    def etag(self) -> str:
        """The ETag of the object."""
        return self._etag.get_etag()

    def part_count(self, part_size: int) -> int:
        """Calculate the number of parts needed to store the object in MinIO.

        Parameters
        ----------
        part_size : int
            The size of each part in bytes.

        Returns
        -------
        int
            The number of parts needed to store the object in MinIO.
        """
        return max(1, (self.size + part_size - 1) // part_size)

    @staticmethod
    def from_local_path(path: Path, ancestor: Path) -> "ListItem":
        """Create a `ListItem` from a local file path.

        Parameters
        ----------
        path : Path
            The path to the file. It must exists and cannot be a directory.
        ancestor : Path
            The `ancestor` directory must be an ancestor of the `path`.

        Returns
        -------
        ListItem
        """
        if not path.is_file():
            raise FileNotFoundError(f"{path} is not a file")
        if not ancestor.is_dir():
            raise NotADirectoryError(f"{ancestor} is not a directory")
        if not path.is_relative_to(ancestor):
            raise ValueError(f"{path} is not in directory {ancestor}")
        stat_result = path.stat()

        return ListItem(
            relative_path=path.relative_to(ancestor).as_posix(),  # Ensures forward slash is used as separator.
            size=stat_result.st_size,
            timestamp=datetime.fromtimestamp(stat_result.st_mtime, tz=timezone.utc),
            version=None,
            _etag=etag.ComputedEtag(path),
        )

    @staticmethod
    def from_minio_object(minio_object: MinioObject, remote_prefix: S3Path) -> "ListItem":
        """Create a `ListItem` from a MinIO object.

        Parameters
        ----------
        minio_object : MinioObject
            Must not be a delete marker or a 'directory'.
        remote_prefix : S3Path
            The prefix of the remote object.

        Returns
        -------
        ListItem
        """
        if (
            minio_object.object_name is None
            or minio_object.size is None
            or minio_object.etag is None
            or minio_object.last_modified is None
        ):
            raise ValueError("Object is missing required attributes")
        if minio_object.is_delete_marker:
            raise ValueError("Object is a delete marker")
        if minio_object.is_dir:
            raise ValueError("Object is a directory")
        relative_path = minio_object.object_name.removeprefix(remote_prefix.key.rstrip("/") + "/")
        return ListItem(
            relative_path=relative_path,
            size=minio_object.size,
            timestamp=minio_object.last_modified,
            version=minio_object.version_id,
            _etag=etag.ConstEtag(minio_object.etag),
        )


class MinioPrefixListing(Iterable[ListItem]):
    """List objects in a MinIO bucket with a given prefix.

    The listing is sorted in ascending order by `relative_path`.
    """

    def __init__(self, client: Minio, prefix: S3Path, timestamp: datetime | None = None) -> None:
        self._client = client
        self._prefix = prefix
        if timestamp and timestamp.tzinfo is not timezone.utc:
            raise ValueError("Timestamp must be UTC")
        self._timestamp = timestamp or datetime.now(timezone.utc)

    def __iter__(self) -> Iterator[ListItem]:
        """Iterate over the objects in the bucket with the given prefix.

        Returns
        -------
        Iterator[ListItem]
            The `ListItem`s sorted in ascending order by `relative_path`.
        """
        rewinded_objects = self._unique_object_names(self._list_past_objects_sorted(self._prefix))
        return (
            ListItem.from_minio_object(minio_object=obj, remote_prefix=self._prefix)
            for obj in rewinded_objects
            if not obj.is_delete_marker
        )

    @staticmethod
    def _unique_object_names(objects: Iterator[MinioObject]) -> Iterator[MinioObject]:
        previous = next(objects, None)
        if previous is None:
            return
        yield previous

        for obj in objects:
            if obj.object_name != previous.object_name:
                yield obj
                previous = obj

    def _list_past_objects_sorted(self, prefix: S3Path) -> Iterator[MinioObject]:
        past_objects: Iterator[MinioObject] = filter(
            lambda obj: obj.last_modified is not None and obj.last_modified <= self._timestamp,
            self._client.list_objects(
                bucket_name=prefix.bucket,
                prefix=prefix.key.rstrip("/") + "/",
                recursive=True,
                include_version=True,
            ),
        )

        def name_and_timestamp_key(obj: MinioObject) -> tuple[str, timedelta]:
            key = obj.object_name or ""
            delta = timedelta.max
            if obj.last_modified:
                delta = self._timestamp - obj.last_modified
            return (key, delta)

        return iter(sorted(past_objects, key=name_and_timestamp_key))


class DirectoryListing(Iterable[ListItem]):
    """List objects in a local directory.

    The listing is sorted in ascending order by `relative_path`.
    """

    def __init__(self, local_dir: Path) -> None:
        if not local_dir.is_dir():
            raise NotADirectoryError(f"{local_dir} is not a directory")
        self._local_dir = local_dir

    def __iter__(self) -> Iterator[ListItem]:
        """Iterate over the files in a local directory.

        Returns
        -------
        Iterator[ListItem]
            The `ListItem`s sorted in ascending order by `relative_path`.
        """
        directory = self._local_dir
        summaries = (
            ListItem.from_local_path(path=path, ancestor=directory) for path in directory.rglob("*") if path.is_file()
        )
        sorted_summaries = sorted(summaries, key=lambda obj: obj.relative_path)
        return iter(sorted_summaries)
