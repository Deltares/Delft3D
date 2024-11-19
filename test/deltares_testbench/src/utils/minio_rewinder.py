import enum
import functools
import hashlib
import io
import os
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import Iterable, Iterator, List, Mapping, Optional, Tuple

from minio import Minio
from minio.commonconfig import Tags
from minio.datatypes import Object as MinioObject
from s3_path_wrangler.paths import S3Path  # type: ignore

from src.utils.logging.i_logger import ILogger

# Large objects should be uploaded to MinIO with a *multipart upload*. This affects not only the
# performance of uploads, but also the computation of the `ETag` used to check data integrity.
# At this time of writing, it seems the MinIO recommendation for the size of the multipart upload
# parts is 100 MiB. This is the same size as AWS S3 recommends:
# https://min.io/docs/minio/linux/reference/minio-mc/mc-mv.html#mc.mv.-disable-multipart
# https://docs.aws.amazon.com/AmazonS3/latest/userguide/mpuoverview.html
DEFAULT_MULTIPART_UPLOAD_PART_SIZE = 100 * 1024 * 1024  # Bytes


class Operation(str, enum.Enum):
    CREATE = "create"
    UPDATE = "update"
    REMOVE = "remove"
    NONE = "none"


@dataclass
class PlanItem:
    """An operation triggered by differences between a local file and an object in MinIO.

    A `Plan` consists of a sequence of `PlanItems`. `PlanItem`s can include:
    1. Creations: New files have been added, which don't have a corresponding object in MinIO.
    2. Updates: The contents of a file is not the same as the corresponding object in MinIO.
    3. Removals: A file is not present in the local directory, but there is an object in MinIO.
    """

    operation: Operation
    minio_path: S3Path
    local_path: Optional[Path] = field(default=None)

    @staticmethod
    def create(local_path: Path, minio_path: S3Path) -> "PlanItem":
        """Make MinIO `CREATE` operation."""
        return PlanItem(operation=Operation.CREATE, local_path=local_path, minio_path=minio_path)

    @staticmethod
    def remove(minio_path: S3Path) -> "PlanItem":
        """Make MinIO `REMOVE` operation."""
        return PlanItem(operation=Operation.REMOVE, minio_path=minio_path)

    @staticmethod
    def update(local_path: Path, minio_path: S3Path) -> "PlanItem":
        """Make MinIO `UPDATE` operation."""
        return PlanItem(operation=Operation.UPDATE, local_path=local_path, minio_path=minio_path)


@dataclass
class Plan:
    """A 'plan' to synchronize a set of objects in MinIO with local files.

    A plan consists of a sequence of steps (`items`) to synchronize a set
    of objects in minio (prefixed by `minio_prefix`) with a local directory
    with files on this machine (`local_dir`). New and updated files will
    get a list of `tags` assigned to them in MinIO.
    """

    local_dir: Path
    minio_prefix: S3Path
    items: List[PlanItem] = field(default_factory=list)
    tags: Optional[Tags] = None
    allow_create_and_delete: bool = False
    multipart_upload_part_size: int = DEFAULT_MULTIPART_UPLOAD_PART_SIZE

    def __str__(self) -> str:
        return "\n".join(str(op) for op in self.items)


@dataclass
class VersionPair:
    """A pair of 'object versions' in MinIO with the same 'key'.

    The pair represents different versions of the same object at different times.
    The `rewinded_version` is the past version (which may or may not exist), and
    the `latest_version` represents the latest version. The `latest_version` is
    never `None`, but it can be a 'delete marker'.
    """

    rewinded_version: Optional[MinioObject]
    latest_version: MinioObject

    @functools.cached_property
    def update_type(self) -> Operation:
        """Compute the kind of operation that lead to this version pair."""
        old, new = self.rewinded_version, self.latest_version
        if old is None or old.is_delete_marker:
            return Operation.NONE if new.is_delete_marker else Operation.CREATE
        if new.is_delete_marker:
            return Operation.REMOVE
        return Operation.NONE if old.etag == new.etag else Operation.UPDATE


class Rewinder:
    """Implements the rewind feature of the Minio server."""

    def __init__(self, client: Minio, logger: ILogger, part_size: int = DEFAULT_MULTIPART_UPLOAD_PART_SIZE) -> None:
        self._client = client
        self._logger = logger
        self._part_size = part_size

    def download(
        self, bucket: str, source_path: str, local_dir: Path, rewind_timestamp: Optional[datetime] = None
    ) -> None:
        """Download a set of objects from MinIO to a local directory.

        Parameters
        ----------
        bucket : str
            The minio bucket.
        source_path : str
            The key prefix of the objects in MinIO.
        local_dir : Path
            The local directory on this computer to save the MinIO objects to as files.
        rewind_timestamp : datetime, optional
            Get past versions of the objects in MinIO. Rewind time to this timestamp.
        """
        rewind_timestamp = rewind_timestamp or datetime.now(timezone.utc)
        object_map = self.__object_versions_grouped_by_key(S3Path.from_bucket(bucket) / source_path)
        rewinded_versions = (
            self.__get_rewinded_version(versions, rewind_timestamp) for versions in object_map.values()
        )
        downloads = [v for v in rewinded_versions if v and not v.is_delete_marker]
        if not downloads:
            self._logger.error(f"No downloads found in bucket {bucket} at {source_path}")
            return

        local_dir.mkdir(parents=True, exist_ok=True)
        if not self.__dir_is_empty(local_dir):
            self._logger.warning(f"Destination directory '{local_dir}' is not empty.")
        self.__download_objects(bucket, downloads, source_path, local_dir)

    def __download_objects(
        self,
        bucket: str,
        downloads: List[MinioObject],
        source_path: str,
        local_dir: Path,
    ) -> None:
        destination_file_paths = []
        for download in downloads:
            object_path = download.object_name or ""
            filename_and_sub__dir_with_extension = object_path.replace(f"{source_path.rstrip('/')}/", "")
            destination_file_path = local_dir / filename_and_sub__dir_with_extension
            destination_file_paths.append(destination_file_path)

            self.__download_object(
                bucket,
                download,
                destination_file_path,
                object_path,
            )

        self.__remove_local_files(local_dir, destination_file_paths)

    def __remove_local_files(self, local_dir: Path, destination_file_paths: List[Path]) -> None:
        for file in list(local_dir.glob("**/*")):
            if file not in destination_file_paths and not file.is_dir():
                self._logger.debug(f"Removing local file that is not present in MinIO bucket: {file}")
                file.unlink()
                if not any(file.parent.iterdir()):
                    file.parent.rmdir()

    def __download_object(
        self, bucket: str, object_info: MinioObject, destination_file_path: Path, object_path: str
    ) -> None:
        if os.path.exists(destination_file_path) and object_info.etag == self.__etag(Path(destination_file_path)):
            self._logger.debug(f"Skipping download: {destination_file_path}, local and online are the same version.")
            return

        try:
            self._logger.debug(f"Downloading: {destination_file_path} (version_id: {object_info.version_id})")
            self._client.fget_object(
                bucket,
                object_path,
                str(destination_file_path),
                version_id=object_info.version_id,
            )
        except Exception as exception:
            self._logger.exception(f"Could not download object from MinIO: {repr(exception)}.")

    def build_plan(
        self,
        src_dir: Path,
        dst_prefix: S3Path,
        tags: Optional[Tags] = None,
        allow_create_and_delete: bool = False,
        part_size: Optional[int] = None,
    ) -> Plan:
        """Build plan to synchronize a set of objects in MinIO with the contents of a local directory.

        Parameters
        ----------
        src_dir : Path
            Path to local directory containing files.
        dst_prefix : S3Path
            S3 Prefix of a set of objects in MinIO.
        tags: Optional[tags], optional
            Key-value pairs to add to all of the MinIO objects.
        allow_create_and_delete: bool, optional
            By default: `False`. If set to `True`, the plan will include the creation and removal of files
            in the MinIO object repository.
        part_size : Optional[int], optional
            Size in bytes of multipart uploads. If not set use the `multipart_upload_part_size` instance
            variable. The computation of the ETag depends on the value of the multipart upload part size.

        Returns
        -------
        Plan
            A plan containing a sequence of steps to synchronize the set of objects in MinIO pointed
            to by `dst_prefix` with the files in the local directory `src_dir`. After executing the
            steps in this plan, the set of objects in MinIO should be the same as the content of the
            local directory.
        """
        part_size = part_size or self._part_size
        steps: List[PlanItem] = []

        # Get sorted `(key, etag)` pairs from local directory and MinIO prefix.
        minio_sorted = sorted(self.__minio_path_hash_pairs(dst_prefix), reverse=True)
        local_sorted = sorted(self.__local_path_hash_pairs(src_dir, part_size), reverse=True)

        while minio_sorted and local_sorted:
            local_key, local_hash = local_sorted.pop()
            minio_key, minio_hash = minio_sorted.pop()

            local_path = src_dir / local_key
            minio_path = dst_prefix / minio_key
            if local_key < minio_key:
                # New local file: Upload local file to MinIO.
                if allow_create_and_delete:
                    steps.append(PlanItem.create(local_path, dst_prefix / local_key))
                minio_sorted.append((minio_key, minio_hash))  # Push back minio object.
            elif local_key == minio_key and local_hash != minio_hash:
                # Local file and remote object differ: Upload local file to MinIO.
                steps.append(PlanItem.update(local_path, minio_path))
            elif local_key > minio_key:
                # Local file missing corresponding object in MinIO. Remove object from MinIO.
                if allow_create_and_delete:
                    steps.append(PlanItem.remove(minio_path))
                local_sorted.append((local_key, local_hash))  # Push back local file.

        if allow_create_and_delete:
            for local_key, _ in local_sorted:
                # New local file: Upload local file to MinIO.
                steps.append(PlanItem.create(src_dir / local_key, dst_prefix / local_key))

            for minio_key, _ in minio_sorted:
                # Local file missing corresponding object in MinIO. Remove object from MinIO.
                steps.append(PlanItem.remove(dst_prefix / minio_key))

        return Plan(
            local_dir=src_dir,
            minio_prefix=dst_prefix,
            items=steps,
            tags=tags,
            allow_create_and_delete=allow_create_and_delete,
            multipart_upload_part_size=part_size,
        )

    def __minio_path_hash_pairs(self, dst_prefix: S3Path) -> Iterator[Tuple[str, str]]:
        objects = self._client.list_objects(  # Get only the latest versions.
            bucket_name=dst_prefix.bucket,
            prefix=dst_prefix.key,
            recursive=True,
        )

        def postfix(full_path: str, prefix: str) -> str:
            return str(Path(full_path).relative_to(Path(prefix)).as_posix())

        return ((postfix(obj.object_name, dst_prefix.key), obj.etag) for obj in objects)

    def __local_path_hash_pairs(self, src_dir: Path, part_size: Optional[int] = None) -> Iterator[Tuple[str, str]]:
        for path in src_dir.rglob("*"):
            if not path.is_dir():  # `path` is a file.
                etag = self.__etag(path, part_size)
                yield (str(path.relative_to(src_dir).as_posix()), etag)  # Make sure to use unix path separators.
            elif self.__dir_is_empty(path):  # `path` is an empty directory.
                etag = hashlib.md5(b"").hexdigest()
                yield (str((path.relative_to(src_dir) / ".miniokeep").as_posix()), etag)

    def execute_plan(self, plan: Plan) -> None:
        """Execute `Plan` by performing all of the steps.

        Parameters
        ----------
        plan : Plan
            Plan containing steps like `CREATE`, `UPDATE` and `REMOVE` to a set
            of objects in MinIO.
        """
        for item in plan.items:
            if item.operation in (Operation.CREATE, Operation.UPDATE):
                # PutObject.
                local_path = item.local_path
                bucket = item.minio_path.bucket
                key = item.minio_path.key
                part_size = plan.multipart_upload_part_size
                if local_path is None:
                    raise RuntimeError("`operation.type` must have a `local_path`")
                elif local_path.exists():
                    self._client.fput_object(
                        bucket_name=bucket,
                        object_name=key,
                        file_path=str(item.local_path),
                        part_size=part_size,
                        tags=plan.tags,
                    )
                elif local_path.name == ".miniokeep":
                    # Upload empty `.miniokeep` file.
                    self._client.put_object(
                        bucket_name=bucket,
                        object_name=key,
                        data=io.BytesIO(),
                        length=0,
                        tags=plan.tags,
                    )
                else:
                    raise RuntimeError(f"Unable to upload non-existent local file: {local_path}")
            elif item.operation == Operation.REMOVE:
                # DeleteObject.
                self._client.remove_object(
                    bucket_name=item.minio_path.bucket,
                    object_name=item.minio_path.key,
                )

    def detect_conflicts(
        self, prefix: S3Path, timestamp: datetime, add_tags_to_latest: bool = False
    ) -> List[VersionPair]:
        """Detect conflicts in MinIO between now and the given timestamp.

        Parameters
        ----------
        prefix : S3Path
            S3 Prefix of a set of objects in MinIO.
        timestamp : datetime
            Compare the MinIO objects from this point in time to the current objects.
        add_tags_to_latest : bool, optional
            Request and attach the object tags to the latest minio object. Default: False.

        Returns
        -------
        List[VersionPair]
            Return list of conficts between `timestamp` and now.
        """
        object_map = self.__object_versions_grouped_by_key(prefix)
        version_pairs = [
            pair
            for pair in (
                VersionPair(
                    rewinded_version=self.__get_rewinded_version(versions, timestamp),
                    latest_version=versions[0],
                )
                for versions in object_map.values()
            )
            if pair.update_type != Operation.NONE
        ]

        if add_tags_to_latest:
            for pair in version_pairs:
                pair.latest_version = self.__add_object_tags(pair.latest_version)

        return version_pairs

    def list_objects(
        self,
        prefix: S3Path,
        timestamp: Optional[datetime] = None,
        include_delete_markers: bool = False,
        add_tags: bool = False,
    ) -> Iterable[MinioObject]:
        """List objects in Minio at given timestamp.

        If the `timestamp` is not provided, get the latest versions of the objects.

        Parameters
        ----------
        prefix : S3Path
            S3 Prefix of a set of objects in MinIO.
        timestamp : Optional[datetime], optional
            List the MinIO objects at this point in time. Or list the latest objects
            if `timestamp` is `None`.
        add_tags : bool, optional
            Request and attach the object tags to the latest minio object. Default: `False`.
        include_delete_markers : bool, optional
            If `False`, filter out delete markers. Default: `False`

        Returns
        -------
        Iterable[MinioObject]
        """
        if timestamp is None:
            timestamp = datetime.max.replace(tzinfo=timezone.utc)

        object_map = self.__object_versions_grouped_by_key(prefix)
        objects: Iterable[MinioObject] = (
            rewinded_version
            for versions in object_map.values()
            if (rewinded_version := self.__get_rewinded_version(versions, timestamp)) is not None
        )

        if not include_delete_markers:
            # Filter out delete markers.
            objects = (obj for obj in objects if not obj.is_delete_marker)

        if add_tags:
            return (self.__add_object_tags(obj) for obj in objects)
        return objects

    @staticmethod
    def __get_rewinded_version(versions: Iterable[MinioObject], timestamp: datetime) -> Optional[MinioObject]:
        """Get rewinded version of object.

        Note: Make sure `versions` is sorted in *descending* order.
        """
        return next((obj for obj in versions if obj.last_modified <= timestamp), None)  # type: ignore

    def __object_versions_grouped_by_key(self, prefix: S3Path) -> Mapping[str, List[MinioObject]]:
        """List object versions in MinIO by `prefix` and group them by object key.

        Parameters
        ----------
        prefix : S3Path
            This `S3Path` should contain both the bucket name and the *key prefix* of
            a set of objects in MinIO.

        Returns
        -------
        Mapping[str, List[MinioObject]]
            A mapping from object keys to a list of object versions. The list of object
            versions is sorted by their `last_modified` property in *descending* order.
            So the *latest* object version is first in the list.
        """
        sorted_objects = sorted(
            (
                obj
                for obj in self._client.list_objects(
                    bucket_name=prefix.bucket,
                    prefix=prefix.key,
                    recursive=True,
                    include_version=True,
                )
                if all((obj.object_name, obj.last_modified))
            ),
            key=lambda obj: (obj.object_name, obj.last_modified),  # Sort first by key, then by published date.
            reverse=True,  # Latest versions come first.
        )

        def collector(obj_map: Mapping[str, List[MinioObject]], obj: MinioObject) -> Mapping[str, List[MinioObject]]:
            obj_map[obj.object_name].append(obj)  # type: ignore
            return obj_map

        return functools.reduce(collector, sorted_objects, defaultdict(list))

    def __etag(self, path: Path, part_size: Optional[int] = None) -> str:
        """Compute the `ETag` of the contents of a file.

        Notes
        -----
        For "small" files, the ETag is the same as an MD5 `hexdigest`. Unfortunately, when files are large
        enough to be uploaded in multiple parts, the ETag computation changes:
        1. Compute the MD5 `digest` for each separate part
        2. Concatenate all of the MD5 digests for each uploaded part.
        3. Compute the hexdigest of the result, and add `-{n}`, where `n` is the number of parts.
        The value of the ETag depends on the size of the parts used during the multipart upload.
        See: https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums
        """
        part_size = part_size or self._part_size

        def chunker(f: io.BufferedReader, size: int) -> Iterator[bytes]:
            chunk = f.read(size)
            while chunk:
                yield chunk
                chunk = f.read(size)

        with open(path, "rb") as f:
            hashes = [hashlib.md5(chunk) for chunk in chunker(f, part_size)]

        if not hashes:
            return hashlib.md5(b"").hexdigest()  # Emtpy file
        elif len(hashes) == 1:
            return hashes[0].hexdigest()  # Regular file
        else:
            digests = b"".join(h.digest() for h in hashes)  # Files split in multiple parts
            return hashlib.md5(digests).hexdigest() + f"-{len(hashes)}"

    def __add_object_tags(self, obj: MinioObject) -> MinioObject:
        if obj.is_delete_marker:
            return obj  # Deleted object don't have tags for some reason.

        tags = self._client.get_object_tags(obj.bucket_name, obj.object_name, obj.version_id)  # type: ignore
        return MinioObject(
            obj.bucket_name,
            obj.object_name,
            last_modified=obj.last_modified,
            etag=obj.etag,
            size=obj.size,
            version_id=obj.version_id,
            is_latest=obj.is_latest,
            tags=tags,
        )

    @staticmethod
    def __dir_is_empty(dir: Path) -> bool:
        """Return `True` if and only if directory is empty."""
        return next(dir.iterdir(), None) is None
