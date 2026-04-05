from datetime import datetime, timezone
from logging import Logger
from pathlib import Path
from typing import Iterable

from minio import Minio
from minio.credentials.providers import AWSConfigProvider
from s3_path_wrangler.paths import S3Path

from ci_tools.minio import DEFAULT_MINIO_HOSTNAME, DEFAULT_MULTIPART_UPLOAD_PART_SIZE
from ci_tools.minio.list_item import DirectoryListing, ListItem, MinioPrefixListing
from ci_tools.minio.matcher import Matcher
from ci_tools.minio.synchronize import DEFAULT_QUEUE_SIZE, DEFAULT_WORKER_COUNT
from ci_tools.minio.synchronize.local_to_remote import LocalToRemote
from ci_tools.minio.synchronize.remote_to_local import RemoteToLocal
from ci_tools.minio.synchronize.sync_plan import Changes, Mode, Synchronizer, SyncPlan


class SyncBuilder:
    """Builder for Synchronizer instances."""

    def __init__(self) -> None:
        self._mode: Mode = Mode.NO_DELETE

        self._local_directory: Path | None = None
        self._remote_prefix: S3Path | None = None
        self._remote_timestamp: datetime | None = None

        self._includes: list[Matcher] = []
        self._excludes: list[Matcher] = []

        self._minio_client: Minio | None = None
        self._logger: Logger | None = None

        self._show_progress: bool = True
        self._thread_count: int = DEFAULT_WORKER_COUNT
        self._max_queue_size: int = DEFAULT_QUEUE_SIZE
        self._part_size: int = DEFAULT_MULTIPART_UPLOAD_PART_SIZE

    def set_mode(self, mode: Mode) -> "SyncBuilder":
        """Set the synchronization mode."""
        self._mode = mode
        return self

    def set_part_size(self, part_size: int) -> "SyncBuilder":
        """Set the part size for multipart uploads."""
        self._part_size = part_size
        return self

    def set_thread_count(self, thread_count: int) -> "SyncBuilder":
        """Set the number of threads to use for synchronization."""
        self._thread_count = thread_count
        return self

    def set_show_progress(self, show_progress: bool) -> "SyncBuilder":
        """Set whether to show the progress bar during synchronization."""
        self._show_progress = show_progress
        return self

    def set_max_queue_size(self, max_queue_size: int) -> "SyncBuilder":
        """Set the maximum size of the queue for the synchronization.

        The messages in the queue are about the size of the `part_size`.
        So the maximum amount of memory is limited to about `max_queue_size * part_size`.
        """
        self._max_queue_size = max_queue_size
        return self

    def set_minio_client(self, minio_client: Minio) -> "SyncBuilder":
        """Set the MinIO client to use for synchronization."""
        self._minio_client = minio_client
        return self

    def set_logger(self, logger: Logger) -> "SyncBuilder":
        """Set the logger that will be used to log information during the synchronization."""
        self._logger = logger
        return self

    def set_local_directory(self, local_directory: Path) -> "SyncBuilder":
        """Set the local directory to either download files into or upload files from."""
        self._local_directory = local_directory
        return self

    def set_remote_prefix(self, remote_prefix: S3Path) -> "SyncBuilder":
        """Set the remote prefix to either download objects from or upload objects to."""
        self._remote_prefix = remote_prefix
        return self

    def set_remote_timestamp(self, timestamp: datetime | None) -> "SyncBuilder":
        """Set the timestamp to use for synchronization.

        This is only supported for remote to local synchronization.
        If the timestamp is not timezone-aware, it will be converted to UTC.
        """
        if timestamp is not None and timestamp.tzinfo is None:
            timestamp = timestamp.astimezone(timezone.utc)
        self._remote_timestamp = timestamp
        return self

    def add_include(self, include: Matcher) -> "SyncBuilder":
        """Add an include matcher, to limit the set of files or object to synchronize."""
        self._includes.append(include)
        return self

    def add_includes(self, includes: Iterable[Matcher]) -> "SyncBuilder":
        """Add multiple include matchers, to limit the set of files or object to synchronize."""
        self._includes.extend(includes)
        return self

    def add_exclude(self, exclude: Matcher) -> "SyncBuilder":
        """Add an exclude matcher, to limit the set of files or object to synchronize."""
        self._excludes.append(exclude)
        return self

    def add_excludes(self, excludes: Iterable[Matcher]) -> "SyncBuilder":
        """Add multiple exclude matchers, to limit the set of files or object to synchronize."""
        self._excludes.extend(excludes)
        return self

    def build_local_to_remote(self) -> Synchronizer:
        """Build a Synchronizer for uploading local file to MinIO."""
        if self._remote_timestamp is not None:
            raise ValueError("The remote timestamp is not supported for local to remote synchronization.")

        source = self._get_local_directory()
        destination = self._get_remote_prefix()
        minio_client = self._get_minio_client()

        file_reader_count = max(self._thread_count // 4, 1)
        part_uploader_count = max(self._thread_count - file_reader_count - 1, 1)

        # A synchronization with an empty source is considered a user error.
        source_items = list(self._get_local_listing())
        if not source_items:
            message = "No files to upload. Please check the local directory."
            if self._includes or self._excludes:
                message += "\nPlease check if your 'include' and/or 'exclude' patterns are not too restrictive."
            raise ValueError(message)

        changes = Changes.from_listings(source_items=source_items, destination_items=self._get_remote_listing())
        plan = SyncPlan(mode=self._mode, changes=changes, part_size=self._part_size)

        return LocalToRemote(
            plan=plan,
            minio_client=minio_client,
            source_directory=source,
            destination_prefix=destination,
            file_reader_count=file_reader_count,
            part_uploader_count=part_uploader_count,
            show_progress=self._show_progress,
            uploader_queue_max_size=self._max_queue_size,
            logger=self._logger,
        )

    def build_remote_to_local(self) -> Synchronizer:
        """Build a Synchronizer for downloading MinIO objects to a local directory."""
        source = self._get_remote_prefix()
        destination = self._get_local_directory()
        minio_client = self._get_minio_client()

        # A synchronization with an empty source is considered a user error.
        source_items = list(self._get_remote_listing())
        if not source_items:
            message = "No objects to download. Please check the remote prefix."
            if self._includes or self._excludes:
                message += "\nPlease check if your 'include' and/or 'exclude' patterns are not too restrictive."
            raise ValueError(message)

        changes = Changes.from_listings(source_items=source_items, destination_items=self._get_local_listing())
        plan = SyncPlan(mode=self._mode, changes=changes, part_size=self._part_size)
        return RemoteToLocal(
            plan=plan,
            minio_client=minio_client,
            source_prefix=source,
            destination_directory=destination,
            worker_count=self._thread_count,
            collector_queue_max_size=self._max_queue_size,
            show_progress=self._show_progress,
            logger=self._logger,
        )

    def _include_exclude_filter(self, list_item: ListItem) -> bool:
        return self._matches_any_include(list_item) and self._mismatches_all_excludes(list_item)

    def _matches_any_include(self, list_item: ListItem) -> bool:
        if not self._includes:
            return True
        return any(pattern.match(list_item) for pattern in self._includes)

    def _mismatches_all_excludes(self, list_item: ListItem) -> bool:
        return all(not pattern.match(list_item) for pattern in self._excludes)

    def _get_minio_client(self) -> Minio:
        if self._minio_client is not None:
            return self._minio_client

        return Minio(
            endpoint=DEFAULT_MINIO_HOSTNAME,
            credentials=AWSConfigProvider(),
        )

    def _get_local_listing(self) -> Iterable[ListItem]:
        local_objects = DirectoryListing(self._get_local_directory())
        return filter(self._include_exclude_filter, local_objects)

    def _get_remote_listing(self) -> Iterable[ListItem]:
        remote_objects = MinioPrefixListing(
            client=self._get_minio_client(),
            prefix=self._get_remote_prefix(),
            timestamp=self._remote_timestamp,
        )
        return filter(self._include_exclude_filter, remote_objects)

    def _get_local_directory(self) -> Path:
        if self._local_directory is None:
            raise ValueError("Local directory must be set")
        return self._local_directory

    def _get_remote_prefix(self) -> S3Path:
        if self._remote_prefix is None:
            raise ValueError("Remote prefix must be set")
        return self._remote_prefix
