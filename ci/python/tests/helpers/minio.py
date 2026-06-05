import logging
from datetime import datetime, timedelta, timezone
from pathlib import Path

import minio
from minio import Minio
from minio.datatypes import Object as MinioObject
from s3_path_wrangler.paths import S3Path
from urllib3 import BaseHTTPResponse

from ci_tools.minio import DEFAULT_MULTIPART_UPLOAD_PART_SIZE
from ci_tools.minio.etag import ConstEtag
from ci_tools.minio.list_item import ListItem
from ci_tools.minio.progress_bar import Clock, ProgressBar, TerminalSize
from ci_tools.minio.synchronize.local_to_remote import LocalToRemote
from ci_tools.minio.synchronize.remote_to_local import RemoteToLocal
from ci_tools.minio.synchronize.sync_plan import Changes, Mode, SyncPlan


class ConstTerminalSize(TerminalSize):
    def __init__(self, lines: int, columns: int) -> None:
        self._lines = lines
        self._columns = columns

    @property
    def terminal_size(self) -> tuple[int, int]:
        return (self._lines, self._columns)


class ManualClock(Clock):
    def __init__(self, init: datetime | None = None) -> None:
        self._time = init or datetime.now(timezone.utc)

    def now(self) -> datetime:
        return self._time

    def add(self, delta: timedelta) -> None:
        self._time += delta


def make_list_item(
    relative_path: str,
    size: int = 0,
    timestamp: datetime | None = None,
    version: str | None = None,
    etag: str = "badc0de",
) -> ListItem:
    timestamp = timestamp or datetime.now(timezone.utc)
    return ListItem(
        relative_path=relative_path,
        size=size,
        timestamp=timestamp,
        version=version,
        _etag=ConstEtag(etag),
    )


def make_object(
    key: str,
    bucket_name: str = "my-bucket",
    size: int = 0,
    etag: str = "d41d8cd98f00b204e9800998ecf8427e",
    version_id: str | None = None,
    last_modified: datetime | None = None,
    is_delete_marker: bool = False,
) -> MinioObject:
    last_modified = last_modified or datetime.now(timezone.utc)
    return MinioObject(
        bucket_name=bucket_name,
        object_name=key,
        size=size,
        etag=etag,
        version_id=version_id,
        last_modified=last_modified,
        is_delete_marker=is_delete_marker,
    )


def make_progress_bar(
    total_size: int = 1000,
    object_count: int = 10,
    show_progress_bar: bool = True,
    terminal_size: TerminalSize | None = None,
    clock: Clock | None = None,
) -> ProgressBar:
    terminal_size = terminal_size or ConstTerminalSize(24, 80)
    clock = clock or ManualClock(datetime.now(timezone.utc))
    return ProgressBar(
        total_size=total_size,
        object_count=object_count,
        show_progress_bar=show_progress_bar,
        terminal_size=terminal_size,
        clock=clock,
    )


def make_changes(
    creations: list[ListItem] | None = None,
    updates: list[ListItem] | None = None,
    deletions: list[ListItem] | None = None,
) -> Changes:
    return Changes(
        creations=creations or [],
        updates=updates or [],
        deletions=deletions or [],
    )


def make_sync_plan(
    mode: Mode = Mode.NO_DELETE,
    changes: Changes | None = None,
    part_size: int = DEFAULT_MULTIPART_UPLOAD_PART_SIZE,
) -> SyncPlan:
    changes = changes or make_changes()
    return SyncPlan(mode=mode, changes=changes, part_size=part_size)


def make_local_to_remote(
    minio_client: Minio,
    source_directory: Path | None = None,
    destination_prefix: S3Path | None = None,
    plan: SyncPlan | None = None,
    file_reader_count: int = 1,
    part_uploader_count: int = 1,
    uploader_queue_max_size: int = 8,
    show_progress: bool = False,
    logger: logging.Logger | None = None,
) -> LocalToRemote:
    source_directory = source_directory or Path("my_files")
    destination_prefix = destination_prefix or S3Path("s3://my-bucket/my_objects")
    logger = logging.getLogger(__name__)
    plan = plan or make_sync_plan()
    return LocalToRemote(
        plan=plan,
        minio_client=minio_client,
        source_directory=source_directory,
        destination_prefix=destination_prefix,
        file_reader_count=file_reader_count,
        part_uploader_count=part_uploader_count,
        uploader_queue_max_size=uploader_queue_max_size,
        show_progress=show_progress,
        logger=logger,
    )


def make_remote_to_local(
    minio_client: Minio,
    destination_directory: Path | None = None,
    source_prefix: S3Path | None = None,
    plan: SyncPlan | None = None,
    worker_count: int = 2,
    collector_queue_max_size: int = 8,
    show_progress: bool = False,
    logger: logging.Logger | None = None,
) -> RemoteToLocal:
    destination_directory = destination_directory or Path("my_files")
    source_prefix = source_prefix or S3Path("s3://my-bucket/my_objects")
    logger = logging.getLogger(__name__)
    plan = plan or make_sync_plan()
    return RemoteToLocal(
        plan=plan,
        minio_client=minio_client,
        source_prefix=source_prefix,
        destination_directory=destination_directory,
        worker_count=worker_count,
        collector_queue_max_size=collector_queue_max_size,
        show_progress=show_progress,
        logger=logger,
    )


def _make_s3_error() -> minio.error.S3Error:
    return minio.error.S3Error(
        code="NoSuchBucket",
        message="The specified bucket does not exist",
        resource="/bucket",
        request_id="request-id",
        host_id="host-id",
        response=BaseHTTPResponse(
            headers={},
            status=404,
            version=42,
            version_string="HTTP/1.1",
            reason="Not Found",
            decode_content=False,
            request_url="bucket",
        ),
    )
