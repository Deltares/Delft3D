import logging
import queue
import signal
from concurrent.futures import ALL_COMPLETED, ThreadPoolExecutor, wait
from dataclasses import dataclass
from logging import Logger
from pathlib import Path
from threading import Condition, Event
from types import FrameType
from typing import BinaryIO

from minio import Minio
from minio.error import MinioException
from s3_path_wrangler.paths import S3Path
from typing_extensions import override
from urllib3 import BaseHTTPResponse

from ci_tools.minio.list_item import ListItem
from ci_tools.minio.progress_bar import ProgressBar
from ci_tools.minio.synchronize import DEFAULT_QUEUE_SIZE, DEFAULT_WORKER_COUNT
from ci_tools.minio.synchronize.sync_plan import Synchronizer, SyncPlan


@dataclass
class DownloadPart:
    """Part of an object downloaded from MinIO."""

    list_item: ListItem
    index: int
    content: bytes


class Download:
    """Tracks the progress of downloading an object from MinIO."""

    def __init__(self, open_file: BinaryIO, parts_left: set[int], part_size: int) -> None:
        self._file = open_file
        self._parts_left = parts_left
        self._part_size = part_size

    @staticmethod
    def from_local_path(local_path: Path, part_size: int, part_count: int) -> "Download":
        """Create a `Download` object from a `Path`.

        This opens the file for writing and keeps track of the parts that
        are still left to download.

        Parameters
        ----------
        local_path : Path
            The path to the file to write.
        part_size : int
            The size of each part in bytes.
        part_count : int
            The total number of parts to download.

        Returns
        -------
        Download
        """
        local_path.parent.mkdir(parents=True, exist_ok=True)
        return Download(
            open_file=local_path.open("wb"),
            parts_left=set(range(part_count)),
            part_size=part_size,
        )

    def write_part(self, part: DownloadPart) -> None:
        """Write a part of the object to the local file.

        Be aware that parts of the object content may arrive out of order.
        So we need to keep track of which parts are still missing, and make
        sure to seek in the file to the correct position before writing the
        part content.

        Parameters
        ----------
        part : DownloadPart
        """
        self._file.seek(part.index * self._part_size)
        self._file.write(part.content)
        self._parts_left.remove(part.index)
        if self.is_complete():
            self.close()

    def is_complete(self) -> bool:
        """Check if all parts of the object have been downloaded."""
        return not self._parts_left

    def close(self) -> None:
        """Close the file handle."""
        self._file.close()


class RemoteToLocal(Synchronizer):
    """Synchronize a set of objects in a MinIO bucket to a local directory.

    This class submits a 'download part' task for each part of an object to
    be downloaded from MinIO. Big files are downloaded in multiple parts, and
    put together by a 'part collector' thread. There is only a single part
    collector task, which is responsible for writing all of the files to
    disk.
    """

    def __init__(
        self,
        plan: SyncPlan,
        minio_client: Minio,
        source_prefix: S3Path,
        destination_directory: Path,
        worker_count: int = DEFAULT_WORKER_COUNT,
        collector_queue_max_size: int = DEFAULT_QUEUE_SIZE,
        show_progress: bool = True,
        logger: Logger | None = None,
    ) -> None:
        self._plan = plan
        self._minio_client = minio_client
        self._source_prefix = source_prefix
        self._destination_directory = destination_directory

        self._thread_count = worker_count
        self._collector_queue: queue.Queue[DownloadPart | None] = queue.Queue(maxsize=collector_queue_max_size + 1)

        self._logger = logger or logging.getLogger(__name__)
        self._show_progress = show_progress

        self._consumer_start = Event()
        self._cancel = Event()
        self._collector_queue_max_size = collector_queue_max_size
        self._collector_queue_not_full = Condition()

    @override
    def synchronize(self) -> None:
        signal.signal(signal.SIGINT, self._signal_handler)  # Set Ctrl-C handler.

        self._logger.info("Starting synchronization")

        with ThreadPoolExecutor(max_workers=self._thread_count, thread_name_prefix="DownloaderPool") as executor:
            collector_future = executor.submit(self._part_collector)
            self._consumer_start.wait()

            download_futures = (
                executor.submit(self._download_part, download, part_index)
                for download in self._plan.copies
                for part_index in range(download.part_count(self._plan.part_size))
            )
            wait(download_futures, return_when=ALL_COMPLETED)
            self._logger.info("All part downloads have been completed or canceled")
            self._put_collector_message(None)  # Signal: No more download parts are added to the queue.

            wait([collector_future], return_when=ALL_COMPLETED)

            if not self._cancel.is_set():
                deletions = self._plan.deletions
                delete_futures = (executor.submit(self._delete_local_object, item) for item in deletions)
                wait(delete_futures, return_when=ALL_COMPLETED)
                self._delete_empty_directories(self._destination_directory)
                self._logger.info("All deletes have been completed or canceled")

        self._logger.info("Synchronization finished")

    def _part_collector(self) -> None:
        self._consumer_start.set()
        self._logger.info("Part collector started")

        part_size = self._plan.part_size
        objects_to_downloads = self._plan.copies

        downloads: dict[str, Download] = {}
        total_progress = ProgressBar(
            total_size=sum(obj.size for obj in objects_to_downloads),
            object_count=len(objects_to_downloads),
            show_progress_bar=self._show_progress,
        )
        total_progress.display()

        try:
            while not self._cancel.is_set():
                message = self._collector_queue.get()
                if message is None:
                    self._collector_queue_task_done()
                    break

                list_item = message.list_item
                remote_object = self._source_prefix / list_item.relative_path
                local_path = self._destination_directory / list_item.relative_path
                download = downloads.get(list_item.relative_path, None)
                if download is None:
                    download = Download.from_local_path(
                        local_path=local_path,
                        part_size=part_size,
                        part_count=list_item.part_count(part_size),
                    )
                    downloads[list_item.relative_path] = download

                download.write_part(message)
                if download.is_complete():
                    total_progress.print(f"download: {remote_object} to {local_path}")
                    total_progress.add_object_count(1)
                    downloads.pop(list_item.relative_path)
                total_progress.add_size(len(message.content))

                total_progress.display()
                self._collector_queue_task_done()
        except IOError:
            self._logger.exception("File IO operation failed")
            self._cancel_download()
        finally:
            # Clean up.
            total_progress.close()
            for relative_path, download in downloads.items():
                self._logger.error("Part collector has not received all parts of object %s", relative_path)
                download.close()

            self._logger.info("Stopping part collector")

    def _download_part(self, list_item: ListItem, part_index: int) -> None:
        if self._cancel.is_set():
            return

        if list_item.size == 0:
            # If the object size is zero, the content is empty.
            # `get_object` raises an error when called with zero offset and length
            content = bytes()
        else:
            part_size = self._plan.part_size
            content = self._get_object_part(
                remote_object=self._source_prefix / list_item.relative_path,
                offset=part_index * part_size,
                length=part_size,
                version_id=list_item.version,
            )

        if not self._cancel.is_set():
            self._put_collector_message(DownloadPart(list_item, part_index, content))

    def _delete_local_object(self, list_item: ListItem) -> None:
        if self._cancel.is_set():
            return

        local_path = self._destination_directory / list_item.relative_path
        local_path.unlink()
        print(f"delete: {local_path}")

    def _delete_empty_directories(self, directory: Path) -> None:
        # The order is important: First delete all subdirectories, then try to delete
        # the current directory. This guarantees that no empty directories are left behind.
        for subdir in (path for path in directory.iterdir() if path.is_dir()):
            self._delete_empty_directories(subdir)
        if self._dir_is_empty(directory) and directory != self._destination_directory:
            directory.rmdir()
            print(f"delete empty directory: {directory}")

    def _signal_handler(self, signum: int, frame: FrameType | None) -> None:
        print("\nAttempting to cancel the downloads gracefully.")
        print("It may take a few seconds to join the threads.")
        self._cancel_download()

    @staticmethod
    def _dir_is_empty(path: Path) -> bool:
        return not any(path.iterdir())

    def _get_object_part(self, remote_object: S3Path, offset: int, length: int, version_id: str | None = None) -> bytes:
        response: BaseHTTPResponse | None = None
        try:
            response = self._minio_client.get_object(
                bucket_name=remote_object.bucket,
                object_name=remote_object.key,
                version_id=version_id,
                offset=offset,
                length=length,
            )
            return response.read()
        except MinioException:
            self._logger.exception("get_object failed")
            self._cancel_download()
            raise
        finally:
            if response is not None:
                response.close()
                response.release_conn()

    def _cancel_download(self) -> None:
        self._cancel.set()
        self._collector_queue.put(None)  # Signal: Notify collector to stop.
        self._notify_collector_queue_not_full()

    def _put_collector_message(self, message: DownloadPart | None) -> None:
        if message is None:
            self._collector_queue.put(None)
            return

        with self._collector_queue_not_full:
            while not self._cancel.is_set() and self._collector_queue.qsize() >= self._collector_queue_max_size:
                self._collector_queue_not_full.wait()
            if not self._cancel.is_set():
                self._collector_queue.put(message)

    def _notify_collector_queue_not_full(self) -> None:
        with self._collector_queue_not_full:
            self._collector_queue_not_full.notify_all()

    def _collector_queue_task_done(self) -> None:
        self._collector_queue.task_done()
        self._notify_collector_queue_not_full()
