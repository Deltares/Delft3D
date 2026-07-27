import contextlib
import logging
import queue
import signal
from concurrent.futures import ALL_COMPLETED, ThreadPoolExecutor, wait
from dataclasses import dataclass
from pathlib import Path
from threading import Condition, Event
from types import FrameType
from typing import Iterator

from minio import Minio
from minio.datatypes import Part as Part
from minio.error import MinioException
from s3_path_wrangler.paths import S3Path
from typing_extensions import override

from ci_tools.minio.list_item import ListItem
from ci_tools.minio.progress_bar import ProgressBar
from ci_tools.minio.synchronize import DEFAULT_QUEUE_SIZE, DEFAULT_WORKER_COUNT
from ci_tools.minio.synchronize.sync_plan import Synchronizer, SyncPlan


@dataclass
class PartInfo:
    """Information about a part of a multipart upload."""

    upload_id: str
    part_number: int


@dataclass
class UploaderMessage:
    """Message sent to the part-uploader queue."""

    list_item: ListItem
    content: bytes
    part_info: PartInfo | None = None


@dataclass
class CollectorMessage:
    """Message sent to the collector queue."""

    list_item: ListItem
    etag: str
    size: int
    part_info: PartInfo | None = None


class MultipartUpload:
    """Tracks the progress of a single multipart upload."""

    def __init__(self, upload_id: str, list_item: ListItem, part_size: int) -> None:
        self.upload_id = upload_id
        self.list_item = list_item
        self._part_size = part_size
        self._part_list: list[Part] = []

    @property
    def part_list(self) -> list[Part]:
        """Return the list of parts sorted by part number.

        The `complete_multipart_upload` method requires the parts to be sorted by part number.

        Returns
        -------
        list[Part]
            The list of parts sorted by part number.
        """
        return sorted(self._part_list, key=lambda part: part.part_number)

    def add_part(self, part_number: int, etag: str, size: int) -> None:
        """Add a part to the list of completed parts.

        Parameters
        ----------
        part_number : int
            The part number of the completed part.
        etag : str
            The ETag of the completed part.
        size : int
            The size of the completed part.
        """
        self._part_list.append(Part(part_number=part_number, etag=etag, size=size))

    def is_complete(self) -> bool:
        """Check if the multipart upload is complete."""
        return len(self._part_list) == self.list_item.part_count(self._part_size)


class LocalToRemote(Synchronizer):
    """Executes a `SyncPlan` from a local directory to a remote prefix.

    This class uses multipart uploads for files larger than the `part_size` specified
    in the `SyncPlan`.
    This class runs three types of workers in parallel.
    - File readers: Read local files, break large files up into parts, and submit them
      to the part uploader queue.
    - Part uploaders: Uploads the parts to the bucket using multipart uploads if
      necessary, and submit completed parts to the part collector queue.
    - Collector: Collects the completed multipart uploads and completes them.

    The workers communicate mostly through queues. Upon receiving a SIGINT signal,
    the workers will stop what they're doing and any multipart uploads that are
    still in progress are aborted.

    See: https://docs.aws.amazon.com/AmazonS3/latest/userguide/mpuoverview.html
    """

    def __init__(
        self,
        plan: SyncPlan,
        minio_client: Minio,
        source_directory: Path,
        destination_prefix: S3Path,
        file_reader_count: int = DEFAULT_WORKER_COUNT // 4,
        part_uploader_count: int = DEFAULT_WORKER_COUNT - (DEFAULT_WORKER_COUNT // 4) - 1,
        logger: logging.Logger | None = None,
        uploader_queue_max_size: int = DEFAULT_QUEUE_SIZE,
        show_progress: bool = True,
    ) -> None:
        self._plan = plan
        self._minio_client = minio_client
        self._source_directory = source_directory
        self._destination_prefix = destination_prefix

        self._logger = logger or logging.getLogger(__name__)
        self._reader_count = file_reader_count
        self._uploader_count = part_uploader_count
        self._show_progress = show_progress
        self._worker_count = file_reader_count + part_uploader_count + 1

        self._file_reader_queue: queue.Queue[ListItem | None] = queue.Queue()
        self._part_uploader_queue: queue.Queue[UploaderMessage | None] = queue.Queue(
            maxsize=uploader_queue_max_size + 1
        )
        self._collector_queue: queue.Queue[CollectorMessage | None] = queue.Queue()

        self._uploader_queue_max_size = uploader_queue_max_size
        self._uploader_queue_not_full = Condition()
        self._cancel = Event()

    @override
    def synchronize(self) -> None:
        signal.signal(signal.SIGINT, self._signal_handler)  # Set Ctrl-C handler.

        for upload in self._plan.copies:
            self._file_reader_queue.put(upload)
        self._file_reader_queue.put(None)

        self._logger.info("Starting synchronization")

        with ThreadPoolExecutor(max_workers=self._worker_count, thread_name_prefix="UploaderPool") as executor:
            reader_futures = [executor.submit(self._file_reader) for _ in range(self._reader_count)]
            uploader_futures = [executor.submit(self._part_uploader) for _ in range(self._uploader_count)]
            collector_future = executor.submit(self._part_collector)

            wait(reader_futures, return_when=ALL_COMPLETED)
            self._logger.info("All file readers finished")

            if not self._cancel.is_set():
                self._put_uploader_message(None)  # Signal: No more parts will be read from files.
                uploader_futures.extend([executor.submit(self._part_uploader) for _ in range(self._reader_count)])

            wait(uploader_futures, return_when=ALL_COMPLETED)
            self._logger.info("All part uploaders finished")

            self._collector_queue.put(None)  # Signal: No more parts will be uploaded to MinIO.

            wait([collector_future], return_when=ALL_COMPLETED)

            if not self._cancel.is_set():
                for list_item in self._plan.deletions:
                    executor.submit(self._delete_remote_object, list_item)

        self._logger.info("Synchronization finished")

    def _file_reader(self) -> None:
        self._logger.info("File reader started")

        part_size = self._plan.part_size
        while not self._cancel.is_set():
            list_item = self._file_reader_queue.get()
            if list_item is None:
                self._file_reader_queue.task_done()
                self._file_reader_queue.put(None)
                break

            local_file = self._source_directory / list_item.relative_path
            remote_object = self._destination_prefix / list_item.relative_path
            if list_item.size >= part_size:  # Start multipart upload.
                upload_id = self._create_multipart_upload(remote_object)
                part_count = list_item.part_count(part_size)
                with local_file.open("rb") as open_file:
                    for part_number in range(1, part_count + 1):
                        uploader_message = UploaderMessage(
                            list_item=list_item,
                            content=open_file.read(part_size),
                            part_info=PartInfo(upload_id, part_number),
                        )
                        self._put_uploader_message(uploader_message)
            else:  # No multipart upload necessary.
                uploader_message = UploaderMessage(
                    list_item=list_item,
                    content=local_file.read_bytes(),
                )
                self._put_uploader_message(uploader_message)
            self._file_reader_queue.task_done()

        self._logger.info("Stopping file reader")

    def _part_uploader(self) -> None:
        self._logger.info("Part uploader started")

        while not self._cancel.is_set():
            message = self._part_uploader_queue.get()
            if message is None:
                self._uploader_queue_task_done()
                self._put_uploader_message(None)
                break

            list_item = message.list_item
            content = message.content
            part_info = message.part_info

            remote_object = self._destination_prefix / list_item.relative_path
            if part_info is not None:  # Part of multipart upload.
                etag = self._upload_part(remote_object, content, part_info)
            else:  # Upload in one go with PutObject.
                etag = self._put_object(remote_object, content)

            if not self._cancel.is_set():
                self._collector_queue.put(
                    CollectorMessage(
                        list_item=list_item,
                        etag=etag,
                        size=len(content),
                        part_info=part_info,
                    )
                )

            self._uploader_queue_task_done()

        self._logger.info("Stopping part uploader")

    def _part_collector(self) -> None:
        self._logger.info("Part collector started")

        part_size = self._plan.part_size
        total_size = sum(obj.size for obj in self._plan.copies)
        object_count = len(self._plan.copies)

        total_progress = ProgressBar(total_size, object_count, show_progress_bar=self._show_progress)
        total_progress.display()
        multipart_uploads: dict[str, MultipartUpload] = {}
        try:
            while not self._cancel.is_set():
                message = self._collector_queue.get()
                if message is None:
                    self._collector_queue.task_done()
                    break

                list_item = message.list_item
                local_path = self._source_directory / list_item.relative_path
                remote_object = self._destination_prefix / list_item.relative_path
                part_info = message.part_info

                if part_info is not None:  # Part of multipart upload.
                    multipart_upload = multipart_uploads.get(list_item.relative_path, None)
                    if multipart_upload is None:
                        multipart_upload = MultipartUpload(
                            upload_id=part_info.upload_id,
                            list_item=list_item,
                            part_size=part_size,
                        )
                        multipart_uploads[list_item.relative_path] = multipart_upload

                    multipart_upload.add_part(part_info.part_number, message.etag, message.size)
                    if multipart_upload.is_complete():
                        self._complete_multipart_upload(
                            remote_object=remote_object,
                            upload_id=part_info.upload_id,
                            part_list=multipart_upload.part_list,
                        )
                        multipart_uploads.pop(list_item.relative_path)

                        total_progress.print(f"upload: {local_path} to {remote_object}")
                        total_progress.add_object_count(1)
                    total_progress.add_size(message.size)

                else:  # Not a multipart upload.
                    total_progress.print(f"upload: {local_path} to {remote_object}")
                    total_progress.add_size(message.size)
                    total_progress.add_object_count(1)

                total_progress.display()
                self._collector_queue.task_done()
        finally:
            # Abort incomplete multipart uploads.
            total_progress.close()
            for multipart_upload in multipart_uploads.values():
                list_item = multipart_upload.list_item
                remote_object = self._destination_prefix / list_item.relative_path
                missing_parts = list_item.part_count(part_size) - len(multipart_upload.part_list)
                self._logger.error(
                    "Missing %d parts in multipart upload for object %s", missing_parts, remote_object.key
                )
                self._abort_multipart_upload(remote_object, multipart_upload.upload_id)

            self._logger.info("Stopping part collector")

    def _delete_remote_object(self, list_item: ListItem) -> None:
        if self._cancel.is_set():
            return

        remote_object = self._destination_prefix / list_item.relative_path
        self._remove_object(remote_object)
        print(f"delete: {remote_object}")

    def _signal_handler(self, signum: int, frame: FrameType | None) -> None:
        print("\nAttempting to cancel the uploads gracefully.")
        print("It may take a few seconds to abort the multipart uploads and join the threads.")
        self._cancel_upload()

    def _cancel_upload(self) -> None:
        self._cancel.set()
        self._put_uploader_message(None)
        with self._uploader_queue_not_full:
            self._uploader_queue_not_full.notify_all()

    def _put_uploader_message(self, message: UploaderMessage | None) -> None:
        if message is None:
            self._part_uploader_queue.put(None)

        with self._uploader_queue_not_full:
            while not self._cancel.is_set() and self._part_uploader_queue.qsize() >= self._uploader_queue_max_size:
                self._uploader_queue_not_full.wait()
            if not self._cancel.is_set():
                self._part_uploader_queue.put(message)

    def _uploader_queue_task_done(self) -> None:
        self._part_uploader_queue.task_done()
        with self._uploader_queue_not_full:
            self._uploader_queue_not_full.notify_all()

    def _put_object(self, remote_object: S3Path, content: bytes) -> str:
        with self._cancel_upload_on_error():
            result = self._minio_client._put_object(
                bucket_name=remote_object.bucket,
                object_name=remote_object.key,
                data=content,
                headers={"Content-Type": "application/octet-stream"},
            )
            if result.etag is None:
                raise RuntimeError("PutObject response did not contain an ETag")
            return result.etag

    def _create_multipart_upload(self, remote_object: S3Path) -> str:
        with self._cancel_upload_on_error():
            return self._minio_client._create_multipart_upload(
                bucket_name=remote_object.bucket,
                object_name=remote_object.key,
                headers={"Content-Type": "application/octet-stream"},
            )

    def _upload_part(self, remote_object: S3Path, content: bytes, part_info: PartInfo) -> str:
        with self._cancel_upload_on_error():
            return self._minio_client._upload_part(
                bucket_name=remote_object.bucket,
                object_name=remote_object.key,
                data=content,
                headers=None,
                upload_id=part_info.upload_id,
                part_number=part_info.part_number,
            )

    def _complete_multipart_upload(self, remote_object: S3Path, upload_id: str, part_list: list[Part]) -> None:
        with self._cancel_upload_on_error():
            self._minio_client._complete_multipart_upload(
                bucket_name=remote_object.bucket,
                object_name=remote_object.key,
                upload_id=upload_id,
                parts=part_list,
            )

    def _abort_multipart_upload(self, remote_object: S3Path, upload_id: str) -> None:
        with self._cancel_upload_on_error():
            self._minio_client._abort_multipart_upload(
                bucket_name=remote_object.bucket,
                object_name=remote_object.key,
                upload_id=upload_id,
            )

    def _remove_object(self, remote_object: S3Path) -> None:
        with self._cancel_upload_on_error():
            self._minio_client.remove_object(
                bucket_name=remote_object.bucket,
                object_name=remote_object.key,
            )

    @contextlib.contextmanager
    def _cancel_upload_on_error(self) -> Iterator[None]:
        try:
            yield
        except MinioException:
            self._logger.exception("Abort: Minio operation failed")
            self._cancel_upload()
            raise
