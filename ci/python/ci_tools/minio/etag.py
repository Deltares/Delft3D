import abc
import hashlib
import itertools
from pathlib import Path
from typing import BinaryIO, Iterator

from typing_extensions import override

from ci_tools.minio import DEFAULT_MULTIPART_UPLOAD_PART_SIZE


class ETag(abc.ABC):
    """Abstract base class for ETag strategies."""

    @abc.abstractmethod
    def get_etag(self) -> str:
        """Get the ETag of an object."""


class ConstEtag(ETag):
    """ETag strategy that returns a constant ETag.

    MinIO's `list_objects` API returns the ETag of each object.
    This class is used to wrap this ETag.
    """

    def __init__(self, etag: str) -> None:
        self._etag = etag

    @override
    def get_etag(self) -> str:
        return self._etag


class ComputedEtag(ETag):
    """ETag strategy that computes the ETag of a file.

    For local files, the `ETag` has to be computed by reading the entire contents of the file
    and calculating a hash. This is a costly operation, so it is only done when absolutely
    necessary. The `ETag` is cached after it is computed, so that it is only computed once.
    """

    def __init__(self, path: Path, part_size: int = DEFAULT_MULTIPART_UPLOAD_PART_SIZE) -> None:
        self._path = path
        self._part_size = part_size
        self._cached_etag: str | None = None

    @override
    def get_etag(self) -> str:
        if self._cached_etag is None:
            with self._path.open("rb") as file:
                self._cached_etag = self._etag(file)
        return self._cached_etag

    def _etag(self, readable: BinaryIO) -> str:
        """Compute the `ETag` of the contents of a stream of bytes.

        For files smaller than the multipart upload size, the ETag is the same as
        the MD5 hexdigest. For files larger than the multipart upload size, the MD5
        digest of each part is computed separately. Then, the digests of each part
        are concatenated and the MD5 digest of the result is computed. Finally, the
        number of parts is appended to the ETag.
        An interesting edge case occurs when a file has a size exactly matching the `part_size`.
        This case is treated as a multipart upload of a single part, instead of treating
        it like an upload in one go. The `ETag` of such a file ends in `-1`
        See: https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums

        Parameters
        ----------
        readable : BinaryIO
            A readable stream of bytes, such as a file opened for reading in binary mode.

        Returns
        -------
        str
            The ETag of the contents of the stream, as a string.

        Examples
        --------
        >>> from io import BytesIO
        >>> etag(BytesIO())  # Empty file.
        'd41d8cd98f00b204e9800998ecf8427e'
        >>> etag(BytesIO(b"Hello world!"))  # Single part.
        '86fb269d190d2c85f6e0468ceca42a20'
        >>> etag(BytesIO(b"Hello world!"), part_size=8)  # Multiple parts.
        '4858bbb64dc56c387a466dd4e20efc29-2'
        >>> etag(BytesIO(b"12345678"), part_size=16)  # One part.
        '25d55ad283aa400af464c76d713c07ad'
        >>> etag(BytesIO(b"12345678"), part_size=8)  # 'Exactly one part' edge case.
        'e42584918d922300a0498dbb6e89745d-1'
        """
        part_size = self._part_size

        def gen_chunks() -> Iterator[bytes]:
            chunk = readable.read(part_size)
            while chunk:
                yield chunk
                chunk = readable.read(part_size)

        chunks = gen_chunks()

        first_chunk = next(chunks, None)
        if first_chunk is None:
            return hashlib.md5(b"").hexdigest()  # Empty file

        if len(first_chunk) < part_size:
            return hashlib.md5(first_chunk).hexdigest()  # Single part

        chunks = itertools.chain(itertools.repeat(first_chunk, 1), chunks)
        digests = list(hashlib.md5(chunk).digest() for chunk in chunks)
        return hashlib.md5(b"".join(digests)).hexdigest() + f"-{len(digests)}"
