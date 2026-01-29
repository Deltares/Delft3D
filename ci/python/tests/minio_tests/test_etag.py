from pathlib import Path

import pytest
from pyfakefs.fake_filesystem import FakeFilesystem

from ci_tools.minio.etag import ComputedEtag, ConstEtag


class TestComputedEtag:
    @pytest.mark.parametrize(
        ("content", "part_size", "expected_etag"),
        [
            pytest.param(b"", 2**20, "d41d8cd98f00b204e9800998ecf8427e", id="empty"),
            pytest.param(b"Hello world!", 2**20, "86fb269d190d2c85f6e0468ceca42a20", id="single_part"),
            pytest.param(b"Hello world!", 8, "4858bbb64dc56c387a466dd4e20efc29-2", id="multiple_parts"),
            pytest.param(b"12345678", 16, "25d55ad283aa400af464c76d713c07ad", id="truncated_part"),
            pytest.param(b"12345678", 8, "e42584918d922300a0498dbb6e89745d-1", id="exactly_one_part"),
        ],
    )
    def test_get_etag(self, content: bytes, part_size: int, expected_etag: str, fs: FakeFilesystem) -> None:
        # Arrange
        path = Path("test_file")
        fs.create_file(path, contents=content)
        computed_etag = ComputedEtag(path, part_size=part_size)

        # Act
        assert computed_etag.get_etag() == expected_etag, "ETag does not match expected value"

    def test_get_etag__etag_is_cached_and_not_recomputed(self, fs: FakeFilesystem) -> None:
        # Arrange
        path = Path("test_file")
        fs.create_file(path, contents=b"Hello world!")
        computed_etag = ComputedEtag(path)

        # Act
        etag1 = computed_etag.get_etag()
        with path.open("a") as writable:
            writable.write("\nI added some stuff!")

        etag2 = computed_etag.get_etag()

        # Assert
        assert etag1 == etag2, "ETag should be cached"


class TestConstEtag:
    def test_get_etag(self) -> None:
        etag = "86fb269d190d2c85f6e0468ceca42a20"
        const_etag = ConstEtag(etag)
        assert const_etag.get_etag() == etag, "ETag does not match expected value"
