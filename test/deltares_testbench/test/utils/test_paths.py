from src.utils.paths import Paths


def test_merge_full_path_skips_none_segments() -> None:
    paths = Paths()

    result = paths.mergeFullPath(None, "suite", "case")

    assert result == "suite/case"


def test_merge_path_elements_handles_none_left() -> None:
    paths = Paths()

    result = paths.mergePathElements(None, "suite")

    assert result == "suite"
