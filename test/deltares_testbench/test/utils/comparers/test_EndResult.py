import pytest

from src.utils.comparers.end_result import EndResult


def test_end_result_enum_values() -> None:
    assert EndResult.ERROR.value == "ERROR"
    assert EndResult.NOK.value == "NOK"
    assert EndResult.OK.value == "OK"


def test_end_result_comparison() -> None:
    assert EndResult.ERROR < EndResult.NOK
    assert EndResult.NOK < EndResult.OK
    assert not (EndResult.OK < EndResult.NOK)
    assert not (EndResult.NOK < EndResult.ERROR)


def test_end_result_from_string() -> None:
    assert EndResult.from_string("ERROR") == EndResult.ERROR
    assert EndResult.from_string("NOK") == EndResult.NOK
    assert EndResult.from_string("OK") == EndResult.OK

    with pytest.raises(ValueError):
        EndResult.from_string("UNKNOWN")
