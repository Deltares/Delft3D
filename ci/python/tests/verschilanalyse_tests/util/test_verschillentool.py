import pytest

from ci_tools.verschilanalyse.util.verschillentool import OutputType, Statistics, VerschillentoolOutput
from tests.helpers import verschilanalyse as helper


def test_from_verschillentool_workbook() -> None:
    flow_velocity_stats = Statistics(1.0, 2.0, 3.0, 0.0)
    water_level_stats = Statistics(4.0, 5.0, 6.0, 0.0)
    workbook = helper.make_verschillentool_workbook(
        flow_velocity_stats=flow_velocity_stats,
        water_level_stats=water_level_stats,
        row_count=5,
        output_type=OutputType.MAP,
    )

    result = VerschillentoolOutput.from_verschillentool_workbook(workbook, OutputType.MAP)

    assert result == VerschillentoolOutput(
        output_type=OutputType.MAP,
        flow_velocity=flow_velocity_stats,
        water_level=water_level_stats,
        row_count=5,
    )


def test_from_verschillentool_workbook__stat_not_found__raise_value_error() -> None:
    workbook = helper.make_verschillentool_workbook()
    workbook["Averages"]["A2"].value = ">:("
    with pytest.raises(ValueError, match="Missing key"):
        VerschillentoolOutput.from_verschillentool_workbook(workbook, OutputType.HIS)
