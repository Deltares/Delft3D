import pytest

from ci_tools.verschilanalyse.util.verschillentool import (
    OutputType,
    Statistics,
    VerschillentoolOutput2D,
    VerschillentoolOutput3D,
)
from tests.helpers import verschilanalyse as helper


def test_from_verschillentool_workbook_3d() -> None:
    flow_velocity_stats = Statistics(1.0, 2.0, 3.0, 0.0)
    water_level_stats = Statistics(4.0, 5.0, 6.0, 0.0)
    salinity_stats = Statistics(1.0, 2.0, 3.0, 0.0)
    temperature_stats = Statistics(4.0, 5.0, 6.0, 0.0)
    workbook = helper.make_verschillentool_workbook3D(
        flow_velocity_stats=flow_velocity_stats,
        water_level_stats=water_level_stats,
        salinity_stats=salinity_stats,
        temperature_stats=temperature_stats,
        row_count=5,
        output_type=OutputType.MAP,
    )

    result = VerschillentoolOutput3D.from_verschillentool_workbook(workbook, OutputType.MAP)

    assert result == VerschillentoolOutput3D(
        output_type=OutputType.MAP,
        flow_velocity=flow_velocity_stats,
        water_level=water_level_stats,
        salinity=salinity_stats,
        temperature=temperature_stats,
        row_count=5,
    )


def test_from_verschillentool_workbook_2d() -> None:
    flow_velocity_stats = Statistics(1.0, 2.0, 3.0, 0.0)
    water_level_stats = Statistics(4.0, 5.0, 6.0, 0.0)
    workbook = helper.make_verschillentool_workbook2D(
        flow_velocity_stats=flow_velocity_stats,
        water_level_stats=water_level_stats,
        row_count=5,
        output_type=OutputType.MAP,
    )

    result = VerschillentoolOutput2D.from_verschillentool_workbook(workbook, OutputType.MAP)

    assert result == VerschillentoolOutput2D(
        output_type=OutputType.MAP,
        flow_velocity=flow_velocity_stats,
        water_level=water_level_stats,
        row_count=5,
    )


def test_from_verschillentool_workbook__stat_not_found__raise_value_error() -> None:
    workbook = helper.make_verschillentool_workbook2D()
    workbook["Averages"]["A2"].value = ">:("
    with pytest.raises(ValueError, match="Missing key"):
        VerschillentoolOutput2D.from_verschillentool_workbook(workbook, OutputType.HIS)
