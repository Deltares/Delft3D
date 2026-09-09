import pytest
from openpyxl.styles import PatternFill

from ci_tools.verschilanalyse.util.excel_exporter import ExcelExporter
from ci_tools.verschilanalyse.util.verschillentool import OutputType, Variable
from tests.helpers import verschilanalyse as helper


def test_make_summary_workbook__check_sheetnames() -> None:
    verschilanalyse_comparison = helper.make_verschilanalyse_comparison()

    workbook = ExcelExporter.make_summary_workbook(verschilanalyse_comparison)

    assert workbook.sheetnames == [
        ExcelExporter.LOG_COMPARISON_SHEET_NAME,
        ExcelExporter.VERSCHILLENTOOL_SHEET_NAMES[OutputType.HIS],
        ExcelExporter.VERSCHILLENTOOL_SHEET_NAMES[OutputType.MAP],
    ]


def test_make_summary_workbook__log_comparisons() -> None:
    # Arrange
    current_log_data = {"foo": helper.make_log_data(), "bar": helper.make_log_data()}
    reference_log_data = {"foo": helper.make_log_data(), "baz": helper.make_log_data()}
    verschilanalyse_comparison = helper.make_verschilanalyse_comparison(
        s3_current_prefix="s3://bucket/output/current",
        s3_reference_prefix="s3://bucket/output/reference",
        current_log_data=current_log_data,
        reference_log_data=reference_log_data,
    )

    # Act
    workbook = ExcelExporter.make_summary_workbook(verschilanalyse_comparison)
    log_comp_sheet = workbook[ExcelExporter.LOG_COMPARISON_SHEET_NAME]

    # Assert output and report paths are set correctly.
    assert [cell[0].value for cell in log_comp_sheet["B1:B4"]] == [
        "s3://bucket/output/current/output",
        "s3://bucket/output/current/logs/logs.zip",
        "s3://bucket/output/reference/output",
        "s3://bucket/output/reference/logs/logs.zip",
    ]

    # bar comparison is first (sorted by name).
    bar_header = log_comp_sheet["A6:C6"][0]
    assert bar_header[0].value == "⚠️ bar"  # bar is in current but not in reference.
    assert bar_header[1].value == "✅ Current"
    assert bar_header[2].value == "⚠️ Reference"

    # foo comparison is second.
    foo_header = log_comp_sheet["A17:C17"][0]
    assert foo_header[0].value == "✅ foo"  #  foo is in both current and reference.
    assert foo_header[1].value == "✅ Current"
    assert foo_header[2].value == "✅ Reference"


@pytest.mark.parametrize("output_type", OutputType)
def test_make_summary_workbook(output_type: OutputType) -> None:
    # Arrange
    red_fill = PatternFill(start_color="FF0000", end_color="FF0000", fill_type="solid")
    ok_2d_output = helper.make_verschillentool_output_2d(  # All stats within tolerance.
        output_type=output_type,
        water_level=helper.tolerance_stats(output_type, Variable.WATER_LEVEL, diff=-1e-6),
        flow_velocity=helper.tolerance_stats(output_type, Variable.FLOW_VELOCITY, diff=-1e-6),
        row_count=42,
    )
    fail_2d_output = helper.make_verschillentool_output_2d(  # All stats over tolerance.
        output_type=output_type,
        water_level=helper.tolerance_stats(output_type, Variable.WATER_LEVEL, diff=10),
        flow_velocity=helper.tolerance_stats(output_type, Variable.FLOW_VELOCITY, diff=10),
        row_count=43,
    )
    ok_3d_output = helper.make_verschillentool_output_3d(  # All stats within tolerance.
        output_type=output_type,
        water_level=helper.tolerance_stats(output_type, Variable.WATER_LEVEL, diff=-1e-6),
        flow_velocity=helper.tolerance_stats(output_type, Variable.FLOW_VELOCITY, diff=-1e-6),
        salinity=helper.tolerance_stats(output_type, Variable.SALINITY, diff=-1e-6),
        temperature=helper.tolerance_stats(output_type, Variable.TEMPERATURE, diff=-1e-6),
        row_count=42,
    )
    fail_3d_output = helper.make_verschillentool_output_3d(  # All stats over tolerance.
        output_type=output_type,
        water_level=helper.tolerance_stats(output_type, Variable.WATER_LEVEL, diff=10),
        flow_velocity=helper.tolerance_stats(output_type, Variable.FLOW_VELOCITY, diff=10),
        salinity=helper.tolerance_stats(output_type, Variable.SALINITY, diff=10),
        temperature=helper.tolerance_stats(output_type, Variable.TEMPERATURE, diff=10),
        row_count=43,
    )

    outputs = {
        "fail_2d": fail_2d_output,
        "ok_2d": ok_2d_output,
        "fail_3d": fail_3d_output,
        "ok_3d": ok_3d_output
        }
    count_header = ExcelExporter.VERSCHILLENTOOL_COUNT_HEADERS[output_type]
    sheet_title = ExcelExporter.VERSCHILLENTOOL_SHEET_NAMES[output_type]
    if output_type == OutputType.HIS:
        verschilanalyse_comparison = helper.make_verschilanalyse_comparison(his_outputs=outputs)
    else:  # output_type == OutputType.MAP
        verschilanalyse_comparison = helper.make_verschilanalyse_comparison(map_outputs=outputs)

    # Act
    workbook = ExcelExporter.make_summary_workbook(verschilanalyse_comparison)
    sheet = workbook[sheet_title]
    header_row, fail_2d_row, fail_3d_row,ok_2d_row, ok_3d_row = sheet["A1:R5"]

    # Assert
    assert header_row[1].value == count_header

    assert fail_2d_row[0].value == "fail_2d"
    assert fail_2d_row[1].value == 43
    assert all(cell.fill == red_fill and str(cell.value).startswith("❌") for cell in fail_2d_row[2:9])
    assert all(str(cell.value) == "N/A" for cell in fail_2d_row[10:])

    assert ok_2d_row[0].value == "ok_2d"
    assert ok_2d_row[1].value == 42
    assert all(cell.fill != red_fill and not str(cell.value).startswith("❌") for cell in ok_2d_row[2:9])
    assert all(str(cell.value) == "N/A" for cell in ok_2d_row[10:])

    assert fail_3d_row[0].value == "fail_3d"
    assert fail_3d_row[1].value == 43
    assert all(cell.fill == red_fill and str(cell.value).startswith("❌") for cell in fail_3d_row[2:])
    assert not any(str(cell.value) == "N/A" for cell in fail_3d_row[2:])

    assert ok_3d_row[0].value == "ok_3d"
    assert ok_3d_row[1].value == 42
    assert all(cell.fill != red_fill and not str(cell.value).startswith("❌") for cell in ok_3d_row[2:])
    assert not any(str(cell.value) == "N/A" for cell in ok_3d_row[2:])

