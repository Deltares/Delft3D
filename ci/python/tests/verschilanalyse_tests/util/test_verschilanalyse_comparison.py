import logging
from pathlib import Path

import pytest
from pyfakefs.fake_filesystem import FakeFilesystem
from pytest_mock import MockerFixture

from ci_tools.verschilanalyse.util.verschilanalyse_comparison import VerschilanalyseComparison
from ci_tools.verschilanalyse.util.verschillentool import OutputType
from tests.helpers import verschilanalyse as helper


def test_get_log_comparisons() -> None:
    verschilanalyse = helper.make_verschilanalyse_comparison(
        current_log_data={name: helper.make_log_data() for name in ("foo", "bar")},
        reference_log_data={name: helper.make_log_data() for name in ("foo", "baz")},
    )

    log_comparisons = verschilanalyse.get_log_comparisons()

    assert "baz" not in log_comparisons
    assert log_comparisons["foo"].reference is not None
    assert log_comparisons["bar"].reference is None


def test_from_report_directories__no_logs_no_excel_files__no_problem(
    caplog: pytest.LogCaptureFixture, fs: FakeFilesystem
) -> None:
    # Act
    with caplog.at_level(logging.WARNING):
        verschilanalyse = VerschilanalyseComparison.from_report_directories(
            current_log_dir=Path("current_logs"),
            reference_log_dir=Path("reference_logs"),
            verschillen_dir=Path("verschillen"),
            s3_current_prefix="s3://bucket/latest/",
            s3_reference_prefix="s3://bucket/last-week//",
        )

    # Assert
    assert len(caplog.records) == 4
    assert all(record.levelname == "WARNING" for record in caplog.records)
    assert "current" in caplog.records[0].message
    assert "reference" in caplog.records[1].message
    assert "his file" in caplog.records[2].message
    assert "map file" in caplog.records[3].message
    assert verschilanalyse == VerschilanalyseComparison(
        s3_current_prefix="s3://bucket/latest",
        s3_reference_prefix="s3://bucket/last-week",
        current_log_data={},
        reference_log_data={},
        his_outputs={},
        map_outputs={},
    )


@pytest.mark.parametrize(
    ("model_log_dir", "is_current"),
    [
        pytest.param(Path("current_logs/models"), True, id="current"),
        pytest.param(Path("reference_logs/models"), False, id="reference"),
    ],
)
def test_from_report_directories__gather_slurm_log_data_from_directories(
    model_log_dir: Path, is_current: bool, fs: FakeFilesystem
) -> None:
    # Arrange
    for model_name in ("foo", "bar", "baz"):
        fs.create_file(
            model_log_dir / f"{model_name}.out",
            contents="** INFO : total computation time (s) : 1.0",
        )

    # Act
    verschilanalyse = VerschilanalyseComparison.from_report_directories(
        current_log_dir=Path("current_logs"),
        reference_log_dir=Path("reference_logs"),
        verschillen_dir=Path("verschillen"),
        s3_current_prefix="s3://bucket/latest",
        s3_reference_prefix="s3://bucket/last-week",
    )

    # Assert
    if is_current:
        assert not verschilanalyse.reference_log_data
        log_data = verschilanalyse.current_log_data
    else:
        assert not verschilanalyse.current_log_data
        log_data = verschilanalyse.reference_log_data

    assert sorted(log_data.keys()) == ["bar", "baz", "foo"]
    assert all(log_data.mean_computation_time == 1.0 for log_data in log_data.values())


@pytest.mark.parametrize("output_type", OutputType)
def test_from_report_directories__verschillentool_output(
    output_type: OutputType, fs: FakeFilesystem, mocker: MockerFixture
) -> None:
    # Arrange
    verschillen_dir = Path("verschillen")
    for model_name in ("foo", "bar", "baz"):
        fs.create_file(
            verschillen_dir / f"verschil_{model_name}/{output_type.value}_output.xlsx",
            contents="",
        )

    load_workbook_mock = mocker.patch("openpyxl.load_workbook")
    load_workbook_mock.return_value = helper.make_verschillentool_workbook(output_type=output_type)

    # Act
    verschilanalyse = VerschilanalyseComparison.from_report_directories(
        current_log_dir=Path("current_logs"),
        reference_log_dir=Path("reference_logs"),
        verschillen_dir=verschillen_dir,
        s3_current_prefix="s3://bucket/latest",
        s3_reference_prefix="s3://bucket/last-week",
    )

    # Assert
    assert load_workbook_mock.call_count == 3
    if output_type == OutputType.HIS:
        assert not verschilanalyse.map_outputs
        assert sorted(verschilanalyse.his_outputs.keys()) == ["bar", "baz", "foo"]
    else:
        assert sorted(verschilanalyse.map_outputs.keys()) == ["bar", "baz", "foo"]
        assert not verschilanalyse.his_outputs
